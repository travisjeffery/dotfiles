#!/usr/bin/env node

'use strict';

const childProcess = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const os = require('os');
const path = require('path');

const PROVENANCE_VERSION = 1;
const LOCK_STALE_MS = 30 * 60 * 1000;
const MAX_SEEDS_PER_VERSION = 3;
const CODEGRAPH_DIR = '.codegraph';
const DATABASE_FILE = 'codegraph.db';
const CONFIG_FILE = 'config.json';
const PROVENANCE_FILE = 'worktree-bootstrap.json';
const ALLOW_FULL_INDEX_ENV = 'CODEGRAPH_BOOTSTRAP_ALLOW_FULL_INDEX';

function execFile(command, args, options = {}) {
    return childProcess.execFileSync(command, args, {
        encoding: 'utf8',
        maxBuffer: 64 * 1024 * 1024,
        stdio: ['ignore', 'pipe', 'pipe'],
        ...options,
    });
}

function git(cwd, args, options = {}) {
    return execFile('git', ['-C', cwd, ...args], options);
}

function sha256(value) {
    return crypto.createHash('sha256').update(value).digest('hex');
}

function realpathOrResolve(value) {
    try {
        return fs.realpathSync(value);
    } catch {
        return path.resolve(value);
    }
}

function stateRoot() {
    return path.resolve(
        process.env.CODEGRAPH_BOOTSTRAP_STATE_DIR
            || path.join(os.homedir(), '.codex', 'codegraph-worktree-bootstrap'),
    );
}

function log(message, details) {
    const root = stateRoot();
    fs.mkdirSync(root, { recursive: true });
    const suffix = details === undefined ? '' : ` ${JSON.stringify(details)}`;
    fs.appendFileSync(
        path.join(root, 'bootstrap.log'),
        `${new Date().toISOString()} ${message}${suffix}\n`,
        'utf8',
    );
}

function emitContext(message) {
    if (!message) {
        return;
    }
    process.stdout.write(`${JSON.stringify({
        hookSpecificOutput: {
            hookEventName: 'SessionStart',
            additionalContext: message,
        },
    })}\n`);
}

function readHookInput() {
    const input = fs.readFileSync(0, 'utf8').trim();
    if (!input) {
        return {};
    }
    return JSON.parse(input);
}

function resolveCodeGraph() {
    const shell = process.env.SHELL && fs.existsSync(process.env.SHELL) ? process.env.SHELL : "/bin/bash";
    const binary = execFile(shell, ["-lc", "command -v codegraph"]).trim();
    if (!binary) {
        return null;
    }
    const realBinary = fs.realpathSync(binary);
    const packageRoot = path.dirname(path.dirname(path.dirname(realBinary)));
    const packageJson = JSON.parse(fs.readFileSync(path.join(packageRoot, 'package.json'), 'utf8'));
    const api = require(packageRoot);
    if (!api.CodeGraph) {
        throw new Error(`CodeGraph API is unavailable from ${packageRoot}`);
    }
    if (api.setLogger && api.silentLogger) {
        api.setLogger(api.silentLogger);
    }
    return {
        api,
        binary,
        packageRoot,
        version: packageJson.version,
    };
}

function repositoryContext(cwd) {
    const root = git(cwd, ['rev-parse', '--show-toplevel']).trim();
    const commonDirValue = git(root, ['rev-parse', '--git-common-dir']).trim();
    const commonDir = realpathOrResolve(
        path.isAbsolute(commonDirValue)
            ? commonDirValue
            : path.join(root, commonDirValue),
    );
    const head = git(root, ['rev-parse', 'HEAD']).trim();
    const canonicalRoot = realpathOrResolve(path.dirname(commonDir));
    const repoId = sha256(commonDir).slice(0, 20);
    return {
        root: realpathOrResolve(root),
        commonDir,
        canonicalRoot,
        head,
        repoId,
    };
}

function targetPaths(root) {
    const directory = path.join(root, CODEGRAPH_DIR);
    return {
        directory,
        database: path.join(directory, DATABASE_FILE),
        config: path.join(directory, CONFIG_FILE),
        provenance: path.join(directory, PROVENANCE_FILE),
    };
}

function seedVersionRoot(repository, codegraphVersion) {
    return path.join(stateRoot(), 'seeds', repository.repoId, codegraphVersion);
}

function readJson(file) {
    try {
        return JSON.parse(fs.readFileSync(file, 'utf8'));
    } catch {
        return null;
    }
}

function writeJsonAtomic(file, value) {
    fs.mkdirSync(path.dirname(file), { recursive: true });
    const temporary = `${file}.tmp-${process.pid}-${crypto.randomBytes(4).toString('hex')}`;
    fs.writeFileSync(temporary, `${JSON.stringify(value, null, 2)}\n`, 'utf8');
    fs.renameSync(temporary, file);
}

function processIsAlive(pid) {
    if (!Number.isInteger(pid) || pid <= 0) {
        return false;
    }
    try {
        process.kill(pid, 0);
        return true;
    } catch (error) {
        return error.code === 'EPERM';
    }
}

function acquireLock(name) {
    const locksRoot = path.join(stateRoot(), 'locks');
    fs.mkdirSync(locksRoot, { recursive: true });
    const lock = path.join(locksRoot, `${name}.lock`);
    try {
        fs.mkdirSync(lock);
    } catch (error) {
        if (error.code !== 'EEXIST') {
            throw error;
        }
        const owner = readJson(path.join(lock, 'owner.json'));
        if (owner && processIsAlive(owner.pid)) {
            return null;
        }
        const stats = fs.statSync(lock);
        if (!owner && Date.now() - stats.mtimeMs <= LOCK_STALE_MS) {
            return null;
        }
        fs.rmSync(lock, { recursive: true, force: true });
        fs.mkdirSync(lock);
    }
    writeJsonAtomic(path.join(lock, 'owner.json'), {
        pid: process.pid,
        startedAt: new Date().toISOString(),
    });
    return {
        release() {
            fs.rmSync(lock, { recursive: true, force: true });
        },
    };
}

function fullIndexAllowed() {
    return process.env[ALLOW_FULL_INDEX_ENV] === '1';
}

function workingTreeStatus(repository) {
    return git(repository.root, [
        'status',
        '--porcelain=v1',
        '--untracked-files=all',
        '--',
        '.',
        `:(exclude)${CODEGRAPH_DIR}`,
        `:(exclude)${CODEGRAPH_DIR}/**`,
    ]).trim();
}

function worktreeIsClean(repository) {
    return workingTreeStatus(repository) === '';
}

function hasSubmoduleDelta(repository, fromCommit, toCommit) {
    const raw = git(repository.root, [
        'diff',
        '--raw',
        '--no-renames',
        fromCommit,
        toCommit,
        '--',
    ]);
    return raw.split('\n').some((line) => /^:(?:160000|\d{6}) (?:160000|\d{6}) /.test(line)
        && line.includes('160000'));
}

function commitDelta(repository, fromCommit, toCommit) {
    if (fromCommit === toCommit) {
        return { deleted: [], changed: [] };
    }
    if (hasSubmoduleDelta(repository, fromCommit, toCommit)) {
        return null;
    }
    const output = git(repository.root, [
        '-c',
        'core.quotepath=false',
        'diff',
        '--name-status',
        '-z',
        '--no-renames',
        fromCommit,
        toCommit,
        '--',
    ], { encoding: 'buffer' });
    const fields = output.toString('utf8').split('\0');
    const deleted = [];
    const changed = [];
    for (let index = 0; index + 1 < fields.length; index += 2) {
        const status = fields[index];
        const file = fields[index + 1];
        if (!status || !file) {
            continue;
        }
        if (status.startsWith('D')) {
            deleted.push(file);
        } else {
            changed.push(file);
        }
    }
    return { deleted, changed };
}

function listSeeds(repository, codegraphVersion) {
    const root = seedVersionRoot(repository, codegraphVersion);
    if (!fs.existsSync(root)) {
        return [];
    }
    return fs.readdirSync(root, { withFileTypes: true })
        .filter((entry) => entry.isDirectory() && !entry.name.startsWith('.tmp-'))
        .map((entry) => {
            const directory = path.join(root, entry.name);
            const metadata = readJson(path.join(directory, 'metadata.json'));
            if (!metadata
                || metadata.provenanceVersion !== PROVENANCE_VERSION
                || metadata.repoId !== repository.repoId
                || metadata.commonDir !== repository.commonDir
                || metadata.codegraphVersion !== codegraphVersion
                || !metadata.commit
                || !fs.existsSync(path.join(directory, DATABASE_FILE))
                || !fs.existsSync(path.join(directory, '.complete'))) {
                return null;
            }
            try {
                git(repository.root, ['cat-file', '-e', `${metadata.commit}^{commit}`]);
            } catch {
                return null;
            }
            return { directory, metadata };
        })
        .filter(Boolean);
}

function seedDistance(repository, seedCommit, targetCommit) {
    try {
        const output = git(repository.root, [
            'diff',
            '--name-only',
            '-z',
            '--no-renames',
            seedCommit,
            targetCommit,
            '--',
        ], { encoding: 'buffer' });
        if (output.length === 0) {
            return 0;
        }
        return output.toString('utf8').split('\0').filter(Boolean).length;
    } catch {
        return Number.MAX_SAFE_INTEGER;
    }
}

function chooseSeed(repository, codegraphVersion) {
    return listSeeds(repository, codegraphVersion)
        .map((seed) => ({
            ...seed,
            distance: seedDistance(repository, seed.metadata.commit, repository.head),
        }))
        .sort((left, right) => left.distance - right.distance
            || right.metadata.createdAt.localeCompare(left.metadata.createdAt))[0] || null;
}

function ensureGitIgnore(directory) {
    const file = path.join(directory, '.gitignore');
    const marker = '# Managed by CodeGraph worktree bootstrap';
    if (fs.existsSync(file)) {
        const current = fs.readFileSync(file, 'utf8');
        if (!current.includes(marker)) {
            fs.appendFileSync(file, `\n${marker}\n*\n`, 'utf8');
        }
        return;
    }
    fs.writeFileSync(file, [
        '# CodeGraph data files',
        '# These are local to each machine and should not be committed',
        '',
        '*.db',
        '*.db-wal',
        '*.db-shm',
        '*.log',
        '.dirty',
        'worktree-bootstrap.json',
        '',
        marker,
        '*',
        '',
    ].join('\n'), 'utf8');
}

function cloneFile(source, destination) {
    try {
        fs.copyFileSync(
            source,
            destination,
            fs.constants.COPYFILE_FICLONE_FORCE,
        );
        return 'reflink';
    } catch (error) {
        fs.rmSync(destination, { force: true });
        const unsupported = new Set([
            'ENOSYS',
            'ENOTSUP',
            'EOPNOTSUPP',
            'EXDEV',
            'EINVAL',
        ]);
        if (!unsupported.has(error.code)) {
            throw error;
        }
    }
    fs.copyFileSync(source, destination);
    return 'copy';
}

function removeDatabaseSidecars(database) {
    for (const suffix of ['-journal', '-shm', '-wal']) {
        fs.rmSync(`${database}${suffix}`, { force: true });
    }
}

function installSeed(repository, seed) {
    const target = targetPaths(repository.root);
    fs.mkdirSync(target.directory, { recursive: true });
    ensureGitIgnore(target.directory);
    const codegraphLock = path.join(target.directory, 'codegraph.lock');
    const lockPid = Number.parseInt(
        fs.existsSync(codegraphLock)
            ? fs.readFileSync(codegraphLock, 'utf8').trim()
            : '',
        10,
    );
    if (processIsAlive(lockPid)) {
        throw new Error(`CodeGraph database is in use by PID ${lockPid}`);
    }
    const temporaryDatabase = `${target.database}.tmp-${process.pid}`;
    let cloneMode;
    try {
        cloneMode = cloneFile(
            path.join(seed.directory, DATABASE_FILE),
            temporaryDatabase,
        );
        removeDatabaseSidecars(target.database);
        fs.rmSync(codegraphLock, { force: true });
        fs.renameSync(temporaryDatabase, target.database);
        fs.copyFileSync(path.join(seed.directory, CONFIG_FILE), target.config);
        fs.rmSync(target.provenance, { force: true });
    } finally {
        fs.rmSync(temporaryDatabase, { force: true });
    }
    log('installed seed', {
        root: repository.root,
        seedCommit: seed.metadata.commit,
        targetCommit: repository.head,
        distance: seed.distance,
        cloneMode,
    });
    return seed.metadata.commit;
}

async function fullReindex(codegraph, repository, reason) {
    if (!fullIndexAllowed()) {
        log('deferred full reindex', { root: repository.root, reason });
        return { mode: 'deferred', reason };
    }
    log('starting full reindex', { root: repository.root, reason });
    codegraph.queries.clear();
    const result = await codegraph.indexAll();
    if (!result.success) {
        throw new Error(`CodeGraph full indexing failed: ${JSON.stringify(result.errors.slice(0, 5))}`);
    }
    log('completed full reindex', {
        root: repository.root,
        filesIndexed: result.filesIndexed,
        filesSkipped: result.filesSkipped,
        nodesCreated: result.nodesCreated,
        durationMs: result.durationMs,
    });
    return { mode: 'full', reason, result };
}

async function loadLanguagesForFiles(codegraphApi, files) {
    const languages = [...new Set(files
        .map((file) => codegraphApi.detectLanguage(file))
        .filter((language) => codegraphApi.isLanguageSupported(language)))];
    if (languages.includes('c') && !languages.includes('cpp')) {
        languages.push('cpp');
    }
    await codegraphApi.loadGrammarsForLanguages(languages);
}

async function applyCommitDelta(
    codegraph,
    repository,
    fromCommit,
    codegraphApi,
) {
    const delta = commitDelta(repository, fromCommit, repository.head);
    if (!delta) {
        return fullReindex(
            codegraph,
            repository,
            'submodule commit changed',
        );
    }
    for (const deleted of delta.deleted) {
        codegraph.queries.deleteFile(deleted);
    }
    const changed = delta.changed.filter((file) => {
        try {
            return fs.statSync(path.join(repository.root, file)).isFile();
        } catch {
            return false;
        }
    });
    if (changed.length > 0) {
        await loadLanguagesForFiles(codegraphApi, changed);
    }
    const result = changed.length > 0
        ? await codegraph.indexFiles(changed)
        : {
            success: true,
            filesIndexed: 0,
            filesSkipped: 0,
            filesErrored: 0,
            nodesCreated: 0,
            edgesCreated: 0,
            errors: [],
            durationMs: 0,
        };
    if (!result.success) {
        throw new Error(`CodeGraph delta indexing failed: ${JSON.stringify(result.errors.slice(0, 5))}`);
    }
    if (changed.length > 0 && typeof codegraph.resolveReferencesBatched === 'function') {
        await codegraph.resolveReferencesBatched();
    }
    log('applied commit delta', {
        root: repository.root,
        fromCommit,
        toCommit: repository.head,
        deleted: delta.deleted.length,
        changed: changed.length,
        durationMs: result.durationMs,
    });
    return {
        mode: 'delta',
        fromCommit,
        deleted: delta.deleted.length,
        changed: changed.length,
        result,
    };
}

async function reconcileIndexedFiles(codegraph, repository, codegraphApi) {
    const changed = [];
    const deleted = [];
    const metadataOnly = [];
    for (const record of codegraph.queries.getAllFiles()) {
        const fullPath = path.join(repository.root, record.path);
        let stats;
        try {
            stats = fs.statSync(fullPath);
        } catch {
            deleted.push(record.path);
            continue;
        }
        if (!stats.isFile()) {
            deleted.push(record.path);
            continue;
        }
        if (record.size === stats.size
            && Math.abs(record.modifiedAt - stats.mtimeMs) < 0.5) {
            continue;
        }
        const content = fs.readFileSync(fullPath, 'utf8');
        const contentHash = sha256(content);
        if (contentHash !== record.contentHash) {
            changed.push(record.path);
        } else {
            metadataOnly.push({
                path: record.path,
                size: stats.size,
                modifiedAt: stats.mtimeMs,
            });
        }
    }

    for (const file of deleted) {
        codegraph.queries.deleteFile(file);
    }
    if (metadataOnly.length > 0) {
        const database = codegraph.db.getDb();
        const update = database.prepare(
            'UPDATE files SET size = ?, modified_at = ? WHERE path = ?',
        );
        database.transaction((records) => {
            for (const record of records) {
                update.run(record.size, record.modifiedAt, record.path);
            }
        })(metadataOnly);
    }
    if (changed.length > 0) {
        await loadLanguagesForFiles(codegraphApi, changed);
        const result = await codegraph.indexFiles(changed);
        if (!result.success) {
            throw new Error(`CodeGraph stale-file repair failed: ${JSON.stringify(result.errors.slice(0, 5))}`);
        }
        if (typeof codegraph.resolveReferencesBatched === 'function') {
            await codegraph.resolveReferencesBatched();
        }
    }
    if (changed.length > 0 || deleted.length > 0) {
        log('reconciled stale indexed files', {
            root: repository.root,
            changed: changed.length,
            deleted: deleted.length,
            metadataOnly: metadataOnly.length,
        });
    }
    return {
        changed: changed.length,
        deleted: deleted.length,
        metadataOnly: metadataOnly.length,
    };
}

function validProvenance(value, repository, codegraphVersion) {
    return value
        && value.provenanceVersion === PROVENANCE_VERSION
        && value.repoId === repository.repoId
        && value.commonDir === repository.commonDir
        && value.codegraphVersion === codegraphVersion
        && typeof value.indexedCommit === 'string';
}

async function reconcile(repository, codegraphRuntime) {
    const target = targetPaths(repository.root);
    let isInitialized = codegraphRuntime.api.CodeGraph.isInitialized(repository.root);
    const provenance = readJson(target.provenance);
    let seededFrom = null;
    if (!isInitialized || !validProvenance(
        provenance,
        repository,
        codegraphRuntime.version,
    )) {
        const seed = chooseSeed(repository, codegraphRuntime.version);
        if (seed) {
            seededFrom = installSeed(repository, seed);
            isInitialized = true;
        }
    }

    let codegraph;
    if (codegraphRuntime.api.CodeGraph.isInitialized(repository.root)) {
        codegraph = await codegraphRuntime.api.CodeGraph.open(repository.root);
    } else {
        codegraph = await codegraphRuntime.api.CodeGraph.init(repository.root, { index: false });
    }
    ensureGitIgnore(target.directory);

    let reconciliation;
    try {
        if (seededFrom) {
            reconciliation = await applyCommitDelta(
                codegraph,
                repository,
                seededFrom,
                codegraphRuntime.api,
            );
        } else if (validProvenance(provenance, repository, codegraphRuntime.version)) {
            reconciliation = await applyCommitDelta(
                codegraph,
                repository,
                provenance.indexedCommit,
                codegraphRuntime.api,
            );
        } else {
            reconciliation = await fullReindex(
                codegraph,
                repository,
                isInitialized ? 'missing or incompatible provenance' : 'no reusable seed',
            );
        }

        if (reconciliation.mode === 'deferred') {
            return {
                reconciliation,
                sync: null,
                drift: null,
                clean: false,
            };
        }

        const sync = await codegraph.sync();
        const drift = await reconcileIndexedFiles(
            codegraph,
            repository,
            codegraphRuntime.api,
        );
        const clean = worktreeIsClean(repository);
        writeJsonAtomic(target.provenance, {
            provenanceVersion: PROVENANCE_VERSION,
            repoId: repository.repoId,
            commonDir: repository.commonDir,
            indexedCommit: repository.head,
            codegraphVersion: codegraphRuntime.version,
            clean,
            reconciledAt: new Date().toISOString(),
        });
        return {
            reconciliation,
            sync,
            drift,
            clean,
        };
    } finally {
        codegraph.close();
    }
}

function sqliteBackup(source, destination) {
    const escaped = destination.replaceAll("'", "''");
    execFile('/usr/bin/sqlite3', [source, `.backup '${escaped}'`]);
}

function shouldPublishSeed(repository, codegraphVersion) {
    if (!worktreeIsClean(repository)) {
        return false;
    }
    const seeds = listSeeds(repository, codegraphVersion);
    if (seeds.some((seed) => seed.metadata.commit === repository.head)) {
        return false;
    }
    return seeds.length === 0 || repository.root === repository.canonicalRoot;
}

function publishSeed(repository, codegraphVersion) {
    if (!shouldPublishSeed(repository, codegraphVersion)) {
        return false;
    }
    const lock = acquireLock(`seed-${repository.repoId}-${codegraphVersion}`);
    if (!lock) {
        return false;
    }
    try {
        if (!shouldPublishSeed(repository, codegraphVersion)) {
            return false;
        }
        const target = targetPaths(repository.root);
        const versionRoot = seedVersionRoot(repository, codegraphVersion);
        fs.mkdirSync(versionRoot, { recursive: true });
        const finalDirectory = path.join(versionRoot, repository.head);
        const temporaryDirectory = path.join(
            versionRoot,
            `.tmp-${repository.head}-${process.pid}-${crypto.randomBytes(4).toString('hex')}`,
        );
        fs.mkdirSync(temporaryDirectory);
        try {
            sqliteBackup(target.database, path.join(temporaryDirectory, DATABASE_FILE));
            fs.copyFileSync(target.config, path.join(temporaryDirectory, CONFIG_FILE));
            writeJsonAtomic(path.join(temporaryDirectory, 'metadata.json'), {
                provenanceVersion: PROVENANCE_VERSION,
                repoId: repository.repoId,
                commonDir: repository.commonDir,
                commit: repository.head,
                codegraphVersion,
                createdAt: new Date().toISOString(),
            });
            fs.writeFileSync(path.join(temporaryDirectory, '.complete'), '\n', 'utf8');
            fs.renameSync(temporaryDirectory, finalDirectory);
            const staleSeeds = listSeeds(repository, codegraphVersion)
                .sort((left, right) => right.metadata.createdAt.localeCompare(
                    left.metadata.createdAt,
                ))
                .slice(MAX_SEEDS_PER_VERSION);
            for (const staleSeed of staleSeeds) {
                fs.rmSync(staleSeed.directory, { recursive: true, force: true });
            }
            log('published seed', {
                root: repository.root,
                commit: repository.head,
                destination: finalDirectory,
            });
            return true;
        } catch (error) {
            fs.rmSync(temporaryDirectory, { recursive: true, force: true });
            throw error;
        }
    } finally {
        lock.release();
    }
}

function summary(result, published) {
    const reconciliation = result.reconciliation;
    let detail;
    if (reconciliation.mode === 'deferred') {
        const suffix = reconciliation.detail
            ? `; ${reconciliation.detail}`
            : '';
        return `CodeGraph is not ready: full indexing was deferred (${reconciliation.reason}${suffix}). Normal source inspection remains available.`;
    } else if (reconciliation.mode === 'delta') {
        detail = `seeded/reconciled ${reconciliation.changed} changed and ${reconciliation.deleted} deleted files`;
    } else {
        detail = `completed a full index (${reconciliation.reason})`;
    }
    const localChanges = result.sync.filesAdded
        + result.sync.filesModified
        + result.sync.filesRemoved;
    if (localChanges > 0) {
        detail += `, then synchronized ${localChanges} local changes`;
    }
    const staleFiles = result.drift.changed + result.drift.deleted;
    if (staleFiles > 0) {
        detail += ` and reconciled ${staleFiles} stale indexed ${staleFiles === 1 ? 'file' : 'files'}`;
    }
    if (published) {
        detail += ' and published an immutable seed';
    }
    return `CodeGraph is ready: ${detail}.`;
}

async function run() {
    const startedAt = Date.now();
    const input = readHookInput();
    const cwd = input.cwd || process.cwd();
    let runtime;
    try {
        runtime = resolveCodeGraph();
    } catch (error) {
        log('could not load CodeGraph', { error: error.message });
        return;
    }
    if (!runtime) {
        return;
    }

    let repository;
    try {
        repository = repositoryContext(cwd);
    } catch {
        return;
    }

    const maintenanceLock = fullIndexAllowed()
        ? acquireLock(`full-index-${repository.repoId}-${runtime.version}`)
        : null;
    if (fullIndexAllowed() && !maintenanceLock) {
        emitContext('CodeGraph seed maintenance is already running for this repository.');
        return;
    }

    const targetLockName = `target-${repository.repoId}-${sha256(repository.root).slice(0, 16)}`;
    const lock = acquireLock(targetLockName);
    if (!lock) {
        if (maintenanceLock) {
            maintenanceLock.release();
        }
        emitContext('CodeGraph bootstrap is already running for this worktree.');
        return;
    }

    try {
        const result = await reconcile(repository, runtime);
        const published = result.reconciliation.mode === 'deferred'
            ? false
            : publishSeed(repository, runtime.version);
        log('bootstrap complete', {
            root: repository.root,
            commit: repository.head,
            durationMs: Date.now() - startedAt,
        });
        emitContext(summary(result, published));
    } catch (error) {
        log('bootstrap failed', {
            root: repository.root,
            commit: repository.head,
            error: error.stack || error.message,
            durationMs: Date.now() - startedAt,
        });
        emitContext(`CodeGraph bootstrap failed open: ${error.message}. Normal source inspection remains available.`);
    } finally {
        lock.release();
        if (maintenanceLock) {
            maintenanceLock.release();
        }
    }
}

if (require.main === module) {
    run().catch((error) => {
        try {
            log('unhandled bootstrap failure', { error: error.stack || error.message });
        } catch {
            // The hook must never prevent a Codex session from starting.
        }
        emitContext(`CodeGraph bootstrap failed open: ${error.message}. Normal source inspection remains available.`);
        process.exitCode = 0;
    });
}

module.exports = {
    applyCommitDelta,
    chooseSeed,
    cloneFile,
    commitDelta,
    fullIndexAllowed,
    processIsAlive,
    publishSeed,
    reconcile,
    repositoryContext,
    run,
    worktreeIsClean,
};
