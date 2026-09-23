'use strict';

const assert = require('node:assert/strict');
const childProcess = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const test = require('node:test');

const hook = path.join(__dirname, 'codegraph-worktree-bootstrap.js');

function exec(command, args, options = {}) {
    return childProcess.execFileSync(command, args, {
        encoding: 'utf8',
        maxBuffer: 32 * 1024 * 1024,
        stdio: ['ignore', 'pipe', 'pipe'],
        ...options,
    });
}

function git(cwd, ...args) {
    return exec('git', ['-C', cwd, ...args]);
}

function write(file, content) {
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, content, 'utf8');
}

function runHook(cwd, stateDirectory, { allowFullIndex = true } = {}) {
    const result = childProcess.spawnSync('node', [hook], {
        encoding: 'utf8',
        env: {
            ...process.env,
            CODEGRAPH_BOOTSTRAP_STATE_DIR: stateDirectory,
            CODEGRAPH_BOOTSTRAP_TIMEOUT_MS: '120000',
            ...(allowFullIndex
                ? { CODEGRAPH_BOOTSTRAP_ALLOW_FULL_INDEX: '1' }
                : {}),
        },
        input: JSON.stringify({
            hook_event_name: 'SessionStart',
            cwd,
            source: 'startup',
        }),
        maxBuffer: 32 * 1024 * 1024,
    });
    assert.equal(result.status, 0, result.stderr);
    return JSON.parse(result.stdout);
}

function symbolCount(root, name) {
    return Number(exec('/usr/bin/sqlite3', [
        path.join(root, '.codegraph', 'codegraph.db'),
        `SELECT COUNT(*) FROM nodes WHERE name = '${name.replaceAll("'", "''")}';`,
    ]).trim());
}

test('seeds a divergent worktree and synchronizes local changes', { timeout: 120000 }, () => {
    const temporaryRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'codegraph-bootstrap-test-'));
    try {
        const repository = path.join(temporaryRoot, 'repository');
        const exactWorktree = path.join(temporaryRoot, 'exact-worktree');
        const worktree = path.join(temporaryRoot, 'worktree');
        const stateDirectory = path.join(temporaryRoot, 'state');
        fs.mkdirSync(repository);
        git(repository, 'init', '-b', 'main');
        git(repository, 'config', 'user.name', 'CodeGraph Bootstrap Test');
        git(repository, 'config', 'user.email', 'codegraph-bootstrap@example.invalid');

        write(
            path.join(repository, 'service.js'),
            'export function alphaService() { return "alpha"; }\n',
        );
        write(
            path.join(repository, 'obsolete.js'),
            'export function obsoleteService() { return "obsolete"; }\n',
        );
        git(repository, 'add', 'service.js', 'obsolete.js');
        git(repository, 'commit', '-m', 'Add alpha service');

        const initial = runHook(repository, stateDirectory);
        assert.match(
            initial.hookSpecificOutput.additionalContext,
            /completed a full index/,
        );
        assert.equal(symbolCount(repository, 'alphaService'), 1);
        assert.equal(symbolCount(repository, 'obsoleteService'), 1);

        git(repository, 'worktree', 'add', '-b', 'exact', exactWorktree);
        const exact = runHook(exactWorktree, stateDirectory);
        assert.match(
            exact.hookSpecificOutput.additionalContext,
            /seeded\/reconciled 0 changed and 0 deleted/,
        );
        assert.equal(symbolCount(exactWorktree, 'alphaService'), 1);

        git(repository, 'worktree', 'add', '-b', 'feature', worktree);
        write(
            path.join(worktree, 'service.js'),
            'export function betaService() { return "beta"; }\n',
        );
        git(worktree, 'rm', 'obsolete.js');
        git(worktree, 'add', 'service.js');
        git(worktree, 'commit', '-m', 'Replace alpha with beta');

        const seeded = runHook(worktree, stateDirectory);
        assert.match(
            seeded.hookSpecificOutput.additionalContext,
            /seeded\/reconciled 1 changed/,
        );
        assert.match(
            seeded.hookSpecificOutput.additionalContext,
            /1 deleted/,
        );
        assert.equal(symbolCount(worktree, 'alphaService'), 0);
        assert.equal(symbolCount(worktree, 'betaService'), 1);
        assert.equal(symbolCount(worktree, 'obsoleteService'), 0);

        const log = fs.readFileSync(path.join(stateDirectory, 'bootstrap.log'), 'utf8');
        assert.match(log, /installed seed .*"cloneMode":"(?:reflink|copy)"/);

        write(
            path.join(worktree, 'service.js'),
            'export function gammaService() { return "gamma"; }\n',
        );
        const synchronized = runHook(worktree, stateDirectory);
        assert.match(
            synchronized.hookSpecificOutput.additionalContext,
            /synchronized 1 local changes/,
        );
        assert.equal(symbolCount(worktree, 'betaService'), 0);
        assert.equal(symbolCount(worktree, 'gammaService'), 1);

        const provenance = JSON.parse(fs.readFileSync(
            path.join(worktree, '.codegraph', 'worktree-bootstrap.json'),
            'utf8',
        ));
        assert.equal(provenance.indexedCommit, git(worktree, 'rev-parse', 'HEAD').trim());
        assert.equal(provenance.clean, false);

        write(
            path.join(worktree, 'service.js'),
            'export function betaService() { return "beta"; }\n',
        );
        const reverted = runHook(worktree, stateDirectory);
        assert.match(
            reverted.hookSpecificOutput.additionalContext,
            /reconciled 1 stale indexed file/,
        );
        assert.equal(symbolCount(worktree, 'gammaService'), 0);
        assert.equal(symbolCount(worktree, 'betaService'), 1);

        fs.unlinkSync(path.join(worktree, '.codegraph', 'worktree-bootstrap.json'));
        const repaired = runHook(worktree, stateDirectory);
        assert.match(
            repaired.hookSpecificOutput.additionalContext,
            /seeded\/reconciled 1 changed and 1 deleted/,
        );
        assert.equal(symbolCount(worktree, 'betaService'), 1);
    } finally {
        fs.rmSync(temporaryRoot, { recursive: true, force: true });
    }
});

test('defers a full index unless maintenance mode is explicit', { timeout: 120000 }, () => {
    const temporaryRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'codegraph-bootstrap-defer-'));
    try {
        const repository = path.join(temporaryRoot, 'repository');
        const stateDirectory = path.join(temporaryRoot, 'state');
        fs.mkdirSync(repository);
        git(repository, 'init', '-b', 'main');
        git(repository, 'config', 'user.name', 'CodeGraph Bootstrap Test');
        git(repository, 'config', 'user.email', 'codegraph-bootstrap@example.invalid');
        write(
            path.join(repository, 'service.js'),
            'export function deferredService() { return "deferred"; }\n',
        );
        git(repository, 'add', 'service.js');
        git(repository, 'commit', '-m', 'Add deferred service');

        const result = runHook(repository, stateDirectory, { allowFullIndex: false });
        assert.match(
            result.hookSpecificOutput.additionalContext,
            /full indexing was deferred/,
        );
        assert.equal(
            fs.existsSync(path.join(repository, '.codegraph', 'worktree-bootstrap.json')),
            false,
        );
        assert.equal(fs.existsSync(path.join(stateDirectory, 'seeds')), false);
    } finally {
        fs.rmSync(temporaryRoot, { recursive: true, force: true });
    }
});

test('falls back to a full index when a submodule commit changes', { timeout: 120000 }, () => {
    const temporaryRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'codegraph-bootstrap-submodule-'));
    try {
        const dependency = path.join(temporaryRoot, 'dependency');
        const repository = path.join(temporaryRoot, 'repository');
        const worktree = path.join(temporaryRoot, 'worktree');
        const stateDirectory = path.join(temporaryRoot, 'state');

        fs.mkdirSync(dependency);
        git(dependency, 'init', '-b', 'main');
        git(dependency, 'config', 'user.name', 'CodeGraph Bootstrap Test');
        git(dependency, 'config', 'user.email', 'codegraph-bootstrap@example.invalid');
        write(
            path.join(dependency, 'dependency.js'),
            'export function dependencyAlpha() { return "alpha"; }\n',
        );
        git(dependency, 'add', 'dependency.js');
        git(dependency, 'commit', '-m', 'Add alpha dependency');

        fs.mkdirSync(repository);
        git(repository, 'init', '-b', 'main');
        git(repository, 'config', 'user.name', 'CodeGraph Bootstrap Test');
        git(repository, 'config', 'user.email', 'codegraph-bootstrap@example.invalid');
        git(
            repository,
            '-c',
            'protocol.file.allow=always',
            'submodule',
            'add',
            dependency,
            'deps/dependency',
        );
        git(repository, 'commit', '-am', 'Add dependency submodule');
        runHook(repository, stateDirectory);
        assert.equal(symbolCount(repository, 'dependencyAlpha'), 1);

        git(repository, 'worktree', 'add', '-b', 'feature', worktree);
        git(
            worktree,
            '-c',
            'protocol.file.allow=always',
            'submodule',
            'update',
            '--init',
        );
        write(
            path.join(dependency, 'dependency.js'),
            'export function dependencyBeta() { return "beta"; }\n',
        );
        git(dependency, 'add', 'dependency.js');
        git(dependency, 'commit', '-m', 'Replace alpha dependency with beta');
        git(path.join(worktree, 'deps/dependency'), 'fetch', 'origin');
        git(
            path.join(worktree, 'deps/dependency'),
            'checkout',
            git(dependency, 'rev-parse', 'HEAD').trim(),
        );
        git(worktree, 'add', 'deps/dependency');
        git(worktree, 'commit', '-m', 'Update dependency submodule');

        const result = runHook(worktree, stateDirectory);
        assert.match(
            result.hookSpecificOutput.additionalContext,
            /completed a full index \(submodule commit changed\)/,
        );
        assert.equal(symbolCount(worktree, 'dependencyAlpha'), 0);
        assert.equal(symbolCount(worktree, 'dependencyBeta'), 1);
    } finally {
        fs.rmSync(temporaryRoot, { recursive: true, force: true });
    }
});
