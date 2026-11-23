"""Unit tests for Antigravity (agy) PreToolUse run-command-guard adapter.

Run: python3 -m unittest discover -s .gemini/config/hooks/tests -v
"""

import json
import os
import subprocess
import sys
import tempfile
import unittest

REPO_ROOT = os.path.normpath(
    os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "..", "..")
)
ADAPTER = os.path.join(REPO_ROOT, ".gemini", "config", "hooks", "run-command-guard.py")
CLAUDE_HOOKS = os.path.join(REPO_ROOT, ".claude", "hooks")


def make_agy_payload(command, cwd, workspace_paths=None, tool_name="run_command", args_cwd=None):
    args = {
        "CommandLine": command,
        "WaitMsBeforeAsync": 5000,
        "toolSummary": "Test command",
    }
    if args_cwd is not None:
        args["Cwd"] = args_cwd
    return {
        "hookEventName": "PreToolUse",
        "conversationId": "00000000-0000-0000-0000-000000000001",
        "stepIdx": 2,
        "modelName": "test-model",
        "cwd": cwd,
        "workspacePaths": workspace_paths if workspace_paths is not None else [cwd],
        "toolCall": {
            "id": "call_test_1",
            "name": tool_name,
            "args": args,
        },
    }


def run_adapter(payload, extra_env=None, proc_cwd=None):
    env = dict(os.environ)
    env["AGY_GUARD_HOOKS_DIR"] = CLAUDE_HOOKS
    if extra_env:
        env.update(extra_env)
    raw = payload if isinstance(payload, str) else json.dumps(payload)
    proc = subprocess.run(
        [sys.executable, ADAPTER],
        input=raw,
        capture_output=True,
        text=True,
        timeout=20,
        env=env,
        cwd=proc_cwd,
    )
    out = json.loads(proc.stdout) if proc.stdout.strip() else None
    return proc.returncode, out, proc.stderr


class TestRunCommandGuard(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory(prefix="agy-guard-test-")
        self.root = os.path.realpath(self._tmp.name)
        self.workspace = os.path.join(self.root, "workspace")
        self.tmpdir = os.path.join(self.root, "tmp")
        self.rm_log = os.path.join(self.root, "rm-guard.log")
        os.makedirs(self.workspace)
        os.makedirs(self.tmpdir)
        self.base_env = {
            "TMPDIR": self.tmpdir,
            "RM_GUARD_LOG": self.rm_log,
        }

    def tearDown(self):
        self._tmp.cleanup()

    def test_hard_blocks_return_deny(self):
        cases = [
            "rm -rf /etc/example",
            "rm -r /opt/example",
            "rm -rf ~/.ssh",
            "rm -rf ~/.aws/credentials",
            "rm -rf .git",
            "rm -rf /",
            "git push --force origin main",
            "git push -f origin main",
            "find /opt/example -delete",
            "shred /etc/hosts",
            # Block wins over ask in compound commands
            "kubectl delete pod web-0 && rm -rf ~/.ssh",
            "terraform destroy && git push --force origin main",
            # Block wins over scratch auto-approve
            'D=$(mktemp -d); rm -rf "$D" /opt/outside',
        ]
        for cmd in cases:
            with self.subTest(cmd=cmd):
                rc, out, _ = run_adapter(make_agy_payload(cmd, self.workspace), self.base_env)
                self.assertEqual(rc, 0)
                self.assertIsInstance(out, dict)
                self.assertEqual(out.get("decision"), "deny")
                self.assertTrue(out.get("reason"))

    def test_destructive_infra_and_unresolved_rm_return_force_ask(self):
        cases = [
            "kubectl delete pod web-0",
            "kubectl -n app delete deploy/api",
            "terraform destroy",
            "pulumi destroy --yes",
            "aws ec2 terminate-instances --instance-ids i-0123456789abcdef0",
            "aws s3 rm s3://example-bucket/key",
            "helm uninstall my-release",
            "git status && kubectl delete ns staging",
            "rm -rf $UNRESOLVED_TARGET_DIR",
        ]
        for cmd in cases:
            with self.subTest(cmd=cmd):
                rc, out, _ = run_adapter(make_agy_payload(cmd, self.workspace), self.base_env)
                self.assertEqual(rc, 0)
                self.assertEqual(out.get("decision"), "force_ask")
                self.assertTrue(out.get("reason"))

    def test_ask_decision_env_overrides(self):
        payload = make_agy_payload("kubectl delete pod web-0", self.workspace)

        rc, out, _ = run_adapter(payload, {**self.base_env, "AGY_GUARD_ASK_DECISION": "ask"})
        self.assertEqual(rc, 0)
        self.assertEqual(out.get("decision"), "ask")

        rc, out, _ = run_adapter(payload, {**self.base_env, "AGY_GUARD_ASK_DECISION": "deny"})
        self.assertEqual(rc, 0)
        self.assertEqual(out.get("decision"), "deny")
        self.assertIn("AGY_GUARD_ASK_DECISION=deny", out.get("reason", ""))

    def test_benign_commands_return_allow(self):
        cases = [
            "git status",
            "git push --force-with-lease origin feat/test",
            "kubectl get pods -A",
            "kubectl delete --dry-run=client -f deploy.yaml",
            "terraform plan",
            "pulumi destroy --preview-only",
            "helm uninstall my-release --dry-run",
            "echo 'rm -rf / && kubectl delete ns prod'",
            "cat <<'EOF'\nrm -rf /\nkubectl delete pod x\nEOF",
            "rm -rf build",
            "rm -f /tmp/single-file.txt",
        ]
        for cmd in cases:
            with self.subTest(cmd=cmd):
                rc, out, _ = run_adapter(make_agy_payload(cmd, self.workspace), self.base_env)
                self.assertEqual(rc, 0)
                self.assertEqual(out, {"decision": "allow"})

    def test_mktemp_and_scratch_deletes_return_auto_approve(self):
        cases = [
            'D=$(mktemp -d); rm -rf "$D"',
            'D=$(mktemp -d) && mkdir -p "$D/sub" && cd "$D" && rm -rf sub',
            'rm -rf "$TMPDIR/build-cache"',
        ]
        for cmd in cases:
            with self.subTest(cmd=cmd):
                rc, out, _ = run_adapter(make_agy_payload(cmd, self.workspace), self.base_env)
                self.assertEqual(rc, 0)
                self.assertEqual(out.get("decision"), "auto_approve")
                self.assertTrue(out.get("reason"))

    def test_worktree_delete_handling(self):
        # A git worktree inside TMPDIR (has a .git file, not a directory) must NOT auto-approve.
        wt = os.path.join(self.tmpdir, "wt")
        os.makedirs(wt)
        with open(os.path.join(wt, ".git"), "w", encoding="utf-8") as f:
            f.write("gitdir: /elsewhere\n")

        rc, out, _ = run_adapter(make_agy_payload('rm -rf "$TMPDIR/wt"', self.workspace), self.base_env)
        self.assertEqual(rc, 0)
        self.assertEqual(out, {"decision": "allow"})

        # Deleting a subdirectory inside the active worktree workspace is allowed.
        rc, out, _ = run_adapter(
            make_agy_payload("rm -rf dist", cwd=self.tmpdir, workspace_paths=[self.workspace], args_cwd=self.workspace),
            self.base_env,
        )
        self.assertEqual(rc, 0)
        self.assertEqual(out, {"decision": "allow"})

        # Deleting an unrelated sibling worktree outside allowed roots is denied.
        other_wt = "/data/worktrees/other-repo/other-branch"
        rc, out, _ = run_adapter(
            make_agy_payload(f"rm -rf {other_wt}", cwd=self.workspace, workspace_paths=[self.workspace]),
            self.base_env,
        )
        self.assertEqual(rc, 0)
        self.assertEqual(out.get("decision"), "deny")

    def test_args_cwd_overrides_payload_cwd(self):
        # Command runs in /opt (outside workspace), so relative `rm -rf foo` targets /opt/foo -> deny.
        payload = make_agy_payload("rm -rf foo", cwd=self.workspace, workspace_paths=[self.workspace], args_cwd="/opt")
        rc, out, _ = run_adapter(payload, self.base_env)
        self.assertEqual(rc, 0)
        self.assertEqual(out.get("decision"), "deny")

    def test_process_cwd_in_config_dir_does_not_false_block_relative_rm(self):
        # agy invokes hooks with process cwd set to hooks.json's directory, not the workspace.
        config_dir = os.path.join(self.root, "gemini-config")
        os.makedirs(config_dir)
        rc, out, _ = run_adapter(
            make_agy_payload("rm -rf build", self.workspace),
            self.base_env,
            proc_cwd=config_dir,
        )
        self.assertEqual(rc, 0)
        self.assertEqual(out, {"decision": "allow"})

        rc, out, _ = run_adapter(
            make_agy_payload('D=$(mktemp -d); rm -rf "$D"', self.workspace),
            self.base_env,
            proc_cwd=config_dir,
        )
        self.assertEqual(rc, 0)
        self.assertEqual(out.get("decision"), "auto_approve")

    def test_non_run_command_and_fail_open_paths(self):
        # Non-run_command tool must still emit explicit allow (agy denies empty `{}`).
        rc, out, _ = run_adapter(make_agy_payload("rm -rf /", self.workspace, tool_name="read_file"), self.base_env)
        self.assertEqual(rc, 0)
        self.assertEqual(out, {"decision": "allow"})

        # Invalid stdin JSON fails open with explicit allow.
        rc, out, _ = run_adapter("{not valid json", self.base_env)
        self.assertEqual(rc, 0)
        self.assertEqual(out, {"decision": "allow"})

        # Empty command fails open with explicit allow.
        rc, out, _ = run_adapter(make_agy_payload("", self.workspace), self.base_env)
        self.assertEqual(rc, 0)
        self.assertEqual(out, {"decision": "allow"})

        # Missing guard directory fails open with explicit allow.
        rc, out, _ = run_adapter(
            make_agy_payload("rm -rf /etc/example", self.workspace),
            {**self.base_env, "AGY_GUARD_HOOKS_DIR": os.path.join(self.root, "nonexistent")},
        )
        self.assertEqual(rc, 0)
        self.assertEqual(out, {"decision": "allow"})

    def test_install_preserves_existing_hooks_and_is_idempotent(self):
        hooks_file = os.path.join(self.root, "hooks.json")
        initial = {
            "herdr": {
                "PreInvocation": [{"type": "command", "command": "bash '/tmp/herdr.sh'", "timeout": 10}],
            },
            "moshi-hook": {
                "enabled": True,
                "PreInvocation": [{"type": "command", "command": "'/tmp/moshi' PreInvocation", "timeout": 5}],
                "Stop": [{"type": "command", "command": "'/tmp/moshi' Stop", "timeout": 5}],
            },
        }
        with open(hooks_file, "w", encoding="utf-8") as f:
            json.dump(initial, f, indent=2)

        # First install writes group and creates backup while preserving existing groups.
        r1 = subprocess.run(
            [sys.executable, ADAPTER, "--install", "--hooks-file", hooks_file, "--hook-command", "python3 '/opt/guard.py'"],
            capture_output=True,
            text=True,
            timeout=10,
        )
        self.assertEqual(r1.returncode, 0, r1.stderr)
        self.assertIn("updated", r1.stdout)
        with open(hooks_file, "r", encoding="utf-8") as f:
            after = json.load(f)
        self.assertEqual(after["herdr"], initial["herdr"])
        self.assertEqual(after["moshi-hook"], initial["moshi-hook"])
        self.assertIn("run-command-guard", after)
        self.assertEqual(after["run-command-guard"]["PreToolUse"][0]["matcher"], "run_command")
        backups = [p for p in os.listdir(self.root) if p.startswith("hooks.json.bak-")]
        self.assertEqual(len(backups), 1)

        # Second install is a no-op and does not create another backup.
        r2 = subprocess.run(
            [sys.executable, ADAPTER, "--install", "--hooks-file", hooks_file, "--hook-command", "python3 '/opt/guard.py'"],
            capture_output=True,
            text=True,
            timeout=10,
        )
        self.assertEqual(r2.returncode, 0, r2.stderr)
        self.assertIn("unchanged", r2.stdout)
        backups_after = [p for p in os.listdir(self.root) if p.startswith("hooks.json.bak-")]
        self.assertEqual(len(backups_after), 1)

        # Corrupt hooks.json is refused without clobbering.
        corrupt = os.path.join(self.root, "corrupt.json")
        with open(corrupt, "w", encoding="utf-8") as f:
            f.write("{broken")
        r3 = subprocess.run(
            [sys.executable, ADAPTER, "--install", "--hooks-file", corrupt],
            capture_output=True,
            text=True,
            timeout=10,
        )
        self.assertNotEqual(r3.returncode, 0)
        with open(corrupt, "r", encoding="utf-8") as f:
            self.assertEqual(f.read(), "{broken")


if __name__ == "__main__":
    unittest.main()
