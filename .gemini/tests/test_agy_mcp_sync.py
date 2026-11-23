"""Unit tests for bin/agy-mcp-sync.

Run: python3 -m unittest discover -s .gemini/tests -v
"""

import json
import os
import stat
import subprocess
import sys
import tempfile
import unittest

REPO_ROOT = os.path.normpath(
    os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")
)
SYNC_BIN = os.path.join(REPO_ROOT, "bin", "agy-mcp-sync")


def run_sync(*args):
    return subprocess.run(
        [sys.executable, SYNC_BIN, *args],
        capture_output=True,
        text=True,
        timeout=20,
    )


class TestAgyMcpSync(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory(prefix="agy-mcp-sync-test-")
        self.root = self._tmp.name
        self.source = os.path.join(self.root, "mcp.json")
        self.target = os.path.join(self.root, "config", "mcp_config.json")
        os.makedirs(os.path.dirname(self.target), exist_ok=True)

    def tearDown(self):
        self._tmp.cleanup()

    def write_json(self, path, obj, mode=None):
        with open(path, "w", encoding="utf-8") as f:
            json.dump(obj, f, indent=2)
        if mode is not None:
            os.chmod(path, mode)

    def read_json(self, path):
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)

    def test_translates_stdio_and_http_from_zero_byte_target_with_0600_and_redaction(self):
        # Simulate fresh agy 0-byte mcp_config.json with overly broad mode.
        with open(self.target, "wb"):
            pass
        os.chmod(self.target, 0o644)

        secret_header = "Bearer synthetic-secret-header-token-9999"
        secret_env = "synthetic-secret-env-token-8888"
        private_url = "https://private-mcp.example.invalid/v1/mcp"

        self.write_json(self.source, {
            "mcpServers": {
                "local-stdio": {
                    "type": "stdio",
                    "command": "npx",
                    "args": ["-y", "@example/mcp-server"],
                    "env": {"API_TOKEN": secret_env},
                    "lifecycle": "eager",
                    "directTools": True,
                },
                "remote-oauth": {
                    "url": private_url,
                },
                "remote-headers": {
                    "url": "https://api.example.com/mcp",
                    "headers": {"Authorization": secret_header},
                    "timeoutSeconds": 120,
                },
                "legacy-sse": {
                    "type": "sse",
                    "url": "https://sse.example.com/sse",
                },
            }
        })

        r = run_sync("--source", self.source, "--target", self.target)
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn("updated", r.stdout)
        self.assertIn("legacy SSE", r.stderr)

        # Redaction: secrets and URLs must never appear in stdout or stderr.
        combined = r.stdout + "\n" + r.stderr
        self.assertNotIn(secret_header, combined)
        self.assertNotIn(secret_env, combined)
        self.assertNotIn(private_url, combined)
        self.assertNotIn("https://", combined)

        # Permissions strictly 0600 on both target and backup.
        self.assertEqual(stat.S_IMODE(os.stat(self.target).st_mode), 0o600)
        backups = [f for f in os.listdir(os.path.dirname(self.target)) if f.startswith("mcp_config.json.bak-")]
        self.assertEqual(len(backups), 1)
        backup_file = os.path.join(os.path.dirname(self.target), backups[0])
        self.assertEqual(stat.S_IMODE(os.stat(backup_file).st_mode), 0o600)

        out = self.read_json(self.target)
        servers = out["mcpServers"]
        self.assertEqual(sorted(servers.keys()), ["local-stdio", "remote-headers", "remote-oauth"])

        self.assertEqual(servers["local-stdio"], {
            "command": "npx",
            "args": ["-y", "@example/mcp-server"],
            "env": {"API_TOKEN": secret_env},
            "disabled": False,
        })
        self.assertEqual(servers["remote-oauth"], {
            "serverUrl": private_url,
            "disabled": False,
        })
        self.assertEqual(servers["remote-headers"], {
            "serverUrl": "https://api.example.com/mcp",
            "headers": {"Authorization": secret_header},
            "disabled": False,
            "timeoutSeconds": 120,
        })

        # Second run is idempotent: unchanged and creates no additional backup.
        r2 = run_sync("--source", self.source, "--target", self.target)
        self.assertEqual(r2.returncode, 0, r2.stderr)
        self.assertIn("unchanged", r2.stdout)
        backups2 = [f for f in os.listdir(os.path.dirname(self.target)) if f.startswith("mcp_config.json.bak-")]
        self.assertEqual(len(backups2), 1)

    def test_preserves_unmanaged_servers_top_level_keys_and_disabled_state(self):
        self.write_json(self.target, {
            "extraTopLevel": {"keep": True},
            "mcpServers": {
                "pi-bridge-12345": {
                    "disabled": False,
                    "headers": {"x-bridge-token": "ephemeral"},
                    "serverUrl": "http://127.0.0.1:41234/mcp",
                },
                "manual-server": {
                    "command": "/usr/local/bin/manual-mcp",
                    "disabled": False,
                },
                "synced-server": {
                    "serverUrl": "https://old.example.com/mcp",
                    "disabled": True,
                },
            },
        }, mode=0o600)

        self.write_json(self.source, {
            "mcpServers": {
                "synced-server": {
                    "url": "https://new.example.com/mcp",
                },
            }
        })

        r = run_sync("--source", self.source, "--target", self.target)
        self.assertEqual(r.returncode, 0, r.stderr)
        out = self.read_json(self.target)
        self.assertEqual(out["extraTopLevel"], {"keep": True})
        self.assertIn("pi-bridge-12345", out["mcpServers"])
        self.assertIn("manual-server", out["mcpServers"])
        # URL updated, existing user disabled=True preserved by default.
        self.assertEqual(out["mcpServers"]["synced-server"]["serverUrl"], "https://new.example.com/mcp")
        self.assertTrue(out["mcpServers"]["synced-server"]["disabled"])

        # --no-preserve-disabled resets disabled back to False.
        r2 = run_sync("--source", self.source, "--target", self.target, "--no-preserve-disabled")
        self.assertEqual(r2.returncode, 0, r2.stderr)
        out2 = self.read_json(self.target)
        self.assertFalse(out2["mcpServers"]["synced-server"]["disabled"])

    def test_exclude_server_and_tool_filtering(self):
        self.write_json(self.source, {
            "mcpServers": {
                "alpha": {
                    "url": "https://alpha.example.com/mcp",
                    "disabledTools": ["existing_bad_tool"],
                },
                "beta": {
                    "command": "/usr/bin/beta-mcp",
                },
                "skip-me": {
                    "url": "https://skip.example.com/mcp",
                },
            }
        })

        filter_file = os.path.join(self.root, "filters.json")
        self.write_json(filter_file, {
            "excludeServers": ["skip-me"],
            "disabledTools": {"alpha": ["drop_table", "existing_bad_tool"]},
            "enabledTools": {"beta": ["read_item"]},
        })

        r = run_sync(
            "--source", self.source,
            "--target", self.target,
            "--filter-file", filter_file,
            "--disable-tool", "alpha:delete_all",
            "--enable-tool", "beta:list_items",
        )
        self.assertEqual(r.returncode, 0, r.stderr)
        out = self.read_json(self.target)["mcpServers"]
        self.assertEqual(sorted(out.keys()), ["alpha", "beta"])
        self.assertEqual(out["alpha"]["disabledTools"], ["delete_all", "drop_table", "existing_bad_tool"])
        self.assertEqual(out["beta"]["enabledTools"], ["list_items", "read_item"])

    def test_dry_run_makes_no_writes(self):
        self.write_json(self.source, {
            "mcpServers": {"one": {"url": "https://one.example.com/mcp"}}
        })
        r = run_sync("--source", self.source, "--target", self.target, "--dry-run")
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn("would-update", r.stdout)
        self.assertFalse(os.path.exists(self.target))

    def test_corrupt_target_json_is_refused(self):
        self.write_json(self.source, {
            "mcpServers": {"one": {"url": "https://one.example.com/mcp"}}
        })
        with open(self.target, "w", encoding="utf-8") as f:
            f.write("{not-valid-json")

        r = run_sync("--source", self.source, "--target", self.target)
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("not valid JSON", r.stderr)
        with open(self.target, "r", encoding="utf-8") as f:
            self.assertEqual(f.read(), "{not-valid-json")

    def test_symlinked_target_is_preserved(self):
        real_target = os.path.join(self.root, "real_mcp_config.json")
        with open(real_target, "w", encoding="utf-8") as f:
            f.write("{}")
        os.symlink(real_target, self.target)

        self.write_json(self.source, {
            "mcpServers": {"one": {"url": "https://one.example.com/mcp"}}
        })

        r = run_sync("--source", self.source, "--target", self.target)
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertTrue(os.path.islink(self.target))
        self.assertEqual(stat.S_IMODE(os.stat(real_target).st_mode), 0o600)
        self.assertIn("one", self.read_json(real_target)["mcpServers"])


if __name__ == "__main__":
    unittest.main()
