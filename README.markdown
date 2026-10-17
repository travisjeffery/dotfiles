Hey, these are [@travisjeffery's](http://twitter.com/travisjeffery) dotfiles.

I use Emacs and Arch Linux.

I've published this in the hopes that someone finds them useful.

Installation with [GNU Stow](https://www.gnu.org/software/stow/):

``` sh
git clone https://github.com/travisjeffery/dotfiles
cd dotfiles
mise trust .config/mise/config.toml
stow -v -t ~ .
mise install
```

## Agent browser access

Agents get two browsers:

- **Playwright, separate profile.** `@playwright/mcp` with `--user-data-dir ~/.local/share/muse/playwright-profile`. Sign in once with `npx --yes --package @playwright/cli@0.1.22 playwright-cli open --browser chrome --persistent --profile ~/.local/share/muse/playwright-profile --headed <url>`, then `playwright-cli close` before an agent uses it (Chrome locks the profile).
- **Your real Chrome, opt-in.** `chrome-devtools-mcp --autoConnect` attaches to your everyday Chrome profile, but only while you have remote debugging turned on:

  ``` sh
  chrome-agent-debug on       # opens chrome://inspect/#remote-debugging; enable it there
  chrome-agent-debug status   # shows whether it is on and that it listens on localhost only
  chrome-agent-debug off      # disable it there, or quit Chrome completely
  ```

  Chrome 136+ ignores `--remote-debugging-port` on the default profile, so there is no launcher flag. Chrome's own toggle binds to 127.0.0.1 and asks you to Allow each new connection.

  **Risk while it is on:** any local process can drive the browser through the port, and the agent can act on every site you are signed in to. Deny connection prompts you did not start, and turn it off when you are done.

Muse MCP entries (`~/.config/muse/settings.json`, `mcpServers`; use absolute paths):

``` json
"playwright": {"command": "npx", "args": ["-y", "@playwright/mcp@0.0.83", "--browser", "chrome",
  "--user-data-dir", "/home/<you>/.local/share/muse/playwright-profile",
  "--output-dir", "/home/<you>/.cache/muse/playwright-output"]},
"chrome-devtools": {"command": "npx", "args": ["-y", "chrome-devtools-mcp@1.10.1", "--autoConnect",
  "--channel", "stable", "--no-usage-statistics", "--no-performance-crux", "--redactNetworkHeaders"]}
```
