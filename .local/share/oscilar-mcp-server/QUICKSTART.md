# Quick Start

## TL;DR

```bash
# 1. Install
cd oscilar-mcp-server
./install.sh

# 2. Add to Claude Code
claude mcp add oscilar-infra \
  -e KUBECONFIG=$HOME/.kube/config \
  -- node $(pwd)/index.js

# 3. Restart Claude Code

# 4. Use it
claude "check nginx logs for errors"
```

## What This Does

Before MCP:
```
You: "claude check nginx logs"
Claude: "I can't access your cluster. Can you run kubectl logs -n ingress-nginx...?"
You: [runs command, pastes output]
Claude: [analyzes]
```

After MCP:
```
You: "claude check nginx logs"
Claude: [automatically runs kubectl logs -n ingress-nginx, analyzes output, gives answer]
```

## Common Commands

```bash
# Infrastructure
claude "check nginx status"
claude "is Karpenter scaling nodes?"
claude "show me all ingress resources"

# Debugging
claude "why is pod xyz-123 failing?"
claude "debug pod myapp in production namespace"
claude "show me pods using the most memory"

# Metrics
claude "query VictoriaMetrics for high CPU pods"
claude "get nginx request rate from metrics"

# AWS
claude "check NLB target health"
claude "list EC2 instances in our cluster"
```

## Verify Installation

```bash
claude
> /mcp

# Should show:
# oscilar-infra: connected
```

## Troubleshooting

**Not connecting?**
- Check Node.js version: `node -v` (must be 18+)
- Check config path in `~/.config/Claude/claude_desktop_config.json`
- Restart Claude Code

**kubectl commands failing?**
- Test kubectl: `kubectl cluster-info`
- Check KUBECONFIG in MCP config

## Next Steps

Read [README.md](README.md) for:
- All available tools
- Detailed configuration
- Advanced usage examples
- Customization options
