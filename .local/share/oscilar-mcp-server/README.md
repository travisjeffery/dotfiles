# Oscilar MCP Server

A Model Context Protocol (MCP) server that gives Claude Code direct access to your Oscilar infrastructure without needing to explain context every time.

## Features

This MCP server provides tools for:

- **kubectl commands**: Execute any kubectl command
- **nginx ingress**: Get logs, status, and diagnostics
- **Karpenter**: Check node provisioning and status
- **Pod debugging**: Comprehensive pod information and logs
- **VictoriaMetrics**: Query metrics using PromQL
- **AWS CLI**: Check NLB health, EC2 instances, etc.
- **Resource monitoring**: Check CPU/memory usage across pods

## Installation

### 1. Install Dependencies

```bash
cd oscilar-mcp-server
npm install
```

### 2. Configure Claude Code

Add the server to Claude Code's configuration:

```bash
# Option 1: Using Claude Code CLI (easier)
claude mcp add oscilar-infra \
  -e KUBECONFIG=$HOME/.kube/config \
  -e DEFAULT_NAMESPACE=default \
  -e NGINX_NAMESPACE=ingress-nginx \
  -e KARPENTER_NAMESPACE=karpenter \
  -e MONITORING_NAMESPACE=monitoring \
  -- node /path/to/oscilar-mcp-server/index.js

# Option 2: Edit config directly
vim ~/.config/Claude/claude_desktop_config.json
```

If editing the config file directly, add:

```json
{
  "mcpServers": {
    "oscilar-infra": {
      "command": "node",
      "args": ["/home/you/oscilar-mcp-server/index.js"],
      "env": {
        "KUBECONFIG": "/home/you/.kube/config",
        "DEFAULT_NAMESPACE": "default",
        "NGINX_NAMESPACE": "ingress-nginx",
        "KARPENTER_NAMESPACE": "karpenter",
        "MONITORING_NAMESPACE": "monitoring",
        "VICTORIAMETRICS_SERVICE": "victoriametrics"
      }
    }
  }
}
```

### 3. Restart Claude Code

After adding the server, restart Claude Code for the changes to take effect.

### 4. Verify Installation

In Claude Code, check if the server is connected:

```bash
claude
> /mcp
```

You should see `oscilar-infra: connected` in the list.

## Available Tools

### kubectl
Execute any kubectl command.

**Example prompts:**
- "Get all pods in the default namespace"
- "Show me the logs for pod xyz-123"
- "Describe the service myapp in production namespace"

**Direct usage:**
```javascript
// Claude will call:
kubectl({
  command: "get pods -n default"
})
```

### nginx_logs
Get nginx ingress controller logs with filtering.

**Example prompts:**
- "Show me the last 200 nginx logs"
- "Get nginx logs and grep for errors"
- "Show me nginx logs containing '5xx'"

**Parameters:**
- `tail`: Number of lines (default: 100)
- `follow`: Follow logs in real-time (default: false)
- `grep`: Pattern to filter logs

### nginx_status
Get comprehensive nginx ingress status.

**Example prompts:**
- "Check nginx ingress controller status"
- "Is nginx healthy?"
- "Show me all ingress resources"

### karpenter_status
Get Karpenter provisioning status and node information.

**Example prompts:**
- "Check Karpenter status"
- "Are nodes being provisioned?"
- "Show me node distribution by instance type"

### pod_debug
Comprehensive pod debugging information.

**Example prompts:**
- "Debug pod myapp-xyz in production namespace"
- "Why is pod xyz-123 failing?"
- "Show me everything about this pod"

**Parameters:**
- `pod_name`: Pod name (required)
- `namespace`: Namespace (default: default)
- `include_logs`: Include logs (default: true)

### victoriametrics_query
Query VictoriaMetrics using PromQL.

**Example prompts:**
- "Query VictoriaMetrics for pod CPU usage"
- "Get nginx request rate from metrics"
- "Show me memory usage for namespace production"

**Parameters:**
- `query`: PromQL query (required)
- `time`: Optional time parameter

**Example queries:**
```promql
# CPU usage
sum(rate(container_cpu_usage_seconds_total{namespace="default"}[5m])) by (pod)

# Memory usage
container_memory_working_set_bytes{namespace="default"}

# Nginx request rate
sum(rate(nginx_ingress_controller_requests[5m]))
```

### aws_cli
Execute AWS CLI commands.

**Example prompts:**
- "Check NLB target health for this ARN"
- "List EC2 instances in our VPC"
- "Show me security groups"

**Direct usage:**
```javascript
// Claude will call:
aws_cli({
  command: "elbv2 describe-target-health --target-group-arn arn:..."
})
```

### get_cluster_context
Get current kubectl context and cluster info.

**Example prompts:**
- "What cluster am I connected to?"
- "Show me the current context"
- "Get cluster information"

### check_pod_resources
Check resource usage (CPU/Memory) for pods.

**Example prompts:**
- "Show me top CPU consuming pods"
- "Which pods are using the most memory?"
- "Check resource usage in production namespace"

**Parameters:**
- `namespace`: Namespace to check (optional, default: all)
- `sort_by`: Sort by "cpu" or "memory"

## Usage Examples

Once configured, you can ask Claude Code natural language questions and it will use the appropriate tools:

```bash
claude

> Why are nginx requests timing out?
# Claude will:
# 1. Call nginx_logs to check for errors
# 2. Call nginx_status to check pod health
# 3. Maybe call kubectl to check backend services

> Check if Karpenter is scaling properly
# Claude will:
# 1. Call karpenter_status
# 2. Check pending pods
# 3. Review node provisioning activity

> Debug why pod myapp-xyz123 in production is crashlooping
# Claude will:
# 1. Call pod_debug with pod_name and namespace
# 2. Analyze logs and events
# 3. Check resource limits

> Show me the top 10 pods by CPU usage
# Claude will:
# 1. Call check_pod_resources with sort_by="cpu"

> Query VictoriaMetrics for high memory pods
# Claude will:
# 1. Call victoriametrics_query with appropriate PromQL
```

## Environment Variables

Configure these in your MCP server config:

- `KUBECONFIG`: Path to kubeconfig file (required for kubectl)
- `DEFAULT_NAMESPACE`: Default namespace for operations (default: "default")
- `NGINX_NAMESPACE`: Namespace where nginx-ingress runs (default: "ingress-nginx")
- `KARPENTER_NAMESPACE`: Namespace where Karpenter runs (default: "karpenter")
- `MONITORING_NAMESPACE`: Namespace where VictoriaMetrics runs (default: "monitoring")
- `VICTORIAMETRICS_SERVICE`: VictoriaMetrics service name (default: "victoriametrics")

## Prerequisites

- Node.js 18+ installed
- `kubectl` configured and in PATH
- `aws` CLI configured (for AWS tools)
- Access to your EKS cluster via kubectl
- VictoriaMetrics accessible within cluster (for metrics queries)

## Troubleshooting

### Server not connecting

Check Claude Code MCP status:
```bash
claude
> /mcp
```

If not listed or showing as disconnected, check:
1. Node.js version: `node --version` (must be 18+)
2. Server path is correct in config
3. Environment variables are set correctly
4. Restart Claude Code after config changes

### kubectl commands failing

Ensure:
1. `KUBECONFIG` environment variable is set correctly
2. You have access to the cluster: `kubectl cluster-info`
3. Current context is correct: `kubectl config current-context`

### VictoriaMetrics queries failing

The VictoriaMetrics tool creates a temporary pod to query the service. Ensure:
1. VictoriaMetrics service exists: `kubectl get svc -n monitoring`
2. Service name matches `VICTORIAMETRICS_SERVICE` env var
3. Network policies allow pod-to-service communication

### AWS CLI commands failing

Ensure:
1. AWS CLI is installed: `aws --version`
2. AWS credentials are configured: `aws sts get-caller-identity`
3. You have appropriate IAM permissions

## Customization

You can modify `index.js` to add more tools or customize existing ones:

```javascript
// Add a new tool
server.setRequestHandler(ListToolsRequestSchema, async () => {
  return {
    tools: [
      // ... existing tools
      {
        name: "custom_tool",
        description: "Your custom tool",
        inputSchema: {
          type: "object",
          properties: {
            param: { type: "string" }
          }
        }
      }
    ]
  };
});

// Handle the tool
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  // ... existing handlers
  case "custom_tool": {
    // Your implementation
  }
});
```

## Security Notes

- The MCP server runs with your local user permissions
- It has the same kubectl and AWS access as your user
- Only use this on your local machine, not on shared systems
- Be cautious about exposing sensitive information in logs

## Benefits Over Manual Commands

**Before (without MCP):**
```bash
claude "check nginx logs"
# You: "kubectl logs -n ingress-nginx ..."
# Claude: analyzes output

claude "why is this pod failing?"
# You: "kubectl describe pod xyz -n prod"
# You: "kubectl logs xyz -n prod"
# Claude: analyzes output
```

**After (with MCP):**
```bash
claude "check nginx logs"
# Claude automatically calls nginx_logs tool
# Gets output and analyzes it

claude "why is this pod failing?"
# Claude automatically calls pod_debug tool
# Gets all relevant info in one shot
```

No more:
- Explaining where things are
- Copy/pasting kubectl output
- Running multiple commands manually
- Describing your cluster setup

Claude just knows how to access everything.

## Updates

To update the server:
```bash
cd oscilar-mcp-server
git pull  # if you put it in git
# or manually update index.js
```

Restart Claude Code to pick up changes.

## License

MIT
