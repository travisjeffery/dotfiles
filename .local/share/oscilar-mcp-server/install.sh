#!/bin/bash
# Installation script for Oscilar MCP Server

set -e

echo "=== Oscilar MCP Server Installation ==="
echo ""

# Check Node.js version
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed"
    echo "Please install Node.js 18+ first: https://nodejs.org/"
    exit 1
fi

NODE_VERSION=$(node -v | cut -d'v' -f2 | cut -d'.' -f1)
if [ "$NODE_VERSION" -lt 18 ]; then
    echo "❌ Node.js version too old: $(node -v)"
    echo "Please install Node.js 18 or higher"
    exit 1
fi

echo "✅ Node.js $(node -v) found"

# Check kubectl
if ! command -v kubectl &> /dev/null; then
    echo "⚠️  kubectl is not installed"
    echo "Install kubectl to use Kubernetes tools"
else
    echo "✅ kubectl $(kubectl version --client --short 2>/dev/null || kubectl version --client) found"
fi

# Check AWS CLI
if ! command -v aws &> /dev/null; then
    echo "⚠️  AWS CLI is not installed"
    echo "Install AWS CLI to use AWS tools"
else
    echo "✅ AWS CLI $(aws --version) found"
fi

# Install npm dependencies
echo ""
echo "Installing dependencies..."
npm install

if [ $? -ne 0 ]; then
    echo "❌ npm install failed"
    exit 1
fi

echo "✅ Dependencies installed"
echo ""

# Get the absolute path to the server
SERVER_PATH=$(pwd)/index.js

# Detect kubeconfig path
if [ -n "$KUBECONFIG" ]; then
    KUBE_PATH="$KUBECONFIG"
elif [ -f "$HOME/.kube/config" ]; then
    KUBE_PATH="$HOME/.kube/config"
else
    KUBE_PATH=""
fi

# Generate configuration
echo "=== Configuration ==="
echo ""
echo "Server path: $SERVER_PATH"
echo "Kubeconfig: ${KUBE_PATH:-Not found}"
echo ""

# Prompt for namespaces
read -p "Default namespace [default]: " DEFAULT_NS
DEFAULT_NS=${DEFAULT_NS:-default}

read -p "Nginx namespace [ingress-nginx]: " NGINX_NS
NGINX_NS=${NGINX_NS:-ingress-nginx}

read -p "Karpenter namespace [karpenter]: " KARPENTER_NS
KARPENTER_NS=${KARPENTER_NS:-karpenter}

read -p "Monitoring namespace [monitoring]: " MONITORING_NS
MONITORING_NS=${MONITORING_NS:-monitoring}

# Create example config
cat > claude_mcp_config.json << EOF
{
  "mcpServers": {
    "oscilar-infra": {
      "command": "node",
      "args": ["$SERVER_PATH"],
      "env": {
        "KUBECONFIG": "$KUBE_PATH",
        "DEFAULT_NAMESPACE": "$DEFAULT_NS",
        "NGINX_NAMESPACE": "$NGINX_NS",
        "KARPENTER_NAMESPACE": "$KARPENTER_NS",
        "MONITORING_NAMESPACE": "$MONITORING_NS",
        "VICTORIAMETRICS_SERVICE": "victoriametrics"
      }
    }
  }
}
EOF

echo ""
echo "✅ Installation complete!"
echo ""
echo "=== Next Steps ==="
echo ""
echo "1. Copy the configuration to Claude Code config:"
echo ""
echo "   mkdir -p ~/.config/Claude"
echo "   # If config exists, merge this into it:"
echo "   cat claude_mcp_config.json"
echo "   # Otherwise, copy it directly:"
echo "   cp claude_mcp_config.json ~/.config/Claude/claude_desktop_config.json"
echo ""
echo "   OR use the Claude Code CLI:"
echo ""
echo "   claude mcp add oscilar-infra \\"
echo "     -e KUBECONFIG=$KUBE_PATH \\"
echo "     -e DEFAULT_NAMESPACE=$DEFAULT_NS \\"
echo "     -e NGINX_NAMESPACE=$NGINX_NS \\"
echo "     -e KARPENTER_NAMESPACE=$KARPENTER_NS \\"
echo "     -e MONITORING_NAMESPACE=$MONITORING_NS \\"
echo "     -- node $SERVER_PATH"
echo ""
echo "2. Restart Claude Code"
echo ""
echo "3. Verify with: claude → /mcp"
echo ""
echo "4. Try it out:"
echo "   claude 'check nginx status'"
echo "   claude 'get all pods in production'"
echo "   claude 'check Karpenter node provisioning'"
echo ""
echo "Configuration saved to: $(pwd)/claude_mcp_config.json"
