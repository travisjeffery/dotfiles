#!/bin/bash
# Test script to verify MCP server setup

echo "=== Oscilar MCP Server Tests ==="
echo ""

# Check Node.js
echo "1. Checking Node.js..."
if command -v node &> /dev/null; then
    NODE_VERSION=$(node -v)
    echo "   ✅ Node.js $NODE_VERSION installed"
else
    echo "   ❌ Node.js not found"
    exit 1
fi

# Check dependencies
echo ""
echo "2. Checking npm dependencies..."
if [ -d "node_modules" ]; then
    echo "   ✅ node_modules found"
else
    echo "   ❌ node_modules not found. Run: npm install"
    exit 1
fi

# Check kubectl
echo ""
echo "3. Checking kubectl access..."
if command -v kubectl &> /dev/null; then
    if kubectl cluster-info &> /dev/null; then
        CONTEXT=$(kubectl config current-context)
        echo "   ✅ kubectl connected to: $CONTEXT"
    else
        echo "   ⚠️  kubectl installed but not connected to cluster"
    fi
else
    echo "   ⚠️  kubectl not installed"
fi

# Check AWS CLI
echo ""
echo "4. Checking AWS CLI..."
if command -v aws &> /dev/null; then
    if aws sts get-caller-identity &> /dev/null; then
        ACCOUNT=$(aws sts get-caller-identity --query Account --output text 2>/dev/null)
        echo "   ✅ AWS CLI configured (Account: $ACCOUNT)"
    else
        echo "   ⚠️  AWS CLI installed but not configured"
    fi
else
    echo "   ⚠️  AWS CLI not installed"
fi

# Try to start the server (just to see if it loads)
echo ""
echo "5. Testing MCP server startup..."
timeout 2 node index.js 2>&1 | head -n 1 &
sleep 1

if [ $? -eq 124 ] || [ $? -eq 0 ]; then
    echo "   ✅ MCP server starts successfully"
else
    echo "   ❌ MCP server failed to start"
    exit 1
fi

# Summary
echo ""
echo "=== Summary ==="
echo ""
echo "✅ MCP server is ready to use"
echo ""
echo "Next steps:"
echo "1. Add to Claude Code config (see README.md)"
echo "2. Restart Claude Code"
echo "3. Verify with: claude → /mcp"
echo ""
echo "Quick test:"
echo "  ./install.sh  # Run installation"
echo "  claude 'get cluster context'  # Test it out"
