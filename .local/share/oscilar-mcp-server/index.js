#!/usr/bin/env node

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import { exec } from "child_process";
import { promisify } from "util";

const execAsync = promisify(exec);

// Configuration from environment
const DEFAULT_NAMESPACE = process.env.DEFAULT_NAMESPACE || "default";
const NGINX_NAMESPACE = process.env.NGINX_NAMESPACE || "ingress-nginx";
const KARPENTER_NAMESPACE = process.env.KARPENTER_NAMESPACE || "karpenter";
const MONITORING_NAMESPACE = process.env.MONITORING_NAMESPACE || "monitoring";
const VICTORIAMETRICS_SERVICE = process.env.VICTORIAMETRICS_SERVICE || "victoriametrics";

// Helper to execute shell commands
async function runCommand(command, description) {
  try {
    const { stdout, stderr } = await execAsync(command, {
      maxBuffer: 10 * 1024 * 1024, // 10MB buffer for large outputs
    });
    
    if (stderr && !stdout) {
      return { success: false, error: stderr, description };
    }
    
    return { success: true, output: stdout, description };
  } catch (error) {
    return { 
      success: false, 
      error: error.message,
      stderr: error.stderr,
      description 
    };
  }
}

// Create server instance
const server = new Server(
  {
    name: "oscilar-mcp-server",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// Define available tools
server.setRequestHandler(ListToolsRequestSchema, async () => {
  return {
    tools: [
      {
        name: "kubectl",
        description: "Execute kubectl commands. Use this for any Kubernetes operations like get, describe, logs, etc.",
        inputSchema: {
          type: "object",
          properties: {
            command: {
              type: "string",
              description: "The kubectl command to run (without 'kubectl' prefix). Example: 'get pods -n default' or 'logs pod-name -n namespace --tail=100'",
            },
          },
          required: ["command"],
        },
      },
      {
        name: "nginx_logs",
        description: "Get nginx ingress controller logs with optional filters",
        inputSchema: {
          type: "object",
          properties: {
            tail: {
              type: "number",
              description: "Number of lines to tail (default: 100)",
            },
            follow: {
              type: "boolean",
              description: "Follow logs in real-time (default: false)",
            },
            grep: {
              type: "string",
              description: "Optional grep pattern to filter logs",
            },
          },
        },
      },
      {
        name: "nginx_status",
        description: "Get comprehensive nginx ingress controller status including pods, service, and config",
        inputSchema: {
          type: "object",
          properties: {},
        },
      },
      {
        name: "karpenter_status",
        description: "Get Karpenter node provisioner status, NodePools, NodeClaims, and recent activity",
        inputSchema: {
          type: "object",
          properties: {},
        },
      },
      {
        name: "pod_debug",
        description: "Get comprehensive debugging information for a specific pod",
        inputSchema: {
          type: "object",
          properties: {
            pod_name: {
              type: "string",
              description: "Name of the pod to debug",
            },
            namespace: {
              type: "string",
              description: "Namespace of the pod (default: default)",
            },
            include_logs: {
              type: "boolean",
              description: "Include pod logs in output (default: true)",
            },
          },
          required: ["pod_name"],
        },
      },
      {
        name: "victoriametrics_query",
        description: "Query VictoriaMetrics using PromQL",
        inputSchema: {
          type: "object",
          properties: {
            query: {
              type: "string",
              description: "PromQL query to execute",
            },
            time: {
              type: "string",
              description: "Optional time parameter (RFC3339 or Unix timestamp)",
            },
          },
          required: ["query"],
        },
      },
      {
        name: "aws_cli",
        description: "Execute AWS CLI commands for checking resources like NLB health, EC2 instances, etc.",
        inputSchema: {
          type: "object",
          properties: {
            command: {
              type: "string",
              description: "The AWS CLI command to run (without 'aws' prefix). Example: 'elbv2 describe-target-health --target-group-arn arn:...'",
            },
          },
          required: ["command"],
        },
      },
      {
        name: "get_cluster_context",
        description: "Get current kubectl context and cluster information",
        inputSchema: {
          type: "object",
          properties: {},
        },
      },
      {
        name: "check_pod_resources",
        description: "Check resource usage (CPU/Memory) for pods in a namespace",
        inputSchema: {
          type: "object",
          properties: {
            namespace: {
              type: "string",
              description: "Namespace to check (default: all namespaces)",
            },
            sort_by: {
              type: "string",
              enum: ["cpu", "memory"],
              description: "Sort by cpu or memory usage",
            },
          },
        },
      },
    ],
  };
});

// Handle tool execution
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;

  try {
    switch (name) {
      case "kubectl": {
        const command = `kubectl ${args.command}`;
        const result = await runCommand(command, `kubectl ${args.command}`);
        return {
          content: [
            {
              type: "text",
              text: result.success 
                ? result.output 
                : `Error: ${result.error}\n${result.stderr || ''}`,
            },
          ],
        };
      }

      case "nginx_logs": {
        const tail = args.tail || 100;
        const follow = args.follow ? "-f" : "";
        const grepCmd = args.grep ? `| grep -i "${args.grep}"` : "";
        
        const command = `kubectl logs ${follow} -n ${NGINX_NAMESPACE} -l app.kubernetes.io/name=ingress-nginx --tail=${tail} ${grepCmd}`;
        const result = await runCommand(command, "nginx ingress logs");
        
        return {
          content: [
            {
              type: "text",
              text: result.success 
                ? result.output 
                : `Error: ${result.error}`,
            },
          ],
        };
      }

      case "nginx_status": {
        const commands = [
          `kubectl get pods -n ${NGINX_NAMESPACE} -l app.kubernetes.io/name=ingress-nginx`,
          `kubectl get svc -n ${NGINX_NAMESPACE}`,
          `kubectl get ingress -A`,
          `kubectl describe pod -n ${NGINX_NAMESPACE} -l app.kubernetes.io/name=ingress-nginx | grep -A 10 "Events:"`,
        ];

        const results = await Promise.all(
          commands.map((cmd) => runCommand(cmd, cmd))
        );

        const output = results
          .map((r, i) => {
            return `=== ${r.description} ===\n${r.success ? r.output : `Error: ${r.error}`}\n`;
          })
          .join("\n");

        return {
          content: [
            {
              type: "text",
              text: output,
            },
          ],
        };
      }

      case "karpenter_status": {
        const commands = [
          `kubectl get pods -n ${KARPENTER_NAMESPACE}`,
          `kubectl get nodepools`,
          `kubectl get nodeclaims`,
          `kubectl get nodes -o custom-columns=NAME:.metadata.name,INSTANCE:.metadata.labels.node\\.kubernetes\\.io/instance-type,CAPACITY:.metadata.labels.karpenter\\.sh/capacity-type`,
          `kubectl logs -n ${KARPENTER_NAMESPACE} -l app.kubernetes.io/name=karpenter --tail=50 | grep -E "(created|deleted|launched|terminated|consolidat)" || echo "No relevant logs found"`,
        ];

        const results = await Promise.all(
          commands.map((cmd) => runCommand(cmd, cmd))
        );

        const output = results
          .map((r) => {
            return `=== ${r.description} ===\n${r.success ? r.output : `Error: ${r.error}`}\n`;
          })
          .join("\n");

        return {
          content: [
            {
              type: "text",
              text: output,
            },
          ],
        };
      }

      case "pod_debug": {
        const namespace = args.namespace || DEFAULT_NAMESPACE;
        const podName = args.pod_name;
        const includeLogs = args.include_logs !== false;

        const commands = [
          `kubectl get pod ${podName} -n ${namespace} -o wide`,
          `kubectl describe pod ${podName} -n ${namespace}`,
          `kubectl get events -n ${namespace} --field-selector involvedObject.name=${podName} --sort-by='.lastTimestamp' | tail -20`,
        ];

        if (includeLogs) {
          commands.push(`kubectl logs ${podName} -n ${namespace} --tail=100`);
          // Try to get previous logs if pod restarted
          commands.push(`kubectl logs ${podName} -n ${namespace} --previous --tail=50 2>&1 || echo "No previous logs available"`);
        }

        const results = await Promise.all(
          commands.map((cmd) => runCommand(cmd, cmd))
        );

        const output = results
          .map((r) => {
            return `=== ${r.description} ===\n${r.success ? r.output : `Error: ${r.error}`}\n`;
          })
          .join("\n");

        return {
          content: [
            {
              type: "text",
              text: output,
            },
          ],
        };
      }

      case "victoriametrics_query": {
        const query = encodeURIComponent(args.query);
        const timeParam = args.time ? `&time=${args.time}` : "";
        
        // Port-forward command (user needs to set this up separately or we can use kubectl proxy)
        const vmUrl = `http://${VICTORIAMETRICS_SERVICE}.${MONITORING_NAMESPACE}.svc:8428`;
        const curlCmd = `kubectl run -n ${MONITORING_NAMESPACE} --rm -i --restart=Never --image=curlimages/curl curl-vm -- curl -s "${vmUrl}/api/v1/query?query=${query}${timeParam}"`;
        
        const result = await runCommand(curlCmd, "VictoriaMetrics query");
        
        return {
          content: [
            {
              type: "text",
              text: result.success 
                ? result.output 
                : `Error: ${result.error}\n\nNote: Make sure VictoriaMetrics is accessible at ${vmUrl}`,
            },
          ],
        };
      }

      case "aws_cli": {
        const command = `aws ${args.command}`;
        const result = await runCommand(command, `aws ${args.command}`);
        
        return {
          content: [
            {
              type: "text",
              text: result.success 
                ? result.output 
                : `Error: ${result.error}`,
            },
          ],
        };
      }

      case "get_cluster_context": {
        const commands = [
          `kubectl config current-context`,
          `kubectl cluster-info`,
          `kubectl get nodes`,
        ];

        const results = await Promise.all(
          commands.map((cmd) => runCommand(cmd, cmd))
        );

        const output = results
          .map((r) => {
            return `=== ${r.description} ===\n${r.success ? r.output : `Error: ${r.error}`}\n`;
          })
          .join("\n");

        return {
          content: [
            {
              type: "text",
              text: output,
            },
          ],
        };
      }

      case "check_pod_resources": {
        const namespace = args.namespace ? `-n ${args.namespace}` : "-A";
        const sortBy = args.sort_by || "cpu";
        
        const command = `kubectl top pods ${namespace} --sort-by=${sortBy}`;
        const result = await runCommand(command, "pod resource usage");
        
        return {
          content: [
            {
              type: "text",
              text: result.success 
                ? result.output 
                : `Error: ${result.error}\n\nNote: This requires metrics-server to be installed in the cluster`,
            },
          ],
        };
      }

      default:
        return {
          content: [
            {
              type: "text",
              text: `Unknown tool: ${name}`,
            },
          ],
          isError: true,
        };
    }
  } catch (error) {
    return {
      content: [
        {
          type: "text",
          text: `Error executing ${name}: ${error.message}`,
        },
      ],
      isError: true,
    };
  }
});

// Start the server
async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("Oscilar MCP Server running on stdio");
}

main().catch((error) => {
  console.error("Fatal error in main():", error);
  process.exit(1);
});
