#!/usr/bin/env bash
# Pipes PreToolUse payloads through block-dangerous-bash.py and checks each verdict.
# Usage: block-dangerous-bash-test.sh [path/to/block-dangerous-bash.py]

set -uo pipefail

HOOK=$(realpath -- "${1:-$(dirname -- "$(realpath -- "$0")")/../block-dangerous-bash.py}")
cwd=$(mktemp -d -p "$HOME/.cache" block-dangerous-bash-test.XXXXXX)
trap 'rmdir -- "$cwd"' EXIT

pass=0 fail=0

# verdict COMMAND -> block | allow | error(RC)
verdict() {
  local rc
  jq -nc --arg c "$1" --arg d "$cwd" '{tool_name:"Bash",tool_input:{command:$c},cwd:$d}' |
    (cd "$cwd" && CLAUDE_PROJECT_DIR=$cwd python3 "$HOOK" >/dev/null 2>&1)
  rc=$?
  if (( rc == 2 )); then echo block
  elif (( rc != 0 )); then echo "error($rc)"
  else echo allow
  fi
}

expect() {
  local want=$1 name=$2 cmd=$3 got
  got=$(verdict "$cmd")
  if [[ $got == "$want" ]]; then printf 'ok   %s: %s\n' "$name" "$got"; (( pass++ ))
  else printf 'FAIL %s: want %s, got %s\n' "$name" "$want" "$got"; (( fail++ ))
  fi
}

# Existing guards.
expect block "rm -rf outside roots" 'rm -rf /etc/x'
expect allow "rm -rf inside cwd" 'rm -rf build'
expect block "git push --force" 'git push --force origin main'
expect allow "git push --force-with-lease" 'git push --force-with-lease origin main'
expect block "kubectl delete" 'kubectl delete pod x'
expect block "terraform destroy" 'terraform destroy'
expect block "pulumi destroy" 'pulumi destroy --yes'
expect block "compound command" 'git status; kubectl delete ns x'
expect block "sudo rm -rf" 'sudo rm -rf /etc/x'
expect block "env-var prefix on kubectl" 'KUBECONFIG=/x kubectl delete pod x'

# AWS: delete- and terminate- operations.
expect block "ec2 terminate-instances" 'aws ec2 terminate-instances --instance-ids i-1'
expect block "cloudformation delete-stack" 'aws cloudformation delete-stack --stack-name s'
expect block "s3api delete-object" 'aws s3api delete-object --bucket b --key k'
expect block "s3api delete-objects" 'aws s3api delete-objects --bucket b --delete file://d.json'
expect block "s3api delete-bucket" 'aws s3api delete-bucket --bucket b'
expect block "ecr batch-delete-image" 'aws ecr batch-delete-image --repository-name r --image-ids imageTag=x'
expect block "cognito admin-delete-user" 'aws cognito-idp admin-delete-user --user-pool-id p --username u'
expect block "emr terminate-job-flows" 'aws emr terminate-job-flows --job-flow-ids j-1'
expect block "autoscaling terminate-instance-in-asg" \
  'aws autoscaling terminate-instance-in-auto-scaling-group --instance-id i-1 --should-decrement-desired-capacity'

# AWS: destroy-shaped operations without a delete or terminate word.
expect block "s3 rm" 'aws s3 rm s3://b/k'
expect block "s3 rm --recursive" 'aws s3 rm s3://b/ --recursive'
expect block "s3 rb" 'aws s3 rb s3://b --force'
expect block "s3 sync --delete" 'aws s3 sync ./out s3://b/ --delete'
expect block "sqs purge-queue" 'aws sqs purge-queue --queue-url https://q'
expect block "ec2 deregister-image" 'aws ec2 deregister-image --image-id ami-1'
expect block "ec2 release-address" 'aws ec2 release-address --allocation-id eipalloc-1'
expect block "ec2 release-hosts" 'aws ec2 release-hosts --host-ids h-1'
expect block "kms schedule-key-deletion" 'aws kms schedule-key-deletion --key-id k --pending-window-in-days 7'
expect block "account disable-region" 'aws account disable-region --region-name af-south-1'
expect block "organizations close-account" 'aws organizations close-account --account-id 1'
expect block "organizations leave-organization" 'aws organizations leave-organization'

# AWS: global options, env-var prefixes and wrappers.
expect block "global options before service" \
  'aws --profile x --region y ec2 terminate-instances --instance-ids i-1'
expect block "--opt=value before service" 'aws --profile=x --output json rds delete-db-instance --db-instance-identifier d'
expect block "boolean global options" 'aws --debug --no-cli-pager s3 rb s3://b'
expect block "AWS_PROFILE prefix" 'AWS_PROFILE=prod aws ec2 terminate-instances --instance-ids i-1'
expect block "two env-var prefixes" 'AWS_PROFILE=prod AWS_REGION=us-east-1 aws dynamodb delete-table --table-name t'
expect block "sudo -E aws" 'sudo -E aws s3 rm s3://b/k'
expect block "env aws" 'env AWS_PROFILE=prod aws sqs purge-queue --queue-url https://q'
expect block "aws-vault exec --" 'aws-vault exec prod -- aws ec2 terminate-instances --instance-ids i-1'
expect block "full path to aws" '/usr/local/bin/aws lambda delete-function --function-name f'
expect block "after cd &&" 'cd /tmp && aws s3 rm s3://b/k'
expect block "in a pipeline" 'echo i-1 | xargs -n1 echo; aws ec2 terminate-instances --instance-ids i-1'
expect block "ec2 --no-dry-run" 'aws ec2 terminate-instances --instance-ids i-1 --no-dry-run'

# AWS: allowed.
expect allow "describe" 'aws ec2 describe-instances'
expect allow "list" 'aws --profile x s3api list-objects-v2 --bucket b'
expect allow "get" 'AWS_PROFILE=prod aws ssm get-parameter --name n'
expect allow "describe with delete in name" 'aws ec2 describe-instance-attribute --attribute disableApiTermination --instance-id i-1'
expect allow "get with delete in name" 'aws s3api get-bucket-lifecycle-configuration --bucket b'
expect allow "list with delete word" 'aws backup list-recovery-points-by-backup-vault --backup-vault-name delete-me'
expect allow "s3 ls" 'aws s3 ls s3://b/'
expect allow "s3 cp" 'aws s3 cp s3://b/k ./k'
expect allow "s3 sync without --delete" 'aws s3 sync ./out s3://b/'
expect allow "ec2 --dry-run" 'aws ec2 terminate-instances --instance-ids i-1 --dry-run'
expect allow "ec2 --dry-run with globals" 'aws --profile x ec2 delete-vpc --vpc-id vpc-1 --dry-run'
expect allow "s3 rm --dryrun" 'aws s3 rm s3://b/ --recursive --dryrun'
expect allow "kms cancel-key-deletion" 'aws kms cancel-key-deletion --key-id k'
expect allow "ec2 stop-instances" 'aws ec2 stop-instances --instance-ids i-1'
expect allow "iam detach-role-policy" 'aws iam detach-role-policy --role-name r --policy-arn a'
expect allow "iam remove-role-from-instance-profile" \
  'aws iam remove-role-from-instance-profile --instance-profile-name p --role-name r'
expect allow "elbv2 deregister-targets" 'aws elbv2 deregister-targets --target-group-arn a --targets Id=i-1'
expect allow "elbv2 enable deletion protection" 'aws elbv2 modify-load-balancer-attributes --attributes Key=deletion_protection.enabled,Value=true'
expect allow "bare aws" 'aws --version'
expect allow "aws help" 'aws ec2 help'

# Helm.
expect block "helm uninstall" 'helm uninstall my-release'
expect block "helm delete" 'helm delete my-release'
expect block "helm del" 'helm del my-release'
expect block "helm un" 'helm un my-release'
expect block "helm global flags first" 'helm --kube-context prod -n apps uninstall my-release'
expect block "helm --flag=value first" 'helm --namespace=apps --debug delete my-release'
expect block "helm with KUBECONFIG prefix" 'KUBECONFIG=~/.kube/prod helm uninstall my-release'
expect block "helm uninstall --keep-history" 'helm uninstall my-release --keep-history -n apps'
expect allow "helm uninstall --dry-run" 'helm uninstall my-release --dry-run'
expect allow "helm del --dry-run with globals" 'helm -n apps del my-release --dry-run'
expect allow "helm list" 'helm list -A'
expect allow "helm upgrade" 'helm upgrade --install my-release ./chart'
expect allow "helm plugin uninstall" 'helm plugin uninstall diff'
expect allow "helm repo remove" 'helm repo remove bitnami'
expect allow "helm namespace named delete" 'helm -n delete list'

# Mentions that are data, not commands.
expect allow "commit message" 'git commit -m "Block aws ec2 terminate-instances and helm uninstall"'
expect allow "echo" "echo 'run: aws s3 rm s3://b/k && helm uninstall x'"
expect allow "grep pattern" 'grep -n "helm uninstall\|aws s3 rb" README.md'
expect allow "heredoc commit message" "git commit -F - <<'EOF'
Block aws ec2 terminate-instances, aws s3 rm and helm uninstall
AWS_PROFILE=prod aws dynamodb delete-table --table-name t
helm delete my-release
EOF"
expect allow "heredoc to a file" "cat > notes.md <<EOF
aws --profile x ec2 terminate-instances --instance-ids i-1
helm --kube-context prod un my-release
EOF"
expect block "command after a heredoc" "cat > notes.md <<EOF
nothing here
EOF
helm uninstall my-release"

printf '\n%d passed, %d failed\n' "$pass" "$fail"
(( fail == 0 ))
