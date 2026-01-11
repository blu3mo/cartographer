# Staging environment variables
# Usage: TF_WORKSPACE=staging terraform plan -var-file=staging.tfvars

environment        = "staging"
efs_creation_token = "cartographer-staging"
dns_subdomain      = "staging"
