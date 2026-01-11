# Production environment variables
# Usage: TF_WORKSPACE=default terraform plan -var-file=prod.tfvars

environment        = "prod"
efs_creation_token = "cartographer-m36"
dns_subdomain      = "app"
