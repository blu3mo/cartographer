# Terraform Cloud Organization (既存をdata sourceで参照)
data "tfe_organization" "plural_reality" {
  name = "plural-reality"
}

# Cartographer Production Workspace
resource "tfe_workspace" "cartographer_prod" {
  name         = "cartographer-prod"
  organization = data.tfe_organization.plural_reality.name

  description    = "Cartographer production infrastructure"
  execution_mode = "remote"
  auto_apply     = false

  # VCS連携なし（CLI駆動）
  working_directory = "infra/terraform"

  tag_names = ["cartographer", "production"]
}

# AWS Credentials (環境変数として設定)
resource "tfe_variable" "aws_access_key_id" {
  workspace_id = tfe_workspace.cartographer_prod.id
  key          = "AWS_ACCESS_KEY_ID"
  value        = var.aws_access_key_id
  category     = "env"
  sensitive    = true
  description  = "AWS Access Key for Terraform Cloud"
}

resource "tfe_variable" "aws_secret_access_key" {
  workspace_id = tfe_workspace.cartographer_prod.id
  key          = "AWS_SECRET_ACCESS_KEY"
  value        = var.aws_secret_access_key
  category     = "env"
  sensitive    = true
  description  = "AWS Secret Key for Terraform Cloud"
}

resource "tfe_variable" "aws_region" {
  workspace_id = tfe_workspace.cartographer_prod.id
  key          = "AWS_REGION"
  value        = "ap-northeast-1"
  category     = "env"
  description  = "AWS Region"
}

# Terraform Variables (terraform.tfvars相当)
resource "tfe_variable" "cloudflare_api_token" {
  workspace_id = tfe_workspace.cartographer_prod.id
  key          = "cloudflare_api_token"
  value        = var.cloudflare_api_token
  category     = "terraform"
  sensitive    = true
  description  = "Cloudflare API Token"
}

resource "tfe_variable" "cloudflare_zone_id" {
  workspace_id = tfe_workspace.cartographer_prod.id
  key          = "cloudflare_zone_id"
  value        = var.cloudflare_zone_id
  category     = "terraform"
  description  = "Cloudflare Zone ID for baisoku-kaigi.com"
}

resource "tfe_variable" "ssh_public_key" {
  workspace_id = tfe_workspace.cartographer_prod.id
  key          = "ssh_public_key"
  value        = var.ssh_public_key
  category     = "terraform"
  description  = "SSH public key for EC2 access"
}

resource "tfe_variable" "domain_name" {
  workspace_id = tfe_workspace.cartographer_prod.id
  key          = "domain_name"
  value        = "baisoku-kaigi.com"
  category     = "terraform"
  description  = "Domain name for the application"
}
