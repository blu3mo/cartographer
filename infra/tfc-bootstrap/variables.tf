# AWS Credentials for Terraform Cloud
variable "aws_access_key_id" {
  description = "AWS Access Key ID for Terraform Cloud remote execution"
  type        = string
  sensitive   = true
}

variable "aws_secret_access_key" {
  description = "AWS Secret Access Key for Terraform Cloud remote execution"
  type        = string
  sensitive   = true
}

# Cloudflare Configuration
variable "cloudflare_api_token" {
  description = "Cloudflare API Token with Zone DNS and SSL permissions"
  type        = string
  sensitive   = true
}

variable "cloudflare_zone_id" {
  description = "Cloudflare Zone ID for baisoku-kaigi.com"
  type        = string
}

# SSH Key
variable "ssh_public_key" {
  description = "SSH public key for EC2 access"
  type        = string
}
