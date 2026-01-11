variable "aws_region" {
  description = "AWS region"
  type        = string
  default     = "ap-northeast-1"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "prod"
}

variable "instance_type" {
  description = "EC2 instance type (ARM)"
  type        = string
  default     = "t4g.medium"
}

variable "ssh_public_key" {
  description = "SSH public key for EC2 access"
  type        = string
}

# Cloudflare Configuration
variable "cloudflare_api_token" {
  description = "Cloudflare API token with Zone DNS edit permissions"
  type        = string
  sensitive   = true
}

variable "cloudflare_zone_id" {
  description = "Cloudflare Zone ID for baisoku-kaigi.com"
  type        = string
}

variable "domain_name" {
  description = "Domain name for the application"
  type        = string
  default     = "baisoku-kaigi.com"
}

variable "efs_creation_token" {
  description = "EFS creation token (unique per workspace)"
  type        = string
  default     = "cartographer-m36"
}

variable "dns_subdomain" {
  description = "DNS subdomain (e.g., 'app' or 'staging')"
  type        = string
  default     = "app"
}
