# Cartographer AWS Infrastructure

Terraform configuration for deploying Cartographer to AWS.

## Prerequisites

- AWS CLI configured
- Terraform >= 1.0
- Cloudflare account with domain

## Usage

```bash
cd infra/terraform
terraform init
terraform plan
terraform apply
```

## Architecture

- EC2 (NixOS) - Next.js + Haskell backend
- EFS - M36 data storage
- Cloudflare - HTTPS/DNS (configured separately)

## implecit context
SSM Parameter Store
- /cartographer/cachix-auth-token に cachix authtoken を格納済