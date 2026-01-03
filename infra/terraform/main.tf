terraform {
  required_version = ">= 1.0"

  cloud {
    organization = "plural-reality"
    workspaces {
      name = "cartographer-prod"
    }
  }

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 4.0"
    }
    tls = {
      source  = "hashicorp/tls"
      version = "~> 4.0"
    }
  }
}

provider "aws" {
  region = var.aws_region

  default_tags {
    tags = {
      Project     = "cartographer"
      Environment = var.environment
      ManagedBy   = "terraform"
    }
  }
}
