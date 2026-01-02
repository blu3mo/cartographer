terraform {
  required_version = ">= 1.0"

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

  # Uncomment after first apply to enable remote state
  # backend "s3" {
  #   bucket         = "cartographer-terraform-state"
  #   key            = "prod/terraform.tfstate"
  #   region         = "ap-northeast-1"
  #   encrypt        = true
  # }
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
