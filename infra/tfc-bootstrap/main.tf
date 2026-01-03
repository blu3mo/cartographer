# Terraform Cloud Bootstrap Configuration
# このディレクトリはTerraform Cloudワークスペース自体をコード管理します

terraform {
  required_version = ">= 1.5"

  required_providers {
    tfe = {
      source  = "hashicorp/tfe"
      version = "~> 0.60"
    }
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }

  # Bootstrap自体はローカルstateで管理（鶏卵問題回避）
  # 必要に応じてS3 backendに変更可能
}

provider "tfe" {
  # terraform login で取得したトークンを自動使用
}

provider "aws" {
  region = "ap-northeast-1"
}
