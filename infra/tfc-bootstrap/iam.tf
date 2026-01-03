# Terraform Cloud専用IAMユーザー
resource "aws_iam_user" "terraform_cloud" {
  name = "terraform-cloud-cartographer"
  path = "/terraform/"

  tags = {
    Purpose   = "Terraform Cloud remote execution"
    ManagedBy = "terraform-tfe-bootstrap"
  }
}

# IAMポリシー (必要な権限のみ)
resource "aws_iam_user_policy" "terraform_cloud" {
  name = "terraform-cloud-cartographer-policy"
  user = aws_iam_user.terraform_cloud.name

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "EC2FullAccess"
        Effect = "Allow"
        Action = [
          "ec2:*"
        ]
        Resource = "*"
      },
      {
        Sid    = "EFSFullAccess"
        Effect = "Allow"
        Action = [
          "elasticfilesystem:*"
        ]
        Resource = "*"
      },
      {
        Sid    = "IAMKeyManagement"
        Effect = "Allow"
        Action = [
          "iam:GetUser",
          "iam:CreateAccessKey",
          "iam:DeleteAccessKey",
          "iam:ListAccessKeys"
        ]
        Resource = aws_iam_user.terraform_cloud.arn
      }
    ]
  })
}

# アクセスキー生成
resource "aws_iam_access_key" "terraform_cloud" {
  user = aws_iam_user.terraform_cloud.name
}

# 出力 (初回のみ使用、以降はTFCに保存される)
output "terraform_cloud_access_key_id" {
  description = "Access Key ID for Terraform Cloud (store in TFC variables)"
  value       = aws_iam_access_key.terraform_cloud.id
  sensitive   = true
}

output "terraform_cloud_secret_access_key" {
  description = "Secret Access Key for Terraform Cloud (store in TFC variables)"
  value       = aws_iam_access_key.terraform_cloud.secret
  sensitive   = true
}
