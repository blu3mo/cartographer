# SSM Parameter Store for Cachix Auth Token
# Store the token with:
#   aws ssm put-parameter --name "/cartographer/cachix-auth-token" --value "YOUR_TOKEN" --type SecureString --region ap-northeast-1

resource "aws_ssm_parameter" "cachix_auth_token" {
  name        = "/cartographer/cachix-auth-token"
  description = "Cachix authentication token for cache pushing"
  type        = "SecureString"
  value       = var.cachix_auth_token

  tags = {
    Name = "cartographer-cachix-token"
  }

  lifecycle {
    ignore_changes = [value]
  }
}

# IAM Role for EC2 to access SSM Parameter Store
resource "aws_iam_role" "app" {
  name = "cartographer-app-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ec2.amazonaws.com"
        }
      }
    ]
  })
}

# IAM Policy for SSM Parameter Store access
resource "aws_iam_role_policy" "ssm_access" {
  name = "cartographer-ssm-access"
  role = aws_iam_role.app.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "ssm:GetParameter",
          "ssm:GetParameters"
        ]
        Resource = [
          aws_ssm_parameter.cachix_auth_token.arn
        ]
      }
    ]
  })
}

# IAM Instance Profile
resource "aws_iam_instance_profile" "app" {
  name = "cartographer-app-profile"
  role = aws_iam_role.app.name
}
