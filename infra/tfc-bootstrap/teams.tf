# チームメンバー管理
# 招待したいメンバーのメールアドレスを terraform.tfvars に設定してください

variable "team_members" {
  description = "招待するチームメンバーのメールアドレス一覧"
  type        = list(string)
  default     = []
}

resource "tfe_organization_membership" "members" {
  for_each = toset(var.team_members)

  organization = data.tfe_organization.plural_reality.name
  email        = each.value
}
