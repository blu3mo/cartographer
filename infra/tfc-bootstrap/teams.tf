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

# Ownersチームの情報を取得 (Freeプランではカスタムチーム作成不可のため)
data "tfe_team" "owners" {
  name         = "owners"
  organization = data.tfe_organization.plural_reality.name
}

# メンバーをOwnersチームに追加 (管理者権限付与)
resource "tfe_team_organization_member" "owners" {
  for_each = tfe_organization_membership.members

  team_id                    = data.tfe_team.owners.id
  organization_membership_id = each.value.id
}


