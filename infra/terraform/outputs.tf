output "instance_public_ip" {
  description = "Public IP of EC2 instance (for Cloudflare DNS)"
  value       = aws_eip.app.public_ip
}

output "efs_dns_name" {
  description = "EFS DNS name for mounting"
  value       = aws_efs_file_system.m36.dns_name
}

output "ssh_command" {
  description = "SSH command to connect"
  value       = "ssh root@${aws_eip.app.public_ip}"
}

output "domain_url" {
  description = "Application URL (after DNS propagation)"
  value       = "https://${var.dns_subdomain}.${var.domain_name}/"
}

output "efs_id" {
  description = "EFS File System ID"
  value       = aws_efs_file_system.m36.id
}

output "domain" {
  description = "Fully qualified domain name"
  value       = "${var.dns_subdomain}.${var.domain_name}"
}

resource "local_file" "infra_json" {
  filename = "${path.module}/infra-${terraform.workspace}.json"
  content  = jsonencode({
    instance_public_ip = aws_eip.app.public_ip
    efs_dns_name       = aws_efs_file_system.m36.dns_name
    domain             = "${var.dns_subdomain}.${var.domain_name}"
    domain_url         = "https://${var.dns_subdomain}.${var.domain_name}/"
  })
}
