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
