# Cloudflare DNS Configuration for baisoku-kaigi.com

provider "cloudflare" {
  api_token = var.cloudflare_api_token
}

# DNS Records
resource "cloudflare_record" "app" {
  zone_id         = var.cloudflare_zone_id
  name            = "app"
  content         = aws_eip.app.public_ip
  type            = "A"
  proxied         = true
  allow_overwrite = true
  comment         = "Managed by Terraform"
}

# Cloudflare Origin Certificate (15 years)
# This certificate is trusted by Cloudflare's edge servers for Full (strict) SSL
resource "cloudflare_origin_ca_certificate" "origin" {
  csr                = tls_cert_request.origin.cert_request_pem
  hostnames          = [var.domain_name, "*.${var.domain_name}"]
  request_type       = "origin-rsa"
  requested_validity = 5475 # 15 years (maximum)
}

# Private key for origin certificate
resource "tls_private_key" "origin" {
  algorithm = "RSA"
  rsa_bits  = 2048
}

# Certificate signing request
resource "tls_cert_request" "origin" {
  private_key_pem = tls_private_key.origin.private_key_pem

  subject {
    common_name  = var.domain_name
    organization = "Cartographer"
  }
}

# Output certificates for NixOS configuration
output "origin_certificate" {
  description = "Cloudflare Origin Certificate (PEM)"
  value       = cloudflare_origin_ca_certificate.origin.certificate
  sensitive   = true
}

output "origin_private_key" {
  description = "Origin Certificate Private Key (PEM)"
  value       = tls_private_key.origin.private_key_pem
  sensitive   = true
}

# Write certificate files for Colmena deployment
resource "local_file" "origin_cert" {
  content         = cloudflare_origin_ca_certificate.origin.certificate
  filename        = "${path.module}/origin-cert.pem"
  file_permission = "0644"
}

resource "local_sensitive_file" "origin_key" {
  content         = tls_private_key.origin.private_key_pem
  filename        = "${path.module}/origin-key.pem"
  file_permission = "0600"
}

