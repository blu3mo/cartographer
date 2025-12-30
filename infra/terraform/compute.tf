# NixOS AMI (community-maintained)
data "aws_ami" "nixos" {
  most_recent = true
  owners      = ["427812963091"]  # NixOS community

  filter {
    name   = "name"
    values = ["nixos/24.05*"]
  }

  filter {
    name   = "architecture"
    values = ["x86_64"]
  }
}

# SSH Key Pair
resource "aws_key_pair" "deploy" {
  key_name   = "cartographer-deploy"
  public_key = var.ssh_public_key
}

# EC2 Instance
resource "aws_instance" "app" {
  ami                    = data.aws_ami.nixos.id
  instance_type          = var.instance_type
  key_name               = aws_key_pair.deploy.key_name
  subnet_id              = aws_subnet.public.id
  vpc_security_group_ids = [aws_security_group.app.id]

  root_block_device {
    volume_size = 30
    volume_type = "gp3"
  }

  # EFS mount via user_data
  user_data = <<-EOF
    #!/bin/bash
    mkdir -p /mnt/efs
    mount -t nfs4 -o nfsvers=4.1 ${aws_efs_file_system.m36.dns_name}:/ /mnt/efs
  EOF

  tags = {
    Name = "cartographer-app"
  }

  depends_on = [aws_efs_mount_target.m36]
}

# Elastic IP
resource "aws_eip" "app" {
  instance = aws_instance.app.id
  domain   = "vpc"

  tags = {
    Name = "cartographer-eip"
  }
}
