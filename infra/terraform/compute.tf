# NixOS Community AMI
data "aws_ami" "nixos" {
  most_recent = true
  owners      = ["427812963091"] # NixOS community

  filter {
    name   = "name"
    values = ["nixos/25.11*-aarch64-linux"]
  }

  filter {
    name   = "architecture"
    values = ["arm64"]
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
  ipv6_address_count     = 1

  root_block_device {
    volume_size = 30
    volume_type = "gp3"
  }

  # Mount EFS
  user_data = <<-EOF
    #!/run/current-system/sw/bin/bash
    mkdir -p /mnt/efs
    mount -t nfs4 -o nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2 ${aws_efs_file_system.m36.dns_name}:/ /mnt/efs
    mkdir -p /mnt/efs/m36-data
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
