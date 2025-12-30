# EFS File System
resource "aws_efs_file_system" "m36" {
  creation_token   = "cartographer-m36"
  performance_mode = "generalPurpose"
  throughput_mode  = "bursting"
  encrypted        = true

  lifecycle_policy {
    transition_to_ia = "AFTER_30_DAYS"
  }

  tags = {
    Name = "cartographer-m36"
  }
}

# EFS Mount Target
resource "aws_efs_mount_target" "m36" {
  file_system_id  = aws_efs_file_system.m36.id
  subnet_id       = aws_subnet.public.id
  security_groups = [aws_security_group.efs.id]
}

# EFS Access Point
resource "aws_efs_access_point" "m36" {
  file_system_id = aws_efs_file_system.m36.id

  posix_user {
    gid = 1000
    uid = 1000
  }

  root_directory {
    path = "/m36-data"
    creation_info {
      owner_gid   = 1000
      owner_uid   = 1000
      permissions = "0755"
    }
  }

  tags = {
    Name = "cartographer-m36-ap"
  }
}
