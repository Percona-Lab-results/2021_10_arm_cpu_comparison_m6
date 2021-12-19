variable "s3_bucket_name" {
  description = "Value of the Name S3 bucket"
  type        = string
  default     = "perconatempsysbenchresult"
}


resource "aws_s3_bucket" "sysbench_result" {
  bucket = var.s3_bucket_name
  acl    = "private"

  tags = {
    Name = "Performance results"
    Owner = "Percona performance"
  }
}

resource "aws_iam_instance_profile" "sysbench_profile_c5" {
  name = "s3_sysbench_c5"
  role = aws_iam_role.tf_s3_role_c5.name
}

resource "aws_iam_role" "tf_s3_role_c5" {
  name = "tf_s3_role_c5"
  assume_role_policy = jsonencode({
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "ec2.amazonaws.com"
      }
    }]
    Version = "2012-10-17"
  })
  path                = "/"
  managed_policy_arns = ["arn:aws:iam::aws:policy/AmazonS3FullAccess"]
}


resource "aws_security_group" "percona_lab_sg_c5" {
  name        = "Sysbench Security Group C5"
  description = "percona sysbench testing SecurityGroup"


  dynamic "ingress" {
    for_each = ["80", "443", "8080", "8081", "1541", "9092", "9093"]
    content {
      from_port   = ingress.value
      to_port     = ingress.value
      protocol    = "tcp"
      cidr_blocks = ["0.0.0.0/0"]
    }
  }

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name  = "percona lab sysbench SecurityGroup"
    Owner = "Percona performance"
  }
}
