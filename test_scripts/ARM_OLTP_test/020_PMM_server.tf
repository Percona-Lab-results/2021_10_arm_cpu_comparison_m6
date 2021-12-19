
resource "aws_instance" "ubuntu_pmm_server" {
  ami           = "ami-09e67e426f25ce0d7"
  instance_type = "t2.medium"

  vpc_security_group_ids = [aws_security_group.percona_lab_sg_c5.id]
  iam_instance_profile   = aws_iam_instance_profile.sysbench_profile_c5.name
  user_data              = file("../modules/install_pmm_server.sh")

  tags = {
    Name    = "PMM_Server"
    PMM     = "Server"
    Type    = "x86"
    Owner   = "Percona performance"
    Project = "Terraform project"
  }

  depends_on = [
    aws_s3_bucket.sysbench_result,
    aws_iam_role.tf_s3_role_c5,
    aws_iam_instance_profile.sysbench_profile_c5
  ]
}

output "PMM_SERVER_STATS"{
  value = " public_ip = ${aws_instance.ubuntu_pmm_server.public_ip} \n private_ip = ${aws_instance.ubuntu_pmm_server.private_ip} \n ARN = ${aws_instance.ubuntu_pmm_server.arn} \n public_dns = ${aws_instance.ubuntu_pmm_server.public_dns}"

}
