
locals {
  cpu_x86 = setproduct(["m6a.large", "m6a.xlarge", "m6a.2xlarge", "m6a.4xlarge", "m6a.8xlarge","m6a.12xlarge","m6a.16xlarge", "m6i.large","m6i.xlarge", "m6i.2xlarge", "m6i.4xlarge", "m6i.8xlarge","m6i.12xlarge","m6i.16xlarge" ], ["gp2"])
    # cpu_x86 = setproduct(["m6a.large", "m6i.large"], ["gp2"])
#
}

resource "aws_instance" "MySQLx86" {
  ami = "ami-09e67e426f25ce0d7"


  for_each = {
    for q in local.cpu_x86 : "${q[0]}-${q[1]}" => {
      instance = q[0]
      storage  = q[1]
    }
  }

  instance_type = each.value.instance

  root_block_device {
    volume_type = "gp2"
    volume_size = "200"
  }

  vpc_security_group_ids = [aws_security_group.percona_lab_sg_m6.id]
  iam_instance_profile   = aws_iam_instance_profile.sysbench_profile_m6.name
  user_data_base64 = "${data.template_cloudinit_config.config_x86.rendered}"

  tags = {
    Name    = "SysBench_MySQL_x86"
    DB      = "MYSQL"
    PMM     = "client"
    Type    = "x86"
    Owner   = "Percona performance"
    Project = "Terraform project"
  }

  depends_on = [
    aws_s3_bucket.sysbench_result,
    aws_iam_role.tf_s3_role_m6,
    aws_iam_instance_profile.sysbench_profile_m6,
    aws_instance.ubuntu_pmm_server
  ]
}

data "template_cloudinit_config" "config_x86" {
  gzip          = true
  base64_encode = true

  part {
  filename     = "000_basic.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_000_basic.rendered}"
  }

  part {
  filename     = "001_setup_R.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_001_setup_R.rendered}"
  }

  part {
  filename     = "010_apache_web_log.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_010_apache_web_log.rendered}"
  }

  part {
  filename     = "030_setup_mysql.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_030_setup_mysql.rendered}"
  }

  part {
  filename     = "031_configure_mysql.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_031_configure_mysql.rendered}"
  }

  part {
  filename     = "040_setup_pmm.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_040_setup_pmm.rendered}"
  }

  part {
  filename     = "050_run_vm_benchmark_test.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_050_run_vm_benchmark_test.rendered}"
  }

  part {
  filename     = "051_run_big_benchmark_test.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_051_run_big_benchmark_test.rendered}"
  }

  part {
  filename     = "080_run_oltp_read_only.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_060_run_oltp_test.rendered}"
  }

  part {
  filename     = "090_copy_result_to_s3.sh.tpl"
  content_type = "text/x-shellscript"
  content      = "${data.template_file.init_090_copy_result.rendered}"
  }

}

output "SysBench_MySQL_x86"{
  value = {
    for key, value_instance in aws_instance.MySQLx86 : key => "\n id = ${value_instance.id} \n public_dns = ${value_instance.public_dns} \n ARN = ${value_instance.arn} \n public_ip = ${value_instance.public_ip} \n private_ip = ${value_instance.private_ip} \n"
    }
}
