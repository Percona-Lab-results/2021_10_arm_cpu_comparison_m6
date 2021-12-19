
data "template_file" "init_000_basic" {
  template = "${file("../modules/000_basic.sh.tpl")}"
}

data "template_file" "init_001_setup_R" {
  template = "${file("../modules/001_setup_R.sh.tpl")}"
}

data "template_file" "init_010_apache_web_log" {
  template = "${file("../modules/010_apache_web_log.sh.tpl")}"
  vars= {
    external="${aws_s3_bucket.sysbench_result.id}"
  }
}

data "template_file" "init_030_setup_mysql" {
  template = "${file("../modules/030_setup_mysql.sh.tpl")}"
}

data "template_file" "init_031_configure_mysql" {
  template = "${file("../modules/031_configure_mysql.sh.tpl")}"
}

data "template_file" "init_040_setup_pmm" {
  template = "${file("../modules/040_setup_pmm.sh.tpl")}"
  vars= {
    pmm_server_ip="${aws_instance.ubuntu_pmm_server.public_ip}",
    pmm_public_dns="${aws_instance.ubuntu_pmm_server.public_dns}"
  }
}

data "template_file" "init_042_setup_pmm_arm" {
  template = "${file("../modules/041_setup_pmm_arm.sh.tpl")}"
  vars= {
    pmm_server_ip="${aws_instance.ubuntu_pmm_server.public_ip}",
  }
}

data "template_file" "init_050_run_vm_benchmark_test" {
  template = "${file("../modules/050_run_vm_benchmark_test.sh.tpl")}"
  vars={
    pmm_public_dns="${aws_instance.ubuntu_pmm_server.public_dns}"
  }
}

data "template_file" "init_051_run_big_benchmark_test" {
  template = "${file("../modules/051_big_benchmark_test.sh.tpl")}"
  vars={
    pmm_public_dns="${aws_instance.ubuntu_pmm_server.public_dns}"
  }
}


data "template_file" "init_060_run_oltp_test" {
  template = "${file("../modules/080_run_oltp_read_only.sh.tpl")}"
  vars={
    pmm_public_dns="${aws_instance.ubuntu_pmm_server.public_dns}"
  }
}

data "template_file" "init_090_copy_result" {
  template = "${file("../modules/090_copy_result_to_s3.sh.tpl")}"
  vars= {
    external_s3_bucket="${aws_s3_bucket.sysbench_result.id}"
  }
}
