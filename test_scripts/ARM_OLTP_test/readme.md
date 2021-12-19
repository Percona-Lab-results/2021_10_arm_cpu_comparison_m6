# Comparing different cpu types performance for Mysql (OLTP read-only test)


## Goal:

the main goal of these scripts if run benchmark on different EC2 and generate data for comparing Intel, AMD and graviton performance

## Preparation:

#### step P-1
Install `terraform`. Just google how to do it. It's quite easy.

#### step P-2
Then provide system variables for terraform to your AWS account
``` shell
export AWS_ACCESS_KEY_ID="%YOUR_AWS_ACCESS_KEY_ID%"
export AWS_SECRET_ACCESS_KEY="%YOUR_AWS_SECRET_ACCESS_KEY%"
export AWS_DEFAULT_REGION="%YOUR_AWS_DEFAULT_REGION%"
```

## Run test

Test were written in `bash`. Infrastructure deployment was written using `terraform`.
there are 4 terraform files:

1. `001_support.tf` -- deploy AWS group, profile and S3 bucket
1. `020_PMM_server.tf` -- deploy AWS instance with PMM server
1. `021_MySql_x86.tf` -- deploy and test Intel and AMD CPU's
1. `022_MySql_ARM.tf` -- deploy and test Graviton CPU

One of condition -- you need to define name of S3 bucket to store raw results.
it  is possible to do with parameter during `terraform apply`
to run the test it is required to run next command form the folder with test:


``` shell
cd ARM_OLTP_test
terraform init
terraform apply -var "s3_bucket_name=variables3bucket"
```

# Destroying:
To destroy all deployed infrastructure you need:

#### step D-1
go to you S3 bucket and delete all information from there
or run:
```
aws s3 rm s3://variables3bucket --recursive
```
#### step D-2

delete infrastructure with command (from the folder with test):

``` shell
cd ARM_OLTP_test
terraform destroy -auto-approve
```
