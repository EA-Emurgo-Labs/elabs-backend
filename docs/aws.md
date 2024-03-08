# AWS

## Region
Our infrastructure for Changeblock is set up in the `us-east-1` region.

## ECR
We use the `emurgo-labs-changeblock` repository on ECR to store docker images.

## Beanstalk
Beanstalk is used to launch our environments, with `changeblock-mainnet` and `changeblock-testnet` being the environments.

## Route 35
The registered DNS is `ea-labs.net`, with `testnet` and `mainnet` being the CNAME records that point to their respective Beanstalk URLs.

## ACM
We have two registered certificates, one for `testnet.ea-labs.net` and the other for `mainnet.ea-labs.net`.

## Load balancer
Our load balancer accepts inbound HTTPS requests (Security Group) and has an HTTPS listener that allows access via the SSL certificate.

## Mainnet IP restriction
The security groups of the mainnet load balancer and mainnet EC instance are set up to restrict HTTP, HTTPS, and SSH requests.

## RDS
We are currently using the free tier for our `changeblock-testnet` database which includes these specifications: db.t3.micro, 2 vCPU, 1 GiB RAM, 20 GiB.

We changed the security group to PostgreSQL, which allows access to the PostgreSQL port.

For the main network database, make sure it is not accessible from outside the AWS network.