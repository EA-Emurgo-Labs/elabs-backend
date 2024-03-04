# Deploying a docker image

This guide will walk you through setting up a Docker image for the Changeblock backend.

## Step 1: Configure the Application

Prepare the following configuration files:
 `config.json`
`.env`
`root. key`
`wallet. db` (if you start from scratch, you can ignore this file)

## Step 2: Build the Docker Image

Finally, obtain a Cachix authentication token and use it to build the Docker image:

```bash
docker build --build-arg CACHIX_AUTHTOKEN=<AUTHTOKEN> -t elabs-backend .
```

Try running it

```bash
docker run labs-backend
```

That's it! You have now successfully set up the Changeblock backend using Docker.

## Step 3: Register it to the AWS ECR

On AWS, we have a private registry repository called `emurgo-labs-changeblock`. We will push the image to this repository.

### Step 3. 1 Install the AWS CLI
We must install the AWS Command Line Interface to interact with our AWS account. https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html

### Step 3.2 Login using the CLI

```bash
aws ecr get-login-password --region us-east-1 | \
  docker login --username AWS --password-stdin 724240871965.dkr.ecr.us-east-1.amazonaws.com
```
### Step 3.3 Tag and push your docker image

```bash
docker tag <DOCKER_IMAGE> 724240871965.dkr.ecr.us-east-1.amazonaws.com/emurgo-labs-changeblock:latest

docker push 724240871965.dkr.ecr.us-east-1.amazonaws.com/emurgo-labs-changeblock:latest
```

## Step 4 Deploying it via Beanstalk
TODO

# See also
* https://mmhaskell.com/blog/2023/2/20/pushing-our-container-to-aws-ecr
* https://mmhaskell.com/blog/2023/2/23/deploying-a-haskell-server-to-aws
* https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/GettingStarted.Cleanup.html