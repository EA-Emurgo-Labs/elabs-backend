# Building the Docker image for the Changeblock backend

This guide will walk you through setting up a Docker image for the Changeblock backend.

## Step 1: Configure the Application

Prepare the following configuration files:
 `config.json`
`.env`
`root.key`
`wallet.db` (if you start from scratch, you can ignore this file)

## Step 2: Build the Docker Image

Finally, obtain a Cachix authentication token and use it to build the Docker image:

```bash
docker build --build-arg CACHIX_AUTHTOKEN=<AUTHTOKEN> -t elabs-backend .
```

That's it! You have now successfully set up the Changeblock backend using Docker.
