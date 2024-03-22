# How to restart the docker container in AWS Console

1. Click EC2 on the left menu

2. Click "instances(running)"

3. Click to your instance id, for example: instance id of changeblock-testnet is "i-0eb0c5e03c74ca70f"

4. Click Action -> Connect -> Connect (on the bottom right), then wait a minute to establish the connection

5. List all container

```bash
[root@ip-172-31-46-114 ~]# sudo docker ps -a 
CONTAINER ID   IMAGE          COMMAND                  CREATED      STATUS         PORTS      NAMES
cffb38c8ed28   3b76219da2dc   "/app/bin/app run --…"   3 days ago   Up 7 minutes   8081/tcp   practical_volhard
```

6. Restart the container with container id

```bash
[root@ip-172-31-46-114 ~]# sudo docker restart cffb38c8ed28
```

7. Check docker's log

```bash
[root@ip-172-31-46-114 ~]# sudo docker logs -f cffb38c8ed28
```
