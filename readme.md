# Run docker container locally

1.  clone the repo git clone [git\@github.com](mailto:git@github.com){.email}:valkov/r-api.git

2.  install docker..

3.  build docker image docker build -t r-api -f Dockerfile .

4.  run docker image docker run -p 8000:8000 r-api:latest

------------------------------------------------------------------------

# To make your changes public on DigitalOcean

1.  ssh [root\@206.81.18.129](mailto:root@206.81.18.129){.email} (your ssh token to be added to Droplet instance)
2.  cd r-api
3.  git pull
4.  docker build -t r-api -f Dockerfile .
5.  docker run -p 8000:8000 r-api:latest
6.  to restart: run "docker ps" get container_id and then 'docker restart {container_id}'

## result

<http://206.81.18.129:8000/random_text?l=20>
