#!/bin/sh

START_PORT=${START_PORT:=8000}
END_PORT=${END_PORT:=8050}
IMAGE=${IMAGE:="c3h3lab/be-a-trader-201611"}
TAG=${VERSION:="seattle"}


docker stop $(docker ps -aq)
docker rm $(docker ps -aq)
for port in $(seq $START_PORT $END_PORT); do
	docker run -p $port:8787 -d $IMAGE:$TAG
done



