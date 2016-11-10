#!/bin/sh

NO_CACHE=${NO_CACHE:="TRUE"}

IMAGE=${IMAGE:="c3h3lab/be-a-trader-201611"}
TAG=${VERSION:="seattle"}

if [ "$NO_CACHE" != "TRUE" ]
	then
		docker build -t $IMAGE:$TAG .
else
	docker build --no-cache=true -t $IMAGE:$TAG . 
fi






