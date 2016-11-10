docker stop $(docker ps -aq)
docker rm $(docker ps -aq)


#for i in $(seq 8000 8016); do docker run -p $i:8787 -d c3h3lab/be-a-trader-201608:$VERSION  ;done
