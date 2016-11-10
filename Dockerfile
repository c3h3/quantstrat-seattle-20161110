FROM rocker/rstudio

MAINTAINER Chia-Chi Chang <c3h3.tw@gmail.com>

RUN apt-get update && apt-get install -y libxml2-dev libcurl4-openssl-dev 

ADD install_packages.R /tmp/
RUN cd /tmp && Rscript install_packages.R && rm /tmp/install_packages.R 

RUN mkdir /demo
ENV DEMOPATH /demo 

COPY userconf.sh /etc/cont-init.d/conf

ADD . /demo/
