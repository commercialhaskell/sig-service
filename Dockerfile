#-*- mode:conf; -*-

FROM ubuntu:15.04
MAINTAINER Tim Dysinger <tim@fpcomplete.com>

# DEPENDENCIES
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update \
 && apt-get install -y net-tools
RUN echo "Acquire::http { Proxy \"http://$(netstat -nr|grep '^0\.0\.0\.0'|awk '{print $2}'):3142\"; };" \
    | tee /etc/apt/apt.conf.d/02proxy
RUN apt-get update \
 && apt-get install -y netbase ca-certificates libgmp10 \
 && apt-get clean
RUN update-ca-certificates
RUN rm /etc/apt/apt.conf.d/02proxy

# GIT
RUN apt-get install -y git
ENV HOME=/root
ADD ./.ssh/ $HOME/.ssh/
RUN chmod go-rwsx $HOME/.ssh/
RUN git config --global user.name "Sig Service"
RUN git config --global user.email "dev@fpcomplete.com"
RUN git config --global push.default current

# SIG_SERVICE
ADD ./.cabal-sandbox/bin/sig-service /usr/bin/
ADD ./ /var/lib/sig-service/
WORKDIR /var/lib/sig-service/
CMD sig-service --port 8080
EXPOSE 8080
