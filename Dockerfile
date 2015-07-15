#-*- mode:conf; -*-

FROM ubuntu:14.04
MAINTAINER Tim Dysinger <tim@fpcomplete.com>

# APT
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update \
 && apt-get install -y net-tools
RUN echo "Acquire::http { Proxy \"http://$(netstat -nr|grep '^0\.0\.0\.0'|awk '{print $2}'):3142\"; };" \
  | tee /etc/apt/apt.conf.d/02proxy
RUN apt-get update \
 && apt-get install -y netbase ca-certificates libgmp10 \
 && apt-get clean
RUN update-ca-certificates

# LOCALES
ENV LANG en_US.UTF-8
RUN locale-gen en_US.UTF-8
RUN dpkg-reconfigure locales

# GIT >2.3
RUN apt-get install -y software-properties-common
RUN apt-add-repository -y ppa:git-core/ppa
RUN apt-get update
RUN apt-get install -y git

# PROJECT
ENV HOME=/root
WORKDIR /usr/local/bin
EXPOSE 8080

# CLEANUP
RUN rm /etc/apt/apt.conf.d/02proxy
