#-*- mode:conf; -*-

FROM ubuntu:14.04
MAINTAINER Tim Dysinger <tim@fpcomplete.com>

# DON'T EXPECT TO INTERACT WITH A PERSON WHILE INSTALLING PACKAGES
ENV DEBIAN_FRONTEND noninteractive

# OPTIONAL: SET UP APT-CACHER USE (REQUIRES RUNNING APT-CACHER-NG ON
# YOUR DOCKER HOST). THIS MAKES REPEATED RE-BUILDS MUCH FASTER.
# RUN apt-get update \
#  && apt-get install -y net-tools
# RUN echo "Acquire::http { Proxy \"http://$(netstat -nr|grep '^0\.0\.0\.0'|awk '{print $2}'):3142\"; };" \
#   | tee /etc/apt/apt.conf.d/02proxy

# INSTALL SOME BASIC THINGS WE NEED FOR NETWORKING
RUN apt-get update && apt-get install -y netbase ca-certificates
# MAKE SURE WE HAVE THE LATEST CA CERTS
RUN update-ca-certificates

# INSTALL OUR GHC RUNTIME SUPPORT LIBRARIES
RUN apt-get install -y libgmp10

# INSTALL GIT >2.3 FROM PPA BECAUSE WE NEED NEWER FEATURES
RUN apt-get install -y software-properties-common
RUN apt-add-repository -y ppa:git-core/ppa
RUN apt-get update
RUN apt-get install -y git

# SET THE ENVIRONMENT FOR $HOME SO OUR GIT/GPG TOOLS WORK AS EXPECTED
ENV HOME=/root

# SET THE WORK DIR TO BE THE YESOD APPLICATION DIRECTORY WHERE CONFIG/
# & STATIC/ ARE (SEE STACK.YAML)
WORKDIR /opt/sig-service/

# EXPOSE THE PORT WE EXPECT TO RUN YESOD ON
EXPOSE 8080

# REMOVE APT-CACHER BECAUSE WE ARE DONE INSTALLING (IN CASE SOMEONE
# WANTS TO EXTEND THIS IMAGE LATER & DOESN'T HAVE APT-CACHER-NG SETUP)
RUN rm /etc/apt/apt.conf.d/02proxy || true

# REMOVE ALL PACKAGE CACHES
RUN apt-get clean
