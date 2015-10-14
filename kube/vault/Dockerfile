#-*- mode: conf -*-

FROM ubuntu:14.04

ENV DEBIAN_FRONTEND noninteractive
ENV LANG en_US.UTF-8
RUN locale-gen $LANG

RUN apt-get update
RUN apt-get install -y net-tools
RUN echo "Acquire::http { Proxy \"http://$(netstat -nr|grep '^0\.0\.0\.0'|awk '{print $2}'):3142\"; };" \
  | tee /etc/apt/apt.conf.d/02proxy

RUN apt-get install -y netbase ca-certificates unzip

RUN update-ca-certificates \
 && apt-get clean \
 && rm /etc/apt/apt.conf.d/02proxy

ADD https://dl.bintray.com/mitchellh/vault/vault_0.3.1_linux_amd64.zip /tmp/vault.zip
RUN cd /tmp \
 && unzip vault.zip \
 && chmod 755 vault \
 && mv vault /bin/ \
 && rm -rf /tmp/vault.zip

VOLUME /data
VOLUME /secret

ENTRYPOINT [ "/bin/vault" ]
CMD [ "server", "-config=/secret/vault.hcl" ]

EXPOSE 8443
