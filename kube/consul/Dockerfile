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

ADD https://dl.bintray.com/mitchellh/consul/0.5.2_linux_amd64.zip /tmp/consul.zip
RUN cd /tmp \
 && unzip consul.zip \
 && chmod 755 consul \
 && mv consul /bin/ \
 && rm -rf /tmp/consul.zip

ADD https://dl.bintray.com/mitchellh/consul/0.5.2_web_ui.zip /tmp/webui.zip
RUN cd /tmp \
 && unzip webui.zip \
 && mv dist/ /ui/ \
 && rm -rf /tmp/webui.zip dist

VOLUME /data
VOLUME /secret

ENTRYPOINT [ "/bin/consul" ]
CMD [ "agent", "-config-file=/secret/consul.json" ]

EXPOSE 8300 8301 8301/udp 8302 8302/udp 8400 8443 8600 8600/udp
