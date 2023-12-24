FROM alpine:3.19.0

LABEL maintainer="Sachin Patil (iclcoolster@gmail.com)" version="2.1"
LABEL description="Emacs master branch" emacs.version="30.0.50"

RUN apk update && apk add ca-certificates

WORKDIR /root
COPY ./compose/init.el /root/.emacs.d/
COPY ./compose/compile_emacs.sh /root/
RUN mkdir -p /root/source
RUN /bin/sh /root/compile_emacs.sh
