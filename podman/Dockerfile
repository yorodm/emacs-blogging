FROM alpine:3.15

LABEL org.opencontainers.image.authors="Sachin Patil" maintainer="iclcoolster@gmail.com" version="2.1"
LABEL description="Emacs master branch" emacs.version="29.0.50"

RUN apk update && apk add ca-certificates

WORKDIR /root
COPY init.el /root/.emacs.d/
COPY compile_emacs.sh /root/
RUN /bin/sh /root/compile_emacs.sh
