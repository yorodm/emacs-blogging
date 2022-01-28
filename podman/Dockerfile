FROM alpine:3.15


LABEL org.opencontainers.image.authors="iclcoolster@gmail.com"

RUN apk update && apk add ca-certificates emacs-nox

WORKDIR /root
COPY init.el /root/.emacs.d/
