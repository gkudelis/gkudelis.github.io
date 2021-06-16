FROM debian:stable-slim

RUN curl -L https://github.com/getzola/zola/releases/download/v0.9.0/zola-v0.9.0-x86_64-unknown-linux-gnu.tar.gz | tar xz -C /usr/bin

COPY publish.sh /publish.sh

ENTRYPOINT ["/publish.sh"]
