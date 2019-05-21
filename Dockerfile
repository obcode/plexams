FROM fpco/stack-build:lts-13.19 as builder
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM ubuntu:16.04
RUN mkdir -p /opt/myapp
ARG BINARY_PATH
WORKDIR /opt/myapp
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
# NOTICE THIS LINE
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.21/8.6.52/bin .
CMD ["/opt/myapp/plexams"]
