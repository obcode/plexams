FROM fpco/stack-build:lts-13.19 as builder
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc -j1

FROM ubuntu:16.04
RUN mkdir -p /opt/myapp
WORKDIR /opt/myapp
ARG BINARY_PATH
RUN mkdir -p /semester
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
# NOTICE THIS LINE
COPY --from=builder /root/.local/bin/plexams .
COPY --from=builder /root/.local/bin/plexams-helper .
COPY --from=builder /root/.local/bin/plexams-server .
WORKDIR /semester
CMD ["/opt/myapp/plexams-server"]
