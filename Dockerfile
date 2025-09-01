FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=America/Los_Angeles

# install system tools & GHC via ghcup
RUN apt-get update && apt-get install -y --no-install-recommends \ 
  ca-certificates \
  build-essential \
  curl \
  libgmp-dev \
  libffi-dev \
  ncurses-dev \
  libnuma-dev \
  zlib1g-dev \
  libgmp10 \
  wget \
  lsb-release \
  software-properties-common \
  apt-transport-https \
  gcc \
  autoconf \
  automake \
  build-essential \
  pkg-config \
  tar \
  git \
  xz-utils \
  llvm-15-dev \
  llvm-15-tools \
  libomp-15-dev \
  libpolly-15-dev \
  libllvm15 \
  && rm -rf /var/lib/apt/lists/* 

# install ghcup
RUN \
  curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
  chmod +x /usr/bin/ghcup

ARG GHC=9.6.7
ARG CABAL=latest

# Install GHC and cabal
RUN \
  ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
  ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL} \
  && ghcup gc -p -s -c -t \
  && rm /usr/bin/ghcup

RUN ghc --version && cabal --version

# copy source and build
COPY . /workspaces/caraml
WORKDIR /workspaces/caraml
RUN make test && cabal v2-install --installdir=/usr/local/bin --overwrite-policy=always
ENTRYPOINT ["caraml"]