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
  gnupg2 \
  dirmngr \
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

ARG GPG_KEY=7784930957807690A66EBDBE3786C5262ECB4A3F
# Use Ubuntu keyserver over HKP to avoid 'No data' errors
RUN gpg --batch --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys "$GPG_KEY"

# install ghcup
RUN \
  curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
  chmod +x /usr/bin/ghcup && \
  ghcup config set gpg-setting GPGStrict

ARG GHC=9.6.7
ARG CABAL=latest

# Install GHC and cabal
RUN \
  ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
  ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL} \
  && ghcup gc -p -s -c -t \
  && rm /usr/local/bin/ghcup

RUN ghc --version && cabal --version

# copy source and build
COPY . /workspaces/caraml
WORKDIR /workspaces/caraml
RUN make test && cabal v2-install --installdir=/usr/local/bin --overwrite-policy=always
ENTRYPOINT ["caraml"]