FROM ubuntu:24.04

ARG GHC_VERSION=9.6.7

# install system tools & GHC via ghcup
RUN apt-get update && apt-get install -y --no-install-recommends \ 
  ca-certificates \
  build-essential \
  curl \
  libgmp-dev \
  libffi-dev \
  ncurses-dev \
  pkg-config \
  tar \
  git \
  xz-utils \
  llvm-15-dev \
  llvm-15-tools \
  libomp-15-dev \
  libpolly-15-dev \
  libllvm15 \
  && rm -rf /var/lib/apt/lists/* \
  # Install ghcup and GHC
  && curl --fail --output /usr/local/bin/ghcup \
  https://downloads.haskell.org/ghcup/x86_64-linux-ghcup \
  && chmod 0755 /usr/local/bin/ghcup \
  && ghcup install cabal --isolate /usr/local/bin \
  && mkdir /usr/local/opt \
  && ghcup install ghc ${GHC_VERSION} --isolate /usr/local/opt/ghc-${GHC_VERSION} \
  && find /usr/local/opt/ghc-${GHC_VERSION}/bin -type f -exec ln -s {} /usr/local/bin \; \
  && find /usr/local/opt/ghc-${GHC_VERSION}/lib -type f \( -name '*_p.a' -o -name '*.p_hi' \) -delete \
  && rm -rf /usr/local/opt/ghc-${GHC_VERSION}/share \
  && ghcup gc -p -s -c -t \
  && rm /usr/local/bin/ghcup

RUN ghc --version && cabal --version

# copy source and build
COPY . /workspaces/caraml
WORKDIR /workspaces/caraml
RUN make test && cabal v2-install --installdir=/usr/local/bin --overwrite-policy=always
ENTRYPOINT ["caraml"]