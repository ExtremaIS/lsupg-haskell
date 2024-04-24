FROM alpine:latest

ARG GHC_VERSION=8.6.5

RUN apk upgrade --no-cache \
 && apk add --no-cache \
      bash \
      binutils-gold \
      curl \
      g++ \
      gcc \
      gmp-dev \
      libc-dev \
      libffi-dev \
      make \
      musl-dev \
      ncurses-dev \
      perl \
      pkgconfig \
      shadow \
      tar \
      xz \
 && curl --fail --output '/usr/local/bin/ghcup' \
      'https://downloads.haskell.org/ghcup/x86_64-linux-ghcup' \
 && chmod 0755 '/usr/local/bin/ghcup' \
 && ghcup install cabal --isolate '/usr/local/bin' \
 && mkdir '/usr/local/opt' \
 && ghcup install ghc "${GHC_VERSION}" \
      --isolate "/usr/local/opt/ghc-${GHC_VERSION}" \
 && find "/usr/local/opt/ghc-${GHC_VERSION}/bin" \
      \( -type f -o -type l \) -exec ln -s {} '/usr/local/bin' \; \
 && find "/usr/local/opt/ghc-${GHC_VERSION}/lib" \
      -type f \( -name '*_p.a' -o -name '*.p_hi' \) -delete \
 && rm -rf "/usr/local/opt/ghc-${GHC_VERSION}/share" \
 && ghcup gc -p -s -c -t \
 && rm '/usr/local/bin/ghcup'

CMD ["bash"]
