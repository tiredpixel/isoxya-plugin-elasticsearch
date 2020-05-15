#===============================================================================
# FROMFREEZE docker.io/library/haskell:8.8.3
FROM docker.io/library/haskell@sha256:850c8bb4fd924861d6355d2ce3e43960ff63a5ad0b30e5f81a21df76eb6b9c97

ARG USER=x
ARG HOME=/home/x
#-------------------------------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        daemontools \
        ghc-8.8.3-prof \
        happy \
        hlint \
        jq \
        libpcre3-dev && \
    rm -rf /var/lib/apt/lists/*

RUN useradd ${USER} -d ${HOME} && \
    mkdir -p ${HOME}/src && \
    chown -R ${USER}:${USER} ${HOME}
#-------------------------------------------------------------------------------
USER ${USER}

WORKDIR ${HOME}/src

ENV \
    PATH=${HOME}/.cabal/bin:$PATH

COPY [ \
    "cabal.config", \
    "*.cabal", \
    "./"]

RUN cabal v1-update && \
    cabal v1-install -j --only-dependencies --enable-tests
#-------------------------------------------------------------------------------
COPY . .
#-------------------------------------------------------------------------------
ENV ADDRESS=localhost \
    PORT=8000

CMD cabal v1-run isx-plugin-elasticsearch -- -b ${ADDRESS} -p ${PORT}

EXPOSE ${PORT}

HEALTHCHECK CMD curl -fs http://${ADDRESS}:${PORT} || false
#===============================================================================
