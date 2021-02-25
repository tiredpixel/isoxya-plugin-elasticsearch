#===============================================================================
# FROMFREEZE docker.io/library/haskell:8.8
FROM docker.io/library/haskell@sha256:0f62d7e62181a26235844f00c73a6afb76bee8f8b777883101e2070ec47454ca

ARG USER=x
ARG HOME=/home/x
#-------------------------------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        daemontools \
        happy \
        hlint \
        jq \
        libpcre3-dev && \
    rm -rf /var/lib/apt/lists/*

RUN useradd ${USER} -d ${HOME} && \
    mkdir -p ${HOME}/repo && \
    chown -R ${USER}:${USER} ${HOME}
#-------------------------------------------------------------------------------
USER ${USER}

WORKDIR ${HOME}/repo

COPY --chown=x:x [ \
    "cabal.config", \
    "*.cabal", \
    "./"]

RUN cabal v1-update && \
    cabal v1-install -j --only-dependencies --enable-tests
#-------------------------------------------------------------------------------
ENV PATH=${HOME}/.cabal/bin:$PATH \
    ADDRESS=0.0.0.0 \
    PORT=8000

CMD cabal v1-run isx-plug-elasticsearch -- -b ${ADDRESS} -p ${PORT}

EXPOSE ${PORT}

HEALTHCHECK CMD curl -fs http://${ADDRESS}:${PORT} || false
#===============================================================================
