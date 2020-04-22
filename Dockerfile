#===============================================================================
# FROMFREEZE haskell:8.6.5
FROM haskell@sha256:c56bb7769a1404431629dfe3d9a99e7a57f8550cf4b1e4c5057ecb71453c67c1

ARG USER=x
ARG HOME=/home/x
#-------------------------------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        daemontools \
        ghc-8.6.5-prof \
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

CMD cabal v1-run isx-pipe-elasticsearch -- -b ${ADDRESS} -p ${PORT}

EXPOSE ${PORT}

HEALTHCHECK CMD curl -fs http://${ADDRESS}:${PORT} || false
#===============================================================================
