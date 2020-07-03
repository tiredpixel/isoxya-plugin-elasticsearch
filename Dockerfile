#===============================================================================
# FROMFREEZE docker.io/library/haskell:8.8.3
FROM docker.io/library/haskell@sha256:79bfdabc69816aa4cfa3ffd416d1425495610495c1c9eca57a54038211ca9746

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
    mkdir -p ${HOME}/repo && \
    chown -R ${USER}:${USER} ${HOME}
#-------------------------------------------------------------------------------
USER ${USER}

WORKDIR ${HOME}/repo

ENV PATH ${HOME}/.cabal/bin:$PATH

COPY --chown=x:x [ \
    "cabal.config", \
    "*.cabal", \
    "./"]

RUN cabal v1-update && \
    cabal v1-install -j --only-dependencies --enable-tests
#-------------------------------------------------------------------------------
COPY --chown=x:x . .
#-------------------------------------------------------------------------------
ENV ADDRESS=0.0.0.0 \
    PORT=8000

CMD cabal v1-run isx-plugin-elasticsearch -- -b ${ADDRESS} -p ${PORT}

EXPOSE ${PORT}

HEALTHCHECK CMD curl -fs http://${ADDRESS}:${PORT} || false
#===============================================================================
