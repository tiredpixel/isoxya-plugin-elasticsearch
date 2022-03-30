# FROMFREEZE docker.io/library/haskell:8.10
FROM docker.io/library/haskell@sha256:72c3d68618aeacc0df381b1ad650221c9e599bcbe017f33030a15c4b28c4f919

ARG USER=x
ARG HOME=/home/x
#-------------------------------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        daemontools \
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
    "cabal.project.freeze", \
    "*.cabal", \
    "./"]

RUN cabal update && \
    cabal build --only-dependencies --enable-tests
#-------------------------------------------------------------------------------
ENV PATH=${HOME}/.cabal/bin:$PATH \
    LANG=C.UTF-8

CMD ["cabal", "run", "isoxya-plugin-elasticsearch", "--", \
    "-b", "0.0.0.0", "-p", "80"]

EXPOSE 80

HEALTHCHECK CMD curl -fs http://localhost || false
