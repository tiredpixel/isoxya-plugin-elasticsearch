#===============================================================================
# FROMFREEZE docker.io/library/debian:10
FROM docker.io/library/debian@sha256:5b5fa7e155b1f19dffb996ea64e55520b80d5bd7a8fdb5aed1acabd217e9ed59

ARG USER=x
ARG HOME=/home/x
#-------------------------------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl && \
    rm -rf /var/lib/apt/lists/*

RUN useradd ${USER} -d ${HOME} && \
    mkdir -p ${HOME}/bin && \
    chown -R ${USER}:${USER} ${HOME}
#-------------------------------------------------------------------------------
COPY bin/ ${HOME}/bin/
#-------------------------------------------------------------------------------
USER ${USER}

WORKDIR ${HOME}

ENV ADDRESS=localhost \
    PATH=${HOME}/bin:$PATH \
    PORT=8000

CMD isx-plug-elasticsearch -b ${ADDRESS} -p ${PORT}

EXPOSE ${PORT}

HEALTHCHECK CMD curl -fs http://${ADDRESS}:${PORT} || false
#===============================================================================
