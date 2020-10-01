#===============================================================================
# FROMFREEZE docker.io/library/debian:10
FROM docker.io/library/debian@sha256:2f53b5f0101a2f296c632c28010df43575b8ced96615b4b0b370d52a7004f473

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

CMD isx-plugin-elasticsearch -b ${ADDRESS} -p ${PORT}

EXPOSE ${PORT}

HEALTHCHECK CMD curl -fs http://${ADDRESS}:${PORT} || false
#===============================================================================
