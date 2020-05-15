#===============================================================================
# FROMFREEZE docker.io/library/debian:10
FROM docker.io/library/debian@sha256:87c8111d19bb221842fd8e48195782c987714f4e8b43b31a6cd5af090fc6b3ee

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
COPY .version ${HOME}/.version
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
