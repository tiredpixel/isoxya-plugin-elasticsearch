#===============================================================================
# FROMFREEZE docker.io/library/debian:9
FROM docker.io/library/debian@sha256:666ffd4fbcdff07edcbd65f0ec1dc86ed294f5e3be93eb26280575f77427df46

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
