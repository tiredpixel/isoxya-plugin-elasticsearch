#===============================================================================
# FROMFREEZE docker.io/library/debian:10
FROM docker.io/library/debian@sha256:102ab2db1ad671545c0ace25463c4e3c45f9b15e319d3a00a1b2b085293c27fb

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

ENV PATH=${HOME}/bin:$PATH

CMD ["isx-plug-elasticsearch", "-b", "localhost", "-p", "8000"]

EXPOSE 8000

HEALTHCHECK CMD curl -fs http://localhost:8000 || false
#===============================================================================
