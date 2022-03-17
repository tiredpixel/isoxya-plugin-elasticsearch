# FROMFREEZE docker.io/library/debian:10
FROM docker.io/library/debian@sha256:eb1a42a44a67879373f6d4c3429bb4cea60217ff74355bd620afbf6456d1ae4a

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

ENV PATH=${HOME}/bin:$PATH \
    LANG=C.UTF-8

CMD ["isoxya-plugin-elasticsearch", "-b", "0.0.0.0", "-p", "80"]

EXPOSE 80

HEALTHCHECK CMD curl -fs http://localhost || false
