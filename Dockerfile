FROM docker.io/library/haskell@sha256:aaa408ad7e7eff6cd76c39feae223db7ba3550b937e833e78958478334fc9afe AS builder

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        jq \
        libpcre3-dev && \
    rm -rf /var/lib/apt/lists/*

RUN useradd x -m && \
    mkdir /home/x/plugin-elasticsearch && \
    chown -R x:x /home/x
#-------------------------------------------------------------------------------
USER x

WORKDIR /home/x/plugin-elasticsearch

COPY --chown=x:x ["*.cabal", "cabal.project.freeze", "./"]

RUN cabal update && \
    cabal build --only-dependencies --enable-tests

COPY --chown=x:x . .

RUN cabal install -O2
#-------------------------------------------------------------------------------
ENV PATH=/home/x/plugin-elasticsearch/bin:/home/x/.cabal/bin:$PATH \
    LANG=C.UTF-8

CMD ["cabal", "run", "isoxya-plugin-elasticsearch", "--", \
    "-b", "0.0.0.0", "-p", "80"]

EXPOSE 80

HEALTHCHECK CMD curl -fs http://localhost || false
#===============================================================================
FROM docker.io/library/debian@sha256:ebe4b9831fb22dfa778de4ffcb8ea0ad69b5d782d4e86cab14cc1fded5d8e761

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl && \
    rm -rf /var/lib/apt/lists/*

RUN useradd x -m && \
    mkdir /home/x/bin && \
    chown -R x:x /home/x
#-------------------------------------------------------------------------------
COPY --from=builder /home/x/.cabal/bin/* /home/x/bin/
#-------------------------------------------------------------------------------
USER x

WORKDIR /home/x

ENV PATH=/home/x/bin:$PATH \
    LANG=C.UTF-8

CMD ["isoxya-plugin-elasticsearch", "-b", "0.0.0.0", "-p", "80"]

EXPOSE 80

HEALTHCHECK CMD curl -fs http://localhost || false
