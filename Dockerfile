FROM haskell:9.12.2-slim-bookworm AS build

WORKDIR /app

RUN cabal update

COPY gh-actions-docs.cabal ./
RUN cabal build --only-dependencies

COPY src ./src
COPY LICENSE README.md ./

RUN cabal build --enable-executable-static && \
    mkdir out && \
    cp $(cabal list-bin exe:gh-actions-docs) out/


FROM debian:bookworm-slim

WORKDIR /app

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install curl -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /app/out/gh-actions-docs /usr/local/bin/gh-actions-docs

RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y --no-install-recommends nodejs
RUN npm install -g prettier && npm update -g
RUN export PATH=$PATH:/root/.node/bin

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

ENTRYPOINT ["gh-actions-docs"]
