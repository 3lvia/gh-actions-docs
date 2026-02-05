FROM haskell:9.12.2-slim-bookworm AS build

WORKDIR /app

RUN curl -fsSL https://deb.nodesource.com/setup_24.x | bash - && \
    apt-get install -y --no-install-recommends nodejs
RUN npm install -g prettier && npm update -g
RUN export PATH=$PATH:/root/.node/bin

RUN cabal update

COPY gh-actions-docs.cabal ./
RUN cabal build --only-dependencies

COPY src ./src
COPY LICENSE README.md ./

RUN cabal install --overwrite-policy=always

# Fixes CVEs
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["gh-actions-docs"]
