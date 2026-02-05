FROM haskell:9.12.2-slim-bookworm AS build

WORKDIR /app

RUN cabal update

COPY gh-actions-docs.cabal ./
RUN cabal build --only-dependencies

COPY src ./src
COPY LICENSE README.md ./

RUN cabal install --overwrite-policy=always --installdir=/root/.cabal/bin gh-actions-docs


FROM haskell:9.12.2-slim-bookworm

WORKDIR /app

COPY --from=build /root/.cabal/bin/gh-actions-docs /usr/local/bin/gh-actions-docs

RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y --no-install-recommends nodejs
RUN npm install -g prettier && npm update -g
RUN export PATH=$PATH:/root/.node/bin

# Fixes CVEs
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

CMD ["gh-actions-docs"]
