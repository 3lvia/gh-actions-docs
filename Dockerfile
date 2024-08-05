FROM haskell:9.4.8-slim

WORKDIR /opt/app

RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y --no-install-recommends nodejs
RUN npm install -g prettier
RUN export PATH=$PATH:/root/.node/bin

RUN cabal update

COPY gh-actions-docs.cabal ./
RUN cabal build --only-dependencies

COPY src ./src
COPY LICENSE README.md ./

RUN cabal install --overwrite-policy=always

# CVE-2024-32002
RUN apt-get update && \
    apt-get install -y --no-install-recommends git git-man

ENTRYPOINT ["gh-actions-docs"]
