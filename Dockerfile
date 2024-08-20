FROM haskell:9.6-slim-bullseye

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

# Fixes CVEs
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
      e2fsprogs \
      libcom-err2 \
      libsqlite3-0 \
      libss2 \
      logsave \
      libnghttp2-14 \
      linux-libc-dev

ENTRYPOINT ["gh-actions-docs"]
