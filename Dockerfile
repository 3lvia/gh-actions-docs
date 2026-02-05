FROM haskell:9.12.2-slim-bookworm AS build

WORKDIR /app

RUN cabal update

COPY gh-actions-docs.cabal ./
RUN cabal build --only-dependencies

COPY src ./src
COPY LICENSE README.md ./

RUN cabal install --overwrite-policy=always --installdir=/app


FROM node:24-bookworm-slim

WORKDIR /app

RUN apt-get update && apt-get upgrade -y
RUN npm update -g && npm install -g prettier

COPY --from=build /app/gh-actions-docs /usr/local/bin/gh-actions-docs

RUN chmod +x /usr/local/bin/gh-actions-docs

ENTRYPOINT ["gh-actions-docs"]
