FROM thoughtbot/ghc

RUN cabal update

WORKDIR /src/brooksdb
