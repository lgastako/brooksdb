CABAL=cabal

all:
	@cat Makefile

build:
	$(CABAL) build

cabal-update:
	$(CABAL) update

cabal-install-cabal-install:
	$(CABAL) install cabal-install

clean:
	$(CABAL) clean

deps:
	$(CABAL) install --only-dependencies

sandbox-init:
	$(CABAL) sandbox init

b: build
cu: cabal-update
cici: cabal-install-cabal-install
d: deps
si: sandbox-init
