CABAL=cabal

all:
	@cat Makefile

build:
	$(CABAL) build

deps:
	$(CABAL) install --only-dependencies

sandbox-init:
	$(CABAL) sandbox init

si: sandbox-init
