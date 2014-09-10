CABAL=cabal
BINARY=dist/build/brooksdb/brooksdb

SRCS=src/CurQuery.hs \
	 src/Data/Brooks/Vals.hs \
	 src/Data/Relation/Operators.hs \
	 src/Data/Relation/Types.hs \
	 src/IO/Brooks/Database.hs \
	 src/IO/Brooks/Timothy.hs \
	 src/Main.hs

all:
	@cat Makefile

$(BINARY): $(SRCS)
	$(CABAL) build

build: $(BINARY)

cabal-update:
	$(CABAL) update

cabal-install-cabal-install:
	$(CABAL) install cabal-install

clean:
	$(CABAL) clean

deps:
	$(CABAL) install --only-dependencies

run: $(BINARY)
	./$(BINARY)

sandbox-init:
	$(CABAL) sandbox init

b: build
cu: cabal-update
cici: cabal-install-cabal-install
d: deps
si: sandbox-init
r: run
