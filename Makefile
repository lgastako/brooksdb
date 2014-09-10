CABAL=cabal
BINARY=dist/build/brooksdb/brooksdb

SRCS=src/CurQuery.hs \
	 src/Data/Brooks/Vals.hs \
	 src/Data/Relation/Operators.hs \
	 src/Data/Relation/Types.hs \
	 src/IO/Brooks/Database.hs \
	 src/IO/Brooks/Timothy.hs \
	 src/Main.hs

GET=$(BINARY) get
PUT=$(BINARY) put

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

demo: $(BINARY)
	$(PUT) foo bar
	$(GET) foo
	$(PUT) baz 5
	$(GET) baz
	$(GET) foo

deps:
	$(CABAL) install --only-dependencies

# run: $(BINARY)
# 	./$(BINARY)

repl: $(BINARY)
	$(BINARY) repl

sandbox-init:
	$(CABAL) sandbox init

b: build
cu: cabal-update
cici: cabal-install-cabal-install
d: deps
si: sandbox-init
r: run
