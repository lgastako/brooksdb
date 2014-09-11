CABAL=cabal
BINARY=dist/build/brooksdb/brooksdb

SRCS=src/Data/Brooks/Vals.hs \
	 src/Data/Relation/Operators.hs \
	 src/Data/Relation/Types.hs \
	 src/IO/Brooks/Database.hs \
	 src/IO/Brooks/Timothy.hs \
	 src/Main.hs

READLINE_DIR=/usr/local/Cellar/readline/6.2.4
READLINE_INC=$(READLINE_DIR)/include
READLINE_LIB=$(READLINE_DIR)/lib

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

echo-bin:
	@echo $(BINARY)

install-readline:
	$(CABAL) install readline --extra-include-dirs=$(READLINE_INC) \
		--extra-lib-dirs=$(READLINE_LIB) \
		--configure-option=--with-readline-includes=$(READLINE_INC) \
		--configure-option=--with-readline-libraries=$(READLINE_LIB)

repl: $(BINARY)
	$(BINARY) repl

sandbox-init:
	$(CABAL) sandbox init

b: build
cu: cabal-update
cici: cabal-install-cabal-install
d: deps
eb: echo-bin
si: sandbox-init
ir: install-readline
r: run
