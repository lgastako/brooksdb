STACK=stack
BINARY=dist/build/brooksdb/brooksdb

SRCS=$(sort $(dir $(wildcard src/*/)))

READLINE_VERSION=6.3.8
READLINE_DIR=/usr/local/Cellar/readline/$(READLINE_VERSION)
READLINE_INC=$(READLINE_DIR)/include
READLINE_LIB=$(READLINE_DIR)/lib

# GET=$(BINARY) get
# PUT=$(BINARY) put

GET=stack exec brooksdb get
SET=stack exec brooksdb set

all:
	@cat Makefile

$(BINARY): $(SRCS)
	$(STACK) build

build: $(BINARY)
	$(STACK) build

clean:
	$(STACK) clean

demo:  # $(BINARY)
	$(SET) foo bar
	$(GET) foo
	$(SET) baz 5
	$(GET) baz
	$(GET) foo

# install-readline:
# 	   $(CABAL) install readline                                         \
# 				--extra-include-dirs=$(READLINE_INC)                      \
# 				--extra-lib-dirs=$(READLINE_LIB)                           \
# 				--configure-option=--with-readline-includes=$(READLINE_INC) \
# 				--configure-option=--with-readline-libraries=$(READLINE_LIB)

install-readline:
	   $(STACK) install readline                                        \
			   --extra-include-dirs=$(READLINE_INC)                      \
			   --extra-lib-dirs=$(READLINE_LIB)                           \
			   --configure-option=--with-readline-includes=$(READLINE_INC) \
			   --configure-option=--with-readline-libraries=$(READLINE_LIB)

repl: $(BINARY)
	stack exec brooksdb repl
# $(BINARY) repl

setup:
	$(STACK) setup

test: $(BINARY)
	$(STACK) test

update:
	$(STACK) update

b: build
ir: install-readline
r: repl
s: setup
t: test
u: update
