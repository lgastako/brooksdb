CABAL=cabal-dev
RLWRAP=rlwrap

all:
	@echo "c:lean"
	@echo "cfg: configure"
	@echo "b:uild"
	@echo "h:addock"
	@echo "i:install"
	@echo "id: install-deps"
	@echo "t:est"
	@echo "f:ull = clean + configure + build + install + test"
	@echo "d:ist"
	@echo "r:epl"
	@echo "l:int"
	@echo "g:hci"
	@echo ""

clean:
	$(CABAL) clean

configure:
	$(CABAL) configure --enable-tests

build:
	$(CABAL) build

haddock:
	$(CABAL) haddock

install:
	$(CABAL) install

install-deps:
	$(CABAL) install --only-dependencies --enable-tests

test:
	@# $(CABAL) test
	@#dist/build/tests/tests
	@echo NO TESTS RIGHT NOW. COME BACK LATER.

dist:
	$(CABAL) sdist

repl:
	@$(RLWRAP) cabal-dev/bin/brooksdb repl

lint:
	hlint .

ghci:
	@# If you are not using cabal-dev, remove the $(CABAL).
	$(CABAL) ghci

full: clean configure build install test

# aliases
a: all

c: clean
cfg: configure
b: build
h: haddock
t: test
i: install
id: install-deps
f: full
d: dist
r: repl
l: lint
g: ghci

