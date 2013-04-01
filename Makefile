CABAL=cabal-dev

all:
	@echo "c:lean"
	@echo "cfg: configure"
	@echo "b:uild"
	@echo "h:addock"
	@echo "i:install"
	@echo "id: install-deps"
	@echo "t:est"
	@echo "f:ull = clean + configure + build + install + test"
	@echo "r:un"
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
	@#dist/build/test-brooksdb/test-brooksdb
	@dist/build/tests/tests-brooksdb

run:
	@cabal-dev/bin/brooksdb

lint:
	hlint .

ghci:
	# If you are not using cabal-dev, remove the $(CABAL).
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
r: run
l: lint
g: ghci

