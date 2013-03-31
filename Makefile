CABAL=cabal-dev

all:
	@echo "c:lean"
	@echo "cfg: configure"
	@echo "b:uild"
	@echo "i:install"
	@echo "t:est"
	@echo "f:ull = clean + configure + build + install + test"
	@echo "g:hci"
	@echo ""

clean:
	$(CABAL) clean

configure:
	$(CABAL) configure --enable-tests

build:
	$(CABAL) build

install:
	$(CABAL) install

test:
	@# $(CABAL) test
	dist/build/test-brooksdb/test-brooksdb

ghci:
	# If you are not using cabal-dev, remove the $(CABAL).
	$(CABAL) ghci

full: clean configure build install test

# aliases
a: all

c: clean
cfg: configure
b: build
t: test
i: install
f: full
g: ghci

