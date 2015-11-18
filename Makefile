STACK=stack

GET=stack exec brooksdb get
SET=stack exec brooksdb set

all:
	@cat Makefile

build:
	$(STACK) build

clean:
	$(STACK) clean

demo:
	$(SET) foo bar
	$(GET) foo
	$(SET) baz 5
	$(GET) baz
	$(GET) foo

setup:
	$(STACK) setup

test:
	$(STACK) test

update:
	$(STACK) update

b: build
s: setup
t: test
u: update
