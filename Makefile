STACK=stack
ALEX=alex

GET=stack exec brooksdb get
SET=stack exec brooksdb set
LEX=stack exec brooksdb lex

all:
	@cat Makefile

build: gen/Language/Heidi/Lexer.hs
	$(STACK) build

clean:
	$(STACK) clean

demo:
	$(SET) foo bar
	$(GET) foo
	$(SET) baz 5
	$(GET) baz
	$(GET) foo
	$(LEX) d/relation-with.d

gen/Language/Heidi:
	mkdir -p gen/Language/Heidi

gen/Language/Heidi/Lexer.hs: gen/Language/Heidi src/Language/Heidi/Lexer.x
	$(ALEX) src/Language/Heidi/Lexer.x -o gen/Language/Heidi/Lexer.hs

setup:
	$(STACK) setup

test:
	$(STACK) test

update:
	$(STACK) update

b: build
d: demo
s: setup
t: test
u: update

.PHONY: test
