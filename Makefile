.PHONY: all opt test clean prof all markdowntests phptests
PANDOC2=$(shell pwd)/dist/build/pandoc2/pandoc2

all:
	cabal install

prof:
	cabal install --enable-library-profiling --enable-executable-profiling

clean:
	cabal clean

test: markdowntests shelltests

markdowntests:
	cd Tests && \
	OPTS="--strict --smart=no" perl MarkdownTest.pl --testdir=Markdown_1.0.3 -s ./markdown --tidy

phptests:
	cd Tests && \
	OPTS="--strict --smart=no" perl MarkdownTest.pl --testdir=PHP_Markdown -s ./markdown  --tidy

shelltests:
	perl shtest.pl -w "$(PANDOC2)" Tests
