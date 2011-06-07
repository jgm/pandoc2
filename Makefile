.PHONY: all opt test clean prof all markdowntests phptests
PANDOC2=$(shell pwd)/dist/build/pandoc2/pandoc2

all:
	cabal install

prof:
	cabal install --enable-library-profiling --enable-executable-profiling

clean:
	cabal clean

test:
	perl shtest.pl -c -w "$(PANDOC2)" '^tests/(Markdown_1.0.3|Pandoc)'

phptests:
	perl shtest.pl -c -w "$(PANDOC2)" ^tests/PHP_Markdown
