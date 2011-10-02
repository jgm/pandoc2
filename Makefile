.PHONY: all opt test bench clean prof all markdowntests phptests
PANDOC2=$(shell pwd)/dist/build/pandoc2/pandoc2
PROG?=${PANDOC2}

all:
	cabal install

prof:
	cabal install --enable-library-profiling --enable-executable-profiling

clean:
	cabal clean

test:
	lua shtest.lua -p "$(PANDOC2)" 'tests/Markdown_1.0.3' 'tests/Pandoc'

phptests:
	lua shtest.lua -p "$(PANDOC2)" ^tests/PHP_Markdown

bench:
	time ${PROG} benchtext.txt >/dev/null

