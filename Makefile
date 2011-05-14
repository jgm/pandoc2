OPTS=-O2 -Wall -fno-warn-unused-do-bind
PROFOPTS=-prof -auto-all -caf-all -fforce-recomp -rtsopts

markdown:
	cabal install

markdown-prof: ${SOURCES}
	cabal install --enable-library-profiling --enable-executable-profiling

.PHONY: test clean

clean:
	cabal clean

test: markdowntests

markdowntests:
	cd Tests && \
	perl MarkdownTest.pl --testdir=Tests_Markdown_1.0.3 -s ./markdown --tidy

phptests:
	cd Tests && \
	perl MarkdownTest.pl --testdir=Tests_PHP_Markdown -s ./markdown  --tidy

