OPTS=-O2 -Wall -fno-warn-unused-do-bind
PROFOPTS=-prof -auto-all -caf-all -fforce-recomp -rtsopts

markdown:
	cabal install

markdown-prof: ${SOURCES}
	cabal install --enable-library-profiling --enable-executable-profiling

.PHONY: test clean

clean:
	cabal clean

test:
	cd Tests && \
	for d in Tests*; do \
		perl MarkdownTest.pl --testdir=$$d -s ../dist/build/markdown/markdown --tidy ;\
	done
