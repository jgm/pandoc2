OPTS=-O2 -Wall -fno-warn-unused-do-bind
PROFOPTS=-prof -auto-all -caf-all -fforce-recomp -rtsopts
SOURCES=markdown.hs Pandoc.hs Parser.hs Definition.hs Builder.hs HTML.hs

markdown: ${SOURCES}
	ghc --make ${OPTS} -o $@ $<

markdown-prof: ${SOURCES}
	ghc --make ${PROFOPTS} -o $@ $<


.PHONY: test clean

clean:
	rm markdown markdown-prof *.o *.hi

test:
	cd Tests && \
	for d in Tests*; do \
		perl MarkdownTest.pl --testdir=$$d -s ../markdown --tidy ;\
	done
