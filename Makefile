OPTS=-rtsopts

markdown: markdown.hs Pandoc.hs Parser.hs Definition.hs HTML.hs
	ghc --make ${OPTS} -o $@ $<

markdown-prof: markdown.hs Pandoc.hs Parser.hs Definition.hs HTML.hs
	ghc --make -prof -auto-all -caf-all -fforce-recomp ${OPTS} -o $@ $<


.PHONY: test clean

clean:
	rm markdown markdown-prof *.o *.hi

test:
	cd MarkdownTest_1.0.3 && \
	perl MarkdownTest.pl -s ../markdown --tidy
