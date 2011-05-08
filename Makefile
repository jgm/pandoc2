OPTS=-rtsopts

markdown: markdown.hs Pandoc.hs Parser.hs Definition.hs HTML.hs
	ghc --make ${OPTS} -o $@ $<

markdown-prof: markdown.hs Pandoc.hs Parser.hs Definition.hs HTML.hs
	ghc --make -prof -auto-all -caf-all ${OPTS} -o $@ $<


.PHONY: test clean

clean:
	rm markdown *.o *.hi

test:
	cd MarkdownTest_1.0.3 && \
	perl MarkdownTest.pl -s ../markdown --tidy
