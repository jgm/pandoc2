markdown: markdown.hs Pandoc.hs Parser.hs Definition.hs HTML.hs
	ghc --make -o $@ $<

.PHONY: test
test:
	cd MarkdownTest_1.0.3 && \
	perl MarkdownTest.pl -s ../markdown --tidy
