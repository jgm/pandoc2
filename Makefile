markdown: markdown.hs Pandoc.hs Parser.hs Definition.hs HTML.hs
	ghc --make -o $@ $<
