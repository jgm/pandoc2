.PHONY: all opt test clean prof all markdowntests phptests

all:
	cabal install -ffastcompile

opt:
	cabal install -f-fastcompile

prof:
	cabal install --enable-library-profiling --enable-executable-profiling

clean:
	cabal clean

test: markdowntests

markdowntests:
	cd Tests && \
	perl MarkdownTest.pl --testdir=Tests_Markdown_1.0.3 -s ./markdown --tidy

phptests:
	cd Tests && \
	perl MarkdownTest.pl --testdir=Tests_PHP_Markdown -s ./markdown  --tidy

