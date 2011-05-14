.PHONY: all opt test clean prof all markdowntests phptests

all:
	cabal install -ffastcompile

opt:
	cabal install -f-fastcompile

prof:
	cabal install --enable-library-profiling --enable-executable-profiling

clean:
	cabal clean

test: markdowntests pandoctests

markdowntests:
	cd Tests && \
	OPTS="--strict --smart=no" perl MarkdownTest.pl --testdir=Markdown_1.0.3 -s ./markdown --tidy

phptests:
	cd Tests && \
	OPTS="--strict --smart=no" perl MarkdownTest.pl --testdir=PHP_Markdown -s ./markdown  --tidy

pandoctests:
	cd Tests && \
	OPTS="--smart=yes" perl MarkdownTest.pl --testdir=Pandoc -s ./markdown  --tidy

