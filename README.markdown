What is this?
-------------

I started working on pandoc before I knew much Haskell, and before
there were many Haskell libraries available.  In retrospect, I regret
some of the early design decisions.  This repository is a place to
explore some architectural improvements.

So far, there's just a definition of the basic data structure, a
builder DSL, a markdown reader, and an HTML writer.  The package
includes an executable, markdown, that behaves just like the original
Markdown.pl, without any pandoc extensions.  This program passes all
of the tests from the Markdown test suite.

Some differences from pandoc 1
------------------------------

* We now use `Sequence`s of `Inline` and `Block` elements instead of lists.
  This makes sense for text, since appending to the end of a `Sequence`
  is computationally cheap. These sequences are wrapped in newtypes, `Inlines`
  and `Blocks`. Thus, the `Emph` constructor now has the type
  `Inlines -> Inline` rather than `[Inline] -> Inline`.
  `mappend` is defined for `Inlines` in a way that builds in normalization:
  so, for example, if you append an `Inlines` that begins with a space onto an
  `Inlines` that ends with a space, there will only be one space. Similarly,
  adjacent `Emph` `Inline`s will be merged, and so on.

* The individual inline and block parsers return an `Inlines` or `Blocks`
  instead of an `Inline` or `Block`; this allows them to return nothing, or
  multiple elements, where before we had to return a single elements. (So,
  for example, `pReference` can return `mempty` instead of a `Null` block.)

* `Text` is used throughout instead of `String`.

* IO actions are now possible in the parsers.  This should make it
  possible to handle things like LaTeX `\include`.

* It is also now easy to issue warnings and informational messages
  during parsing, to alert the user if information is being lost,
  for example.

* The old markdown parser made two passes--one to get a list of
  references, and then again to parse the document, using this
  list of references.  The new parser makes just one pass,
  and uses generics (from uniplate rather than syb, for speed)
  to walk the AST and fill in the refereces at the end.

* The old parser handled embedded blocks (block quotations,
  sublists) by first parsing out a "raw" chunk of text (omitting
  opening `>`'s and indentation, for example), then parsing this
  raw text using block parsers.  The new parser avoids the need
  for multiple passes by storing an "endline" and "block separator"
  parser in state.

* The old parser required space after block elements, so that
  newlines would generally have to be added to the input.  The
  new parser does not.

* blaze-html is now used (instead of the old xhtml package) for HTML
  generation.

Observations
------------

The code is cleaner (though more abstractions can be made) and shorter.

Performance is, surprisingly, a bit slower than `pandoc --strict` (though
much faster than `pandoc --strict --normalize`). I suspect there are more
optimizations possible and would welcome suggestions. `resolveRefs` is slow,
perhaps because of the generics, and might be improved by a custom Uniplate
instance. Perhaps it would be worth writing new versions of the parsec
combinators (`many`, `manyTill`, etc.) that can directly return `Sequence` and
`Text`.

I have included code to convert tabs to spaces, mainly for running
the Markdown test suite, but this isn't necessary; the parser can handle plain
tabs.

To run the Markdown test suite, do `make test`. To run the PHP Markdown test
suite, do `make phptests`. Several of the PHP tests fail, but in many of these
cases I disagree about what behavior is normative. (For example, I don't think
a `#` character at the beginning of a line inside a paragraph should start a
header.)
