module Text.Pandoc2.Parsing.Generic
where
import Text.Parsec
import Control.Applicative ((<$>), (*>))
import Control.Monad (join, guard)

-- | 'sepBy' redefined to include a 'try', so the separator
-- can fail.
sepBy :: Stream s m t
      => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m [a]
sepBy p sep = do
  x <- p
  xs <- many $ try (sep *> p)
  return (x:xs)

upto :: Stream s m t => Int -> ParsecT s u m t -> ParsecT s u m [t]
upto n _ | n <= 0 = return []
upto n p = do
  (p >>= (\x -> (x:) <$> upto (n-1) p)) <|> return []

-- | A more general form of @notFollowedBy@.  This one allows any
-- type of parser to be specified, and succeeds only if that parser
-- fails. It does not consume any input.
notFollowedBy' :: (Stream s m t, Show b)
               => ParsecT s u m b -> ParsecT s u m ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

-- | Like 'manyTill', but parses at least one element.
many1Till :: Stream s m t
          => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till p q = do
  x <- p
  xs <- manyTill p q
  return (x:xs)

-- | Fail unless in column 1.
pInColumn1 :: Stream s m t
           => ParsecT s u m ()
pInColumn1 = do
  pos <- getPosition
  guard $ sourceColumn pos == 1

