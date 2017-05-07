{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- !!! NOTE: This module has been superceded by Oracle.Phonopy.BandYaml.Preprocessed !!!
-- (that's right, this still isn't efficient enough!)

module Phonopy.BandYaml.LL1 where

import           "exphp-prelude" ExpHPrelude hiding (sequence, sequence_)
import           "base" Data.Complex
import           "base" Debug.Trace
import           "base" Data.Functor
import           "base" Data.Function((&))
import qualified "base" Data.List as List
import           "base" Control.Applicative
import qualified "attoparsec" Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified "vector" Data.Vector as Vector
import           "yaml" Text.Libyaml(Event(..))
import qualified "yaml" Text.Libyaml as Libyaml
import           "bytestring" Data.ByteString(ByteString)
import qualified "bytestring" Data.ByteString.Char8 as ByteString
import           "conduit" Data.Conduit -- for... ResourceT? Okay.
import           "conduit-combinators" Conduit


-- NOTE: I just received my answer that INLINABLE pragmas have no effect on uses within
--       a module, right *after* I had finished adding them to every single function in
--       existence.
--
--       ...I think they'll stay for now.

readBandYamlKets :: FilePath -> IO (Vector (Vector (UVector (Complex Double))))
readBandYamlKets fp =
    runResourceT $
    runConduitRes $
    Libyaml.decodeFile fp
    .| runParser bandYaml
{-# INLINABLE readBandYamlKets #-}

bandYaml_ :: (Monad m)=> EventParser m ()
bandYaml_ = stream_
{-# INLINABLE bandYaml_ #-}

bandYaml :: (MonadIO m)=> EventParser m (Vector (Vector (UVector (Complex Double))))
bandYaml = named "bandYaml" $
    ket                                -- EventParser m (Vector (Complex Double))
    & limitMappingValue "eigenvector"  -- EventParser m (Vector (Complex Double))
    & (<< liftIO (putStrLn "eigenvector"))
    & (>>= (pure $!!))
    & vector                           -- EventParser m (Vector (Vector (Complex Double)))
    & limitMappingValue "band"         -- EventParser m (Vector (Vector (Complex Double)))
    & (<< liftIO (putStrLn "band"))
    & vector                           -- EventParser m (Vector (Vector (Vector (Complex Double))))
    & (>>= (pure $!!))
    & limitMappingValue "phonon"       -- EventParser m (Vector (Vector (Vector (Complex Double))))
    & bracketDocument
    & bracketStream
{-# INLINABLE bandYaml #-}

ket :: (Monad m)=> EventParser m (UVector (Complex Double))
ket = named "ket" $
    parseTupleWith2 (:+) scalarFloat scalarFloat -- EventParser m (Complex Double)
    & vector         -- each coordinate...       -- EventParser m (Vector (Complex Double))
    & (>>= (pure $!!))
    & vector         -- each atom...             -- EventParser m (Vector (Vector (Complex Double)))
    & (>>= (pure $!!))
    & fmap (>>= id)  -- ...as 3N coordinates     -- EventParser m (Vector (Complex Double))
    & fmap Vector.convert
    & (>>= (pure $!!))
{-# INLINABLE ket #-}

vector :: (Monad m)=> EventParser m a -> EventParser m (Vector a)
vector p = named "vector" $
    fmap (Vector.fromList . List.reverse) $ foldl'Sequence (flip (:)) [] p
{-# INLINABLE vector #-}

------------------------

-- An LL(1) parser
data Parser i m a = Parser
    { parserApplicable :: (Maybe i -> Bool)
    , parserSink       :: Sink i m a
    } deriving (Functor)

type EventParser m a = Parser Event m a

-- | Type of a parser that skips something.
type Ignore = forall m. (Monad m)=> Parser Event m ()

-- | Type of unary functions that take a "continuation parser" and supply it
--    with a modified stream of events.
type ParserCont = forall m a. (Monad m)=> Parser Event m a -> Parser Event m a

infixl 4 <<
(<<) :: Applicative m => m a -> m b -> m a
(<<) = (<*)
{-# INLINABLE (<<) #-}

instance (MonadIO m)=> MonadIO (Parser i m) where
    liftIO io = Parser (const True) (liftIO io)
    {-# INLINABLE liftIO #-}

runParser :: Parser i m a -> Sink i m a
runParser = parserSink
{-# INLINABLE runParser #-}

-- NOTE: it is not known if this is law-abiding
instance Applicative (Parser i m) where
    pure = return
    {-# INLINABLE pure #-}
    (<*>) = ap
    {-# INLINABLE (<*>) #-}

-- NOTE: it is not known if this is law-abiding
instance Monad (Parser i m) where
    return = Parser (const True) . pure
    {-# INLINABLE return #-}

    a >>= f = Parser
        { parserApplicable = parserApplicable a
        , parserSink = parserSink a >>= parserSink . f
        }
    {-# INLINABLE (>>=) #-}

instance (Monad m)=> Alternative (Parser i m) where
    empty = Parser (const False) (fail "Parser: empty")
    {-# INLINABLE empty #-}

    a <|> b = Parser
        { parserApplicable = (||) <$> parserApplicable a <*> parserApplicable b
        , parserSink = do
            peekC >>= \e -> if parserApplicable a e
                            then parserSink a
                            else parserSink b
        }
    {-# INLINABLE (<|>) #-}

------------------------

-- Fundamental tools for building Parsers
--
-- Also keep in mind 'pure' (read nothing and succeed) and 'empty' (read nothing and fail)

-- | Parse an event if it satisfies a predicate. (else fail)
satisfy :: (Monad m)=> (i -> Bool) -> Parser i m i
satisfy p = Parser
    { parserApplicable = maybe False p
    , parserSink = maybe (error "satisfy: Internal error") id <$> await
    }
{-# INLINABLE satisfy #-}

-- | Skip an event equal to the one supplied. (and fail on any other event)
satisfyEq :: (Monad m, Eq i)=> i -> Parser i m ()
satisfyEq x = satisfy_ $ (x ==)
{-# INLINE satisfyEq #-}

-- | Skip an event if it satisfies a predicate. (else fail)
satisfy_ :: (Monad m)=> (i -> Bool) -> Parser i m ()
satisfy_ p = satisfy p $> ()
{-# INLINE satisfy_ #-}

-- | Parse an event into whatever your dreams are made of. (else fail)
eventParse :: (Monad m)=> (i -> Maybe a) -> Parser i m a
eventParse func = Parser
    { parserApplicable = maybe False (isJust . func)
    , parserSink = await >>= \case Nothing -> fail "eventParse: Internal error (await)"
                                   Just i  -> case func i of Nothing -> fail "eventParse: Internal error (func)"
                                                             Just x  -> pure x
    }
{-# INLINABLE eventParse #-}

------------------------
-- The things I ultimately need for the above code to work:

-- | Scan a mapping in search for a specific key, and then run the continuation parser
--   beginning on the first event for the associated value.
--
--   The continuation parser must consume exactly the events for a single node.
--   Failure behavior is not currently specified.
limitMappingValue :: ByteString -> ParserCont
limitMappingValue = limitMappingValueImpl -- defined below
{-# INLINABLE limitMappingValue #-}

-- | Read a sequence, running an event parser on the event stream beginning at each item.
--   Take the outputs of this parser and fold them.
--
--   The input parser must consume exactly the events for a single node on each run.
--   Failure behavior is not currently specified.
--
--   This version is strict, forcing the evaluation at each step.
foldl'Sequence :: (Monad m)=> (b -> a -> b) -> b -> EventParser m a -> EventParser m b
foldl'Sequence op start item = bracketAnchorlessSequence (doMany start) where
    doMany b = doSome b <|> doNone b
    doNone b = pure b
    doSome b = limitNode item >>= \a -> doMany $! (b `op` a)
{-# INLINABLE foldl'Sequence #-}

-- | Parse a sequence of two items with a function.
--
--   Each input parser must consume exactly one node.
--   Failure behavior is not currently specified.
parseTupleWith2 :: (Monad m)=> (a -> b -> c) -> Parser Event m a -> Parser Event m b -> Parser Event m c
parseTupleWith2 f parseA parseB = bracketAnchorlessSequence $ f <$> parseA <*> parseB
{-# INLINABLE parseTupleWith2 #-}

scalarFloat :: (Monad m)=> Parser Event m Double
scalarFloat = eventParse $ \case
    (EventScalar s _ _ _) -> attoParse Attoparsec.double s
    _ -> Nothing
{-# INLINABLE scalarFloat #-}

attoParse :: Attoparsec.Parser a -> ByteString -> Maybe a
attoParse p s = case flip Attoparsec.feed "" (Attoparsec.parse p s) of
    Attoparsec.Done "" x -> Just x
    _                    -> Nothing

named :: String -> Parser i m a -> Parser i m a
named _ = id -- TODO
{-# INLINABLE named #-}


------------------------
-- Grammar from pyyaml.org.
--
-- stream ::= STREAM-START document* STREAM-END
-- document ::= DOCUMENT-START node DOCUMENT-END
-- node ::= ALIAS | SCALAR | sequence | mapping
-- sequence ::= SEQUENCE-START node* SEQUENCE-END
-- mapping ::= MAPPING-START (node node)* MAPPING-END

------------------------
-- A suite of value-ignoring parsers

streamStart, streamEnd, documentStart, documentEnd, sequenceEnd, mappingEnd :: Ignore
anchorlessMappingStart, anchorlessSequenceStart :: Ignore
streamStart    = satisfyEq EventStreamStart
streamEnd      = satisfyEq EventStreamEnd
documentStart  = satisfyEq EventDocumentStart
documentEnd    = satisfyEq EventDocumentEnd
sequenceEnd    = satisfyEq EventSequenceEnd
mappingEnd     = satisfyEq EventMappingEnd
anchorlessMappingStart  = satisfyEq (EventMappingStart Nothing)
anchorlessSequenceStart = satisfyEq (EventSequenceStart Nothing)
{-# INLINABLE streamStart #-}
{-# INLINABLE streamEnd #-}
{-# INLINABLE documentStart #-}
{-# INLINABLE documentEnd #-}
{-# INLINABLE sequenceEnd #-}
{-# INLINABLE mappingEnd #-}
{-# INLINABLE anchorlessMappingStart  #-}
{-# INLINABLE anchorlessSequenceStart #-}

-- underscored because we ignore anchors
sequenceStart_, mappingStart_ :: Ignore
sequenceStart_ = satisfy_ $ \case (EventSequenceStart _) -> True; _ -> False
mappingStart_  = satisfy_ $ \case (EventMappingStart _) -> True; _ -> False
{-# INLINABLE sequenceStart_ #-}
{-# INLINABLE mappingStart_ #-}

stream_, document_, node_, sequence_, mapping_ :: Ignore
stream_   = named "boring stream"   $ bracketStream streamInner_
document_ = named "boring document" $ bracketDocument node_
node_     = named "boring node"     $ alias_ <|> scalar_ <|> sequence_ <|> mapping_
sequence_ = named "boring sequence" $ sequenceStart_ >> sequenceInner_ >> sequenceEnd
mapping_  = named "boring mapping"  $ mappingStart_ >> mappingInner_ >> mappingEnd
-- sequence_ = named "boring sequence" $ sequenceStart_ >> traceP "skipping sequence" >> sequenceInner_ >> sequenceEnd
-- mapping_  = named "boring mapping"  $ mappingStart_ >> traceP "skipping mapping" >> mappingInner_ >> mappingEnd
{-# INLINABLE stream_ #-}
{-# INLINABLE document_ #-}
{-# INLINABLE node_ #-}
{-# INLINABLE sequence_ #-}
{-# INLINABLE mapping_ #-}

traceP :: (MonadIO io) => String -> io ()
traceP = liftIO . traceIO
{-# INLINABLE traceP #-}

streamInner_, sequenceInner_, mappingInner_ :: Ignore
streamInner_   = skipMany document_
sequenceInner_ = skipMany node_
mappingInner_  = skipMany (node_ >> node_)
{-# INLINABLE streamInner_ #-}
{-# INLINABLE sequenceInner_ #-}
{-# INLINABLE mappingInner_ #-}

skipMany :: (Alternative f)=> f a -> f ()
skipMany x = () <$ many x
{-# INLINABLE skipMany #-}

scalar_, alias_ :: Ignore
scalar_ = satisfy_ $ \case (EventScalar _ _ _ _) -> True; _ -> False
alias_  = satisfy_ $ \case (EventAlias _)        -> True; _ -> False
{-# INLINABLE scalar_ #-}
{-# INLINABLE alias_ #-}

-- NOTE: Should take Text but I don't want to deal with encodings.
--       (LibYaml appears to support utf8, utf16le, and utf16be)
scalarStringLiteral_ :: ByteString -> Ignore
-- scalarStringLiteral_ x = eventSatisfy_ $ \case (EventScalar bs _ _ _) -> trace (ByteString.unpack bs) bs == x;  _ -> False
scalarStringLiteral_ x = satisfy_ $ \case (EventScalar bs _ _ _) -> bs == x;  _ -> False
{-# INLINABLE scalarStringLiteral_ #-}

------------------------
-- Helpers for the pair-delimited nonterminals
bracketStream, bracketDocument :: ParserCont
bracketStream   = between streamStart streamEnd
bracketDocument = between documentStart documentEnd
{-# INLINABLE bracketStream #-}
{-# INLINABLE bracketDocument #-}

-- I currently do not stand to benefit in any fashion from supporting anchors.
-- These are used in places where an anchor COULD meaningfully affect parsing,
--  but are not explicitly supported by this module.
bracketAnchorlessSequence, bracketAnchorlessMapping :: ParserCont
bracketAnchorlessSequence = between anchorlessSequenceStart sequenceEnd
bracketAnchorlessMapping = between anchorlessMappingStart mappingEnd
{-# INLINABLE bracketAnchorlessSequence #-}
{-# INLINABLE bracketAnchorlessMapping #-}

between :: Parser i m junk -> Parser i m junk' -> Parser i m x -> Parser i m x
between pre post mid = pre >> mid << post
{-# INLINABLE between #-}

------------------------

-- | Supply the continuation parser with the events for the value associated
--   with a specific key in a mapping.
--
--   Failure behavior is not currently specified.
limitMappingValueImpl :: ByteString -> ParserCont
limitMappingValueImpl label cont = named "mappingValue" parser
  where
    parser = bracketAnchorlessMapping succeedEventually

    succeedEventually = succeedNow <|> succeedLater
    succeedNow = scalarStringLiteral_ label >> limitNode cont << mappingInner_
    succeedLater = node_ >> node_ >> succeedEventually
{-# INLINABLE limitMappingValueImpl #-}

-- Feed a single element's events into a parser, serving a role similar to `takeExactlyC`
--
-- (I think these are possible to implement now that I'm no longer using conduit-parse,
--  but they're nontrivial to implement, and in the optimistic case it's easy enough to
--  always write parsers that consume exactly one nonterminal)
limitStream, limitDocument, limitNode, limitSequence, limitMapping :: ParserCont
limitStream   = id
limitDocument = id
limitNode     = id
limitSequence = id
limitMapping  = id

{-# INLINABLE limitStream #-}
{-# INLINABLE limitDocument #-}
{-# INLINABLE limitNode #-}
{-# INLINABLE limitSequence #-}
{-# INLINABLE limitMapping #-}

-- Raw types for parsing band.yaml,
-- with straightforward ToJSON/FromJSON implementations.
