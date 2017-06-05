module Diagram.Parser
  ( parseTextColor
  , parseBackgroundColor
  , parseBorderColor
  , parseCaption
  , parseReturnCaption
  , parseAttribute
  , parseAttributes
  , parseComponents
  , parseSequence
  ) where

import Prelude hiding (between,when)
import Color (Color, fromHexString)
import Data.String (fromCharArray)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Maybe (Maybe(..))
import Data.Identity (Identity)
import Data.List (List, catMaybes)
import Elm.Pipe ((|>))


import Text.Parsing.Parser ( ParserT)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Language ( haskellDef)
import Text.Parsing.Parser.String (string, char)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser, hexDigit, space)


import Diagram.Types.Attribute (Attribute(..))
import Diagram.Types.Sequence (Component, ComponentId, component, Step, sync, async, start, componentId, sequenceId)


tokenParser :: TokenParser
tokenParser = makeTokenParser haskellDef


parseKeyword :: forall a b. String -> (a -> b) -> ParserT String Identity a -> ParserT String Identity b
parseKeyword kw f p =
  do
    _ <- string kw
    _ <- tokenParser.whiteSpace
    s <- p
    pure (f s)


colorParser6 :: forall m. Monad m => ParserT String m (Maybe Color)
colorParser6 =
  do
    h <- char '#'
    r1 <- hexDigit
    r2 <- hexDigit
    g1 <- hexDigit
    g2 <- hexDigit
    b1 <- hexDigit
    b2 <- hexDigit
    pure ( fromHexString $ fromCharArray [h, r1, r2, g1, g2, b1, b2])


colorParser3 :: forall m. Monad m => ParserT String m (Maybe Color)
colorParser3 =
  do
    h <- char '#'
    r <- hexDigit
    g <- hexDigit
    b <- hexDigit
    pure ( fromHexString $ fromCharArray [h, r, g, b])


colorParser :: forall m. Monad m => ParserT String m (Maybe Color)
colorParser =
  ((try colorParser6) <|> colorParser3)


parseTextColor :: ParserT String Identity (Maybe Attribute)
parseTextColor =
  parseKeyword "fg" ( (<$>) TextColor ) colorParser


parseBackgroundColor :: ParserT String Identity (Maybe Attribute)
parseBackgroundColor =
  parseKeyword "bg" ( (<$>) BackgroundColor ) colorParser


parseBorderColor :: ParserT String Identity (Maybe Attribute)
parseBorderColor =
  parseKeyword "bc" ( (<$>) BorderColor ) colorParser


parseCaption :: ParserT String Identity (Maybe Attribute)
parseCaption =
  parseKeyword "c" ( Just <<< Caption ) tokenParser.stringLiteral


parseReturnCaption :: ParserT String Identity (Maybe Attribute)
parseReturnCaption =
  parseKeyword "rc" ( Just <<< ReturnCaption ) tokenParser.stringLiteral


parseAttribute :: ParserT String Identity (Maybe Attribute)
parseAttribute =
  (try parseTextColor)
  <|> (try parseBackgroundColor)
  <|> (try parseBorderColor)
  <|> (try parseCaption)
  <|> (try parseReturnCaption)


parseMaybeAttributes :: ParserT String Identity (List (Maybe Attribute))
parseMaybeAttributes =
  tokenParser.brackets $ tokenParser.commaSep $ parseAttribute


parseMaybeAttributes' :: ParserT String Identity (List (Maybe Attribute))
parseMaybeAttributes' =
  parseAttribute
  |> tokenParser.commaSep
  |> tokenParser.brackets


parseAttributes :: ParserT String Identity (List Attribute)
parseAttributes =
  (\l -> catMaybes l) <$> parseMaybeAttributes


parseComponent :: ParserT String Identity Component
parseComponent =
  do
    id <- tokenParser.identifier
    attrs <- parseAttributes
    pure (component id attrs)

parseComponents :: ParserT String Identity (List Component)
parseComponents =
  do
    _ <- string "Components"
    _ <- tokenParser.whiteSpace
    _ <- tokenParser.symbol ":"
    _ <- tokenParser.whiteSpace
    tokenParser.brackets $ tokenParser.commaSep $ parseComponent


parseSync :: ParserT String Identity Step
parseSync =
  do
    _ <- string "sync"
    _ <- space
    _ <- tokenParser.whiteSpace
    parseStep sync

parseAsync :: ParserT String Identity Step
parseAsync =
  do
    _ <- string "async"
    _ <- space
    _ <- tokenParser.whiteSpace
    parseStep async


parseNextStep :: ParserT String Identity Step
parseNextStep =
  fix (\_ ->
    (parseAsync <|> parseSync))


parseStep :: (ComponentId -> (List Attribute) -> (List Step) -> Step) -> ParserT String Identity Step
parseStep stepFn =
  do
    componentId <- parseComponentId
    attrs <- parseAttributes
    steps <- parseSteps
    pure ( stepFn componentId attrs steps)

parseSteps :: ParserT String Identity (List Step)
parseSteps =
    (fix (\_ -> parseNextStep))
    |> tokenParser.commaSep
    |> tokenParser.brackets


parseSequence :: ParserT String Identity Step
parseSequence =
  do
    _ <- string "Sequence"
    -- _ <- tokenParser.whiteSpace
    -- seqId <- (optionMaybe tokenParser.identifier)
    _ <- tokenParser.whiteSpace
    parseStep (start Nothing)


parseComponentId :: ParserT String Identity ComponentId
parseComponentId =
  do
    cid <- tokenParser.identifier
    pure (componentId cid)


parseSequenceId :: ParserT String Identity ComponentId
parseSequenceId =
  do
    sid <- tokenParser.identifier
    pure (sequenceId sid)




--
