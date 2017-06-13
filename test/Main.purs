module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE,  logShow)
import Test.Assert (ASSERT, assert')

import Text.Parsing.Parser (ParserT, Parser, runParser)
import Text.Parsing.Parser.Language (haskellDef)

import Text.Parsing.Parser.String (string, char)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser, hexDigit)

import Data.List (List(..), fromFoldable)
import Data.Either (Either(..))
import Data.Identity (Identity)

import Data.Tuple (Tuple(..))
import Color (rgb)
import Data.Maybe (Maybe(..))

import Diagram.Parser (parseAttribute, parseAttributes, parseComponents, parseSequence)
import Diagram.Types.Attribute (Attribute(..))
import Diagram.Types.Sequence (Component, Step, Identifier(..), component, start, sync, async)

tokenParser :: TokenParser
tokenParser = makeTokenParser haskellDef

testPair :: ParserT String Identity (Tuple String String)
testPair =
  Tuple <$> tokenParser.identifier <*> tokenParser.identifier


main :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
main = do

  -- learning stuff
  parseTest "a,a,a" (Cons "a" (Cons "a" (Cons "a" Nil))) $ string "a" `sepBy1` string ","
  parseTest "[abc]" "abc" $ tokenParser.brackets $ string "abc"
  parseTest "#" '#' $  char '#'
  parseTest "f" 'f' $  hexDigit
  parseTest "fe" 'e' $  hexDigit *> hexDigit
  parseTest "Abc " "Abc" $ tokenParser.identifier
  parseTest "abc " "abc" $ tokenParser.identifier
  parseTest "abc ss" "abc" $ tokenParser.identifier
  parseTest "abc ss" "abc" $ tokenParser.identifier
  parseTest "abc ss" (Tuple "abc" "ss") $ Tuple <$> tokenParser.identifier <*> tokenParser.identifier
  parseTest "[abc ss]" (Cons (Tuple "abc" "ss") Nil) $ tokenParser.brackets $ tokenParser.commaSep $ testPair
  parseTest "[abc ss, def dd]" (Cons (Tuple "abc" "ss") (Cons (Tuple "def" "dd") Nil)) $ tokenParser.brackets $ tokenParser.commaSep $ testPair

  -- real stuff
  parseTest "fg #345, bg #345fef" (fromFoldable [(Just (TextColor (rgb 51 68 85))), (Just (BackgroundColor (rgb 52 95 239)))]) $ tokenParser.commaSep $ parseAttribute
  parseTest "[fg #345, bg #345fef]" (fromFoldable [(Just (TextColor (rgb 51 68 85))), (Just (BackgroundColor (rgb 52 95 239)))]) $ tokenParser.brackets $ tokenParser.commaSep $ parseAttribute
  parseTest """[fg #345, c "#345fef"]""" (fromFoldable [(Just (TextColor (rgb 51 68 85))), (Just (Caption "#345fef"))]) $ tokenParser.brackets $ tokenParser.commaSep $ parseAttribute
  parseTest """[fg #345, c "#345fef"]""" (fromFoldable [(TextColor (rgb 51 68 85)), (Caption "#345fef")]) $ parseAttributes

  parseTest components1a components1Res $ parseComponents
  parseTest components1b components1Res $ parseComponents

  parseTest sequence1a sequence1Res $ parseSequence
  parseTest sequence1b sequence1Res $ parseSequence


components1a :: String
components1a = """Components:
  [ abc [], def [fg #345], ghi [c "ghi"]]"""

components1b :: String
components1b = """Components:
  [ abc []
  , def [fg #345]
  , ghi [c "ghi"]
  ]
"""

components1Res :: List Component
components1Res =
    (fromFoldable
      [ ( component "abc" Nil )
      , ( component "def" (fromFoldable [ ( TextColor (rgb 51 68 85) )]) )
      , ( component "ghi" (fromFoldable [ ( Caption "ghi" ), ( TextColor (rgb 51 68 85) )]) )
      ])

sequence1a :: String
sequence1a = """Sequence aaa []
  [ sync abc [] [], async def [fg #345] [], sync ghi [c "ghi"] []]"""

sequence1b :: String
sequence1b = """Sequence aaa []
  [ sync abc [] []
  , async def [fg #345] []
  , sync ghi [c "ghi"] []
  ]
"""

sequence1Res :: Step
sequence1Res =
  (start Nothing (Identifier "aaa") Nil
    ( fromFoldable
      [ sync (Identifier "abc") Nil Nil
      , async (Identifier "def") (fromFoldable [ ( TextColor (rgb 51 68 85) )]) Nil
      , sync (Identifier "ghi") (fromFoldable [ ( Caption "ghi" ) ]) Nil
      ]
    )
  )

parseTest :: forall s a eff. Show a => Eq a => s -> a -> Parser s a -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseTest input expected p = case runParser input p of
  Right actual -> do
    assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
    logShow actual
  Left err -> assert' ("error: " <> show err) false
