module Diagram.Types.Sequence
  ( Step (..)
  , Steps
  , Component(..)
  , ComponentId
  , componentId
  , sequenceId
  , SequenceId
  , Identifier(..)
  , component
  , sync
  , async
  , start)
  where


import Data.Array (fromFoldable)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Diagram.Types.Attribute (Attributes, Attribute, findCaption)
import Prelude (class Eq, class Show, class Ord, show, eq, (<>), (&&))


newtype Identifier a
  = Identifier a

derive instance eqIdentifier :: Eq a => Eq (Identifier a)

derive instance ordIdentifier :: Ord a => Ord (Identifier a)


type ComponentId
  = Identifier String


componentId :: String -> Identifier String
componentId str =
  Identifier str


type SequenceId
  = Identifier String


sequenceId :: String -> SequenceId
sequenceId str =
  Identifier str


data Component
  = Component ComponentId Attributes


instance showComponent :: Show Component where
  show (Component (Identifier i) attributes) =
    case (findCaption attributes) of
      Nothing ->
        i
      Just c ->
        c


instance eqComponent :: Eq Component where
  eq (Component id1 _) (Component id2 _)
    = eq id1 id2


data Step
  = Synchronous ComponentId Attributes Steps
  | Asynchronous ComponentId Attributes Steps
  | Start (Maybe SequenceId) ComponentId Attributes Steps


instance showSequence :: Show Step where
  show (Synchronous (Identifier cId) attributes steps)
    = "\n=> " <> (show cId) <> " " <> (showCaption attributes) <> " " <> (show steps) <> "\n"

  show (Asynchronous (Identifier cId) attributes steps)
    = "\n-> " <> (show cId) <> " " <> (showCaption attributes) <> " " <> (show steps) <> "\n"

  show (Start mSequenceId (Identifier cId) attributes steps)
    = "\non " <> (show cId) <> " " <> (showCaption attributes) <> " " <> (show steps) <> "\n"


showStep :: String -> Step -> String
showStep prefix step =
  prefix <> " " <> (show step)


instance eqStep :: Eq Step where
  eq (Start (Just seq1) c1 attrs1 steps1) (Start (Just seq2) c2 attrs2 steps2)
    = (eq seq1 seq2) && (eq c1 c2) && (eq attrs1 attrs2) && (eq steps1 steps2)
  eq (Start Nothing c1 attrs1 steps1) (Start Nothing c2 attrs2 steps2)
    = (eq c1 c2) && (eq attrs1 attrs2) && (eq steps1 steps2)
  eq (Asynchronous c1 attrs1 steps1) (Asynchronous c2 attrs2 steps2)
    = (eq c1 c2) && (eq attrs1 attrs2) && (eq steps1 steps2)
  eq (Synchronous c1 attrs1 steps1) (Synchronous c2 attrs2 steps2)
    = (eq c1 c2) && (eq attrs1 attrs2) && (eq steps1 steps2)
  eq _ _
    = false


showCaption :: Attributes -> String
showCaption attributes =
  case (findCaption attributes) of
    Nothing ->
      ""
    Just c ->
      c


type Steps =
  Array Step


component :: String -> List Attribute -> Component
component cId attrs =
      Component (Identifier cId) (fromFoldable attrs)


start :: (Maybe SequenceId) -> ComponentId -> List Attribute -> List Step -> Step
start mSequenceId cId attrs steps =
  Start mSequenceId cId (fromFoldable attrs) (fromFoldable steps)


sync :: ComponentId -> List Attribute -> List Step -> Step
sync cId attrs steps =
  Synchronous cId (fromFoldable attrs) (fromFoldable steps)


async :: ComponentId -> List Attribute -> List Step -> Step
async cId attrs steps =
  Asynchronous cId (fromFoldable attrs) (fromFoldable steps)


--
