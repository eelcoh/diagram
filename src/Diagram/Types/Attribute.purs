module Diagram.Types.Attribute (Attribute(..), Attributes, findCaption, findReturnCaption, caption) where

import Data.Array
import Color (Color, rgb)
import Control.Bind ((>>=))
import Data.Maybe (Maybe(..))
import Elm.Pipe ((|>))
import Prelude (class Eq, class Show, eq, show, (<>))


type Attributes
  = Array Attribute


data Attribute
  = TextColor Color
  | BackgroundColor Color
  | BorderColor Color
  | Caption String
  | ReturnCaption String


instance showAttribute :: Show Attribute where
  show (TextColor c)
    = "TextColor" <> (show c)
  show (BackgroundColor c)
    = "BackgroundColor" <> (show c)
  show (BorderColor c)
    = "BorderColor" <> (show c)
  show (Caption c)
    = "Caption" <> c
  show (ReturnCaption c)
    = "ReturnCaption" <> c


instance eqAttribute :: Eq Attribute where
  eq (TextColor c1) (TextColor c2)
    = eq c1 c2
  eq (BackgroundColor c1) (BackgroundColor c2)
    = eq c1 c2
  eq (BorderColor c1) (BorderColor c2)
    = eq c1 c2
  eq (Caption c1) (Caption c2)
    = eq c1 c2
  eq (ReturnCaption c1) (ReturnCaption c2)
    = eq c1 c2
  eq _ _
    = false


toColorAttribute :: (Color -> Attribute) -> Int -> Int -> Int -> Attribute
toColorAttribute attributeConstructor r g b  =
  rgb r g b
    |> attributeConstructor


backgroundColor :: Int -> Int -> Int -> Attribute
backgroundColor =
  toColorAttribute BackgroundColor


textColor :: Int -> Int -> Int -> Attribute
textColor =
  toColorAttribute TextColor


borderColor :: Int -> Int -> Int -> Attribute
borderColor =
  toColorAttribute BorderColor


caption :: String -> Attribute
caption c =
  Caption c


returnCaption :: String -> Attribute
returnCaption c =
  ReturnCaption c

findCaption :: Attributes -> Maybe String
findCaption attributes =
  find isCaption attributes
    >>= extractString

findReturnCaption :: Attributes -> Maybe String
findReturnCaption attributes =
  find isReturnCaption attributes
    >>= extractString

isCaption :: Attribute -> Boolean
isCaption (Caption c) = true
isCaption _           = false

isReturnCaption :: Attribute -> Boolean
isReturnCaption (ReturnCaption c) = true
isReturnCaption _                 = false

extractColor :: Attribute -> Maybe Color
extractColor attr =
  case attr of
      TextColor c ->
        Just c
      BackgroundColor c ->
        Just c
      BorderColor c ->
        Just c
      Caption _ ->
        Nothing
      ReturnCaption _ ->
        Nothing

extractString :: Attribute -> Maybe String
extractString attr =
  case attr of
      TextColor _ ->
        Nothing
      BackgroundColor _ ->
        Nothing
      BorderColor _ ->
        Nothing
      Caption c ->
        Just c
      ReturnCaption c ->
        Just c
--
