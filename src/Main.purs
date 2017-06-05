module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Diagram.Types.Sequence (sync, componentId)
import Diagram.Types.Attribute (caption)
import Elm.Pipe ((|>))
import Data.List ((:), List(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main =
  let

    attrs =
      caption "hoi()" : Nil

    seq =
      sync (componentId "ABC") attrs Nil

  in
    do
      show seq
        |> (<>) "Hello sailor! "
        |> log
