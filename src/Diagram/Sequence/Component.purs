module Diagram.Sequence.Component
  ( findComponentById
  , findLifelineByComponentId
  ) where

import Data.Array
import Data.Maybe (Maybe)
import Diagram.Types.Sequence as S
import Diagram.Sequence.Types (Diagram, Component, Lifeline(..))
import Elm.Pipe ((|>))
import Prelude (eq, map)


{-}
findComponent :: Component -> Array Component -> Maybe Int
findComponent c cs =
  elemIndex c cs
-}

findComponentById :: S.ComponentId -> Diagram -> Maybe Component
findComponentById cId {components} =
  find (isComponent cId) components

-- findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLifelineByComponentId :: S.ComponentId -> Diagram -> Maybe Lifeline
findLifelineByComponentId componentId {components} =
    findIndex (isComponent componentId) components
      |> map Lifeline


isComponent :: S.ComponentId -> Component -> Boolean
isComponent cId {component, layers} =
  getComponentId component
    |> eq cId


getComponentId :: S.Component -> S.ComponentId
getComponentId (S.Component cId _) =
  cId
