module Diagram.Sequence.Component
  ( findComponentByLifeline
  , findLifelineByComponentId
  , updateComponents
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

findComponentById :: S.ComponentId -> Diagram -> Maybe Component
findComponentById cId {components} =
  find (isComponent cId) components
-}

-- findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLifelineByComponentId :: S.ComponentId -> Diagram -> Maybe Lifeline
findLifelineByComponentId componentId {components} =
    findIndex (isComponent componentId) components
      |> map Lifeline


findComponentByLifeline :: Array Component -> Lifeline -> Maybe Component
findComponentByLifeline components (Lifeline l)  =
  index components l


isComponent :: S.ComponentId -> Component -> Boolean
isComponent cId {component, layers} =
  getComponentId component
    |> eq cId


getComponentId :: S.Component -> S.ComponentId
getComponentId (S.Component cId _) =
  cId

updateComponents :: Array Component -> Lifeline -> Component -> Maybe (Array Component)
updateComponents components (Lifeline l) component =
  updateAt l component components
