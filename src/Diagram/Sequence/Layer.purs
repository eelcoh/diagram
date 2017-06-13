module Diagram.Sequence.Layer
  ( registerSession
  ) where

import Data.Array (index)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Diagram.Sequence.Types (Component, Layer, LayerRegister, Lifeline(..), Session, Diagram)
import Elm.Pipe ((|>))

registerSession :: Session -> Lifeline -> Diagram -> Maybe (Tuple Layer Diagram)
registerSession session lifeline diagram =
  findComponentByLifeline diagram.components lifeline
    >>= addSessionToLifeline session lifeline diagram


addSessionToLifeline :: Session -> Lifeline -> Diagram -> Component -> Maybe (Tuple Layer (Array Component))
addSessionToLifeline session lifeline diagram c@{component, layers} =
  addSessionToLayers session layers (Layer 0)
    |> updateDiagram diagram lifeline


addSessionToLayers :: Session -> Array LayerRegister -> Layer -> Tuple Layer LayerRegister
addSessionToLayers session layers layer@(Layer l) =
  case (uncons layers) of
    Nothing ->
      Tuple layer (LayerRegister [session])
    Just lh:lrest ->
      case (addSessionToLayer session lh) of
        Nothing ->
          addSessionToLayers session lrest (Layer (l+1))
        Just layerRegister ->
          Tuple layer layerRegister


addSessionToLayer :: Session -> LayerRegister -> Maybe LayerRegister
addSessionToLayer session (LayerRegister sessions) =
  addSessionToLayerSessions sessions session
    |> map LayerRegister

addSessionToLayerSessions :: Session -> Array Session -> Maybe (Array Session)
addSessionToLayerSessions session sessions =
  case (uncons sessions) of
    Nothing ->
      Just [session]
    Just {start, end}:t ->
      if start > session.end
        then
          Just (cons session sessions)
        else
          if end < session.start
            then
              addSessionToLayerSessions t session
               |> map (\s -> cons {start, start} s)
            else
              Nothing

updateDiagram :: Array Component -> Lifeline -> Component -> Maybe (Tuple Layer Diagram)
updateDiagram diagram lifeline {component} (Tuple layer layerRegister) =
  let
    newComponent =
      {component: component, layers: layerRegister}

    mNewComponents =
      updateComponents diagram.components lifeline component

    newDiagram =
      diagram { components : newComponents}
  in
    map (Tuple layer) diagram
