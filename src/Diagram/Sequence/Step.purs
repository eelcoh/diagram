module Diagram.Sequence.Step
  ( sessionPassOne
  , SessionPassOne
  ) where

import Control.Apply (lift2)
import Control.Bind ((>>=))
import Data.Array (cons, head, last, uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Diagram.Sequence.Component (findLifelineByComponentId)
import Diagram.Sequence.Types (Diagram, Direction(LeftToRight, ToSelf, RightToLeft), Lifeline, Start, Vertical, Y(Y))
import Diagram.Types.Attribute (Attribute)
import Diagram.Types.Sequence (ComponentId, Step(Start, Asynchronous, Synchronous), Steps)
import Elm.Pipe ((|>))
import Prelude (Ordering(LT, EQ, GT), add, compare, flip, (+), map)


-- types used in this module only
data SessionPassOne =
  SessionPassOne Lifeline Vertical (Array SessionPassOne)

-- type for convenience
data Tuple4 a b c d
  = Tuple4 a b c d

sessionPassOne :: Start -> Maybe Lifeline -> Diagram -> Step -> Maybe SessionPassOne
sessionPassOne start mLifelineFrom diagram step =
  case step of
    Synchronous componentId attributes steps ->
      addSession diagram mLifelineFrom componentId attributes steps start true

    Asynchronous componentId attributes steps ->
      addSession diagram mLifelineFrom componentId attributes steps start false

    Start (mSequenceId) componentId attributes steps ->
      addSession diagram mLifelineFrom componentId attributes steps start false


addSession :: Diagram -> Maybe Lifeline -> ComponentId -> Array Attribute -> Steps -> Start -> Boolean -> Maybe SessionPassOne
addSession diagram mLifelineFrom componentId attrs steps start withReturn =
  let
    mFirstStep =
      head steps

    mLastStep =
      last steps


    mLifeline =
      findLifelineByComponentId componentId diagram

    mFirstStepLifelineTo =
      mFirstStep
        >>= getLifelineForStep diagram

    mLastStepLifelineTo =
      mLastStep
        >>= getLifelineForStep diagram


    mDirectionLast =
      lift2 getDirection mLastStepLifelineTo mLifeline

    mDirectionIn =
      lift2 getDirection mLifeline mLifelineFrom

    mDirectionFirst =
      lift2 getDirection mLifeline mFirstStepLifelineTo

    mDirectionBack =
      lift2 getDirection mLifeline mLifelineFrom


    addBefore =
      case mFirstStep of

        Nothing ->
          Y 0

        Just step ->

          case lift2 Tuple mDirectionIn  mDirectionFirst of
            Just (Tuple ToSelf ToSelf) ->
              Y 1
            Just (Tuple ToSelf LeftToRight) ->
              Y 1
            Just (Tuple ToSelf RightToLeft) ->
              Y 1
            Just (Tuple LeftToRight ToSelf) ->
              Y 0
            Just (Tuple LeftToRight LeftToRight) ->
              Y 0
            Just (Tuple LeftToRight RightToLeft) ->
              Y 1
            Just (Tuple RightToLeft ToSelf) ->
              Y 1
            Just (Tuple RightToLeft LeftToRight) ->
              Y 1
            Just (Tuple RightToLeft RightToLeft) ->
              Y 0
            Nothing ->
              Y 0

    addLast =
      case mLastStep of

        Nothing ->
          Y 0

        Just step ->
          let

            lastHasReturn =
              case step of
                Asynchronous _ _ _ ->
                  true
                Start _ _ _ _ ->
                  true
                Synchronous _ _ _ ->
                  false

          in
            case lift2 (Tuple4 withReturn lastHasReturn) mDirectionLast mDirectionBack of
              Just (Tuple4 false _ _ _ ) ->
                Y 0
              Just (Tuple4 true false _ _) ->
                Y 0
              Just (Tuple4 _ _ ToSelf ToSelf) ->
                Y 1
              Just (Tuple4 _ _ ToSelf LeftToRight) ->
                Y 1
              Just (Tuple4 _ _ ToSelf RightToLeft) ->
                Y 0
              Just (Tuple4 _ _ LeftToRight ToSelf) ->
                Y 0
              Just (Tuple4 _ _ LeftToRight LeftToRight) ->
                Y 0
              Just (Tuple4 _ _ LeftToRight RightToLeft) ->
                Y 1
              Just (Tuple4 _ _ RightToLeft ToSelf) ->
                Y 1
              Just (Tuple4 _ _ RightToLeft LeftToRight) ->
                Y 0
              Just (Tuple4 _ _ RightToLeft RightToLeft) ->
                Y 0
              Nothing ->
                Y 0

    arrowInExtra =
      case mDirectionIn  of
        (Just ToSelf) ->
          Y 1
        _ ->
          Y 0

    arrowOutExtra =
      case map (Tuple withReturn) mDirectionBack of
        Just ( Tuple true ToSelf) ->
          Y 1
        _ ->
          Y 0

    nextSessions =
      sessionsPassOne (start + addBefore + arrowInExtra) mLifelineFrom diagram steps

    mLastArrowOut =
      map (\(SessionPassOne _ vertical _) -> vertical.arrowOut) (last nextSessions)

    mLastEnd =
      map (\(SessionPassOne _ vertical _) -> vertical.end) (last nextSessions)

    arrowIn =
      start

    sessionStart =
      arrowIn + arrowInExtra

    sessionEnd =
      fromMaybe (sessionStart + Y 1) mLastEnd
        |> add addLast

    arrowOut =
      sessionEnd + arrowOutExtra

    v =
      { start : sessionStart
      , end : sessionEnd
      , arrowIn : arrowIn
      , arrowOut : arrowOut
      }

  in
    map (\l -> SessionPassOne l v nextSessions) mLifeline



sessionsPassOne :: Start -> Maybe Lifeline -> Diagram -> Steps -> Array SessionPassOne
sessionsPassOne start mLifelineFrom diagram steps =
  case uncons steps of
    Nothing ->
      []

    Just { head: step, tail: rest } ->
      let
        first =
          sessionPassOne start mLifelineFrom diagram step

      in
        case first of
          Nothing ->
            []
          Just session @ SessionPassOne lifeline vertical sessions ->
            let
              newStart =
                vertical.arrowOut + (Y 1)
            in
                sessionsPassOne newStart mLifelineFrom diagram rest
                |> cons session



getLifelineForStep :: Diagram -> Step -> Maybe Lifeline
getLifelineForStep diagram step =
  getComponentIdForStep step
    |> (flip findLifelineByComponentId) diagram


getComponentIdForStep :: Step -> ComponentId
getComponentIdForStep step =
  case step of
    Synchronous componentId _ _ ->
      componentId

    Asynchronous componentId _ _ ->
      componentId

    Start _ componentId _ _ ->
      componentId


getDirection :: Lifeline -> Lifeline -> Direction
getDirection l1 l2 =
  case (compare l1 l2) of
    GT ->
      RightToLeft
    EQ ->
      ToSelf
    LT ->
      LeftToRight
