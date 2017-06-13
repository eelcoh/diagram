module Diagram.Sequence.Step
  ( sessionPassOne
  , SessionPassOne
  ) where

import Control.Apply (lift2)
import Control.Bind ((=<<), (>>=))
import Data.Array (cons, head, last, uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Diagram.Sequence.Component (findLifelineByComponentId)
import Diagram.Sequence.Types (Diagram, Direction(LeftToRight, ToSelf, RightToLeft), Element, Head(Open, Closed), Horizontal, Layer, Lifeline, Session(..), Start, Vertical, Y(Y), ArrowDetails)
import Diagram.Types.Attribute (Attribute)
import Diagram.Types.Sequence (ComponentId, Step(Start, Asynchronous, Synchronous), Steps)
import Elm.Pipe ((|>))
import Prelude (Ordering(LT, EQ, GT), add, compare, flip, (+), map)


-- types used in this module only
data SessionPassOne =
  SessionPassOne (Array Attribute) Lifeline Vertical Arrows (Array SessionPassOne)

data SessionPassTwo =
  SessionPassTwo (Array Attribute) Horizontal Vertical Arrows (Array SessionPassOne)

type ArrowMeta =
  { incoming :: Maybe Head
  , outgoing :: Boolean
  }

type Arrows =
  { incoming :: Maybe ArrowIn
  , outgoing :: Maybe ArrowOut
  }

data ArrowIn
  = Incoming Direction IncomingArrowType

data IncomingArrowType
  = SyncArrow
  | AsyncArrow

data ArrowOut
  = Outgoing Direction

-- type for convenience
data Tuple4 a b c d
  = Tuple4 a b c d


-- Pass one: get all vertical dimensions right
sessionPassOne :: Start -> Maybe Lifeline -> Diagram -> Step -> Maybe SessionPassOne
sessionPassOne start mLifelineFrom diagram step =
  case step of
    Synchronous componentId attributes steps ->
      let
        arrows =
          { incoming : (Just Closed), outgoing : true }
      in
        addSession diagram mLifelineFrom componentId attributes steps start arrows

    Asynchronous componentId attributes steps ->
      let
        arrows =
          { incoming : (Just Open), outgoing : false }
      in
        addSession diagram mLifelineFrom componentId attributes steps start arrows

    Start (mSequenceId) componentId attributes steps ->
      let
        arrows =
          { incoming : Nothing, outgoing : false }
      in
        addSession diagram mLifelineFrom componentId attributes steps start arrows


sessionPassTwo :: SessionPassOne -> Maybe Horizontal -> Diagram -> Tuple SessionPassTwo Diagram
sessionPassTwo session@(SessionPassOne attributes lifeline vertical arrows steps) mHorizontalFrom diagram =
  let
    mkSession {start, end} =
      Session {start: start, end: end}

    session =
      mkSession vertical

    mLayerAndDiagram =
      registerSession session lifeline diagram

    horizontal =
      case mLayerAndDiagram of
        Just (Tuple layer _) ->
          { lifeline:lifeline, layer: X layer }
        Nothing ->
          { lifeline:lifeline, layer: X 0 }

    newArrowIn =
      map (lift2 createArrow mHorizontalFrom) arrows.incoming

    newArrowOut =
      map (lift2 createArrow mHorizontalFrom) arrows.outgoing

    Tuple newSteps newNwDiagram =
      case mLayerAndDiagram of
        Just (Tuple _ newDiagram) ->
          sessionsPassTwo newDiagram steps
        Nothing ->
          Tuple steps diagram

  in
    SessionsPassTwo attributes horizontal vertical newArrows newSteps

sessionsPassTwo :: Array SessionPassOne -> Diagram -> Tuple (Array SessionPassTwo) Diagram
sessionsPassTwo newDiagram steps =
  case (uncons steps) of
    Nothing ->
      Tuple [] diagram
    Just session:rest ->
      let
        Tuple nwSession nwDiagram =
          sessionPassTwo session newDiagram

        Tuple nwSessions nwNwDiagram
          sessionsPassTwo nwDiagram rest

      in
        Tuple (cons nwSession nwSessions) nwNwDiagram




createIncomingArrow :: Horizontal -> Vertical -> Horizontal -> Vertical -> ArrowIn -> ArrowDetails
createIncomingArrow xStart yStart xEnd yEnd (Incoming direction arrowtype) =
  let
    cStart =
      { lifeline = xStart.lifeline
      , y = yStart.y
      , layer = xStart.layer
      }

    cEnd =
      { lifeline = xEnd.lifeline
      , layer = xEnd.layer
      , y = yEnd.y
      }

    aT =
      case arrowtype of
        SyncArrowType ->
          SyncArrow
        AsyncArrowType ->
          AyncArrow
  in
    { start : cStart
    , end : cEnd
    , arrowType : aT
    , direction : direction
    }











addSession :: Diagram -> Maybe Lifeline -> ComponentId -> Array Attribute -> Steps -> Start -> ArrowMeta -> Maybe SessionPassOne
addSession diagram mLifelineFrom componentId attrs steps start arrowMeta =
  let
    withReturn
      = arrowMeta.outgoing

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
      sessionsPassOne attrs (start + addBefore + arrowInExtra) mLifelineFrom diagram steps

    mLastArrowOut =
      map (\(SessionPassOne _ vertical _ _) -> vertical.arrowOutEnd) (last nextSessions)

    mLastEnd =
      map (\(SessionPassOne _ vertical _ _) -> vertical.end) (last nextSessions)

    arrowInStart =
      start

    sessionStart =
      arrowInStart + arrowInExtra

    sessionEnd =
      fromMaybe (sessionStart + Y 1) mLastEnd
        |> add addLast

    arrowOutEnd =
      sessionEnd + arrowOutExtra

    v =
      { start : sessionStart
      , end : sessionEnd
      , arrowInStart : arrowInStart
      , arrowOutEnd : arrowOutEnd
      }

    mkArrowIn direction hd =
      Incoming direction hd

    arrowIn =
      lift2 mkArrowIn mDirectionIn arrowMeta.incoming

    arrowOut =
      if arrowMeta.outgoing
        then
          map Outgoing mDirectionLast
        else
          Nothing

    arrows =
      { incoming : arrowIn, outgoing : arrowOut }

  in
    map (\l -> SessionPassOne attrs l v arrows nextSessions) mLifeline



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
          Just session @ SessionPassOne _ _ v _ _ ->
            let
              newStart =
                v.arrowOutEnd + (Y 1)
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
