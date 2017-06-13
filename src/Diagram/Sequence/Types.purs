module Diagram.Sequence.Types
  ( Diagram(..)
  , Component
  , Lifeline(..)
  , Element(..)
  , SessionDetails
  , ArrowDetails
  , ComponentDetails
  , LifelineDetails
  , Start
  , End
  , Y(Y)
  , X(X)
  , Layer
  , LayerRegister
  , Direction(..)
  , Session(..)
  , Head (..)
  , Actor
  , Vertical
  , Horizontal
  ) where


import Prelude
import Diagram.Types.Sequence as S
import Data.List (List)
import Data.Tuple (Tuple)
import Diagram.Types.Attribute (Attribute)


type Diagram =
  { components :: Array Component
  , elements :: List Element
  }


type Component =
  { component :: S.Component
  , layers :: Array LayerRegister
  }


data Element
  = ESession (List Attribute) SessionDetails
  | EArrow (List Attribute) ArrowDetails
  | EComponent (List Attribute) ComponentDetails
  | ELifeline (List Attribute) LifelineDetails


type SessionDetails =
  { start :: Y
  , end :: Y
  , lifeline :: Lifeline
  , layer :: Int
  }

type ArrowDetails =
  { start :: Coordinate
  , end :: Coordinate
  , arrowType :: ArrowType
  , direction :: Direction
  }

type Coordinate =
  { lifeline :: Lifeline
  , layer :: Layer
  , y :: Y
  }

type ComponentDetails =
  { lifeline :: Lifeline
  , type :: Actor
  }

type LifelineDetails =
  { lifeline :: Lifeline
  }

type Vertical =
  { start :: Y
  , end :: Y
  , arrowInStart :: Y
  , arrowOutEnd :: Y
  }

type Horizontal =
  { lifeline :: Lifeline
  , layer :: X
  }

data Actor
  = Person
  | System

data Head
  = Open
  | Closed

data ArrowType
  = SyncArrow
  | AsyncArrow
  | ReturnArrow

data Direction
  = LeftToRight
  | RightToLeft
  | ToSelf


newtype LayerRegister
  = LayerRegister (Array Session)


newtype Session
  = Session {start :: Y, end :: Y}


type Layer
  = X

newtype X
  = X Int

instance semiringX :: Semiring X where
  one =
    X 1

  mul (X x1) (X x2) =
    X (x1 * x2)

  zero =
    X 0

  add (X x1) (X x2) =
    X (x1 + x2)

type Start
  = Y

type End
  = Y

newtype Y =
    Y Int



instance semiringY :: Semiring Y where
  one =
    Y 1

  mul (Y y1) (Y y2) =
    Y (y1 * y2)

  zero =
    Y 0

  add (Y y1) (Y y2) =
    Y (y1 + y2)


newtype Lifeline =
    Lifeline Int

derive instance ordLifeline :: Ord Lifeline
derive instance eqLifeline :: Eq Lifeline
