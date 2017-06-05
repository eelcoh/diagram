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
  , Layer
  , Direction(..)
  , Session
  , Head
  , Actor
  , Vertical
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
  , layers :: List Layer
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
  { start :: Y
  , startLayer :: Int
  , end :: Y
  , endLayer :: Int
  , head :: Head
  , direction :: Direction
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
  , arrowIn :: Y
  , arrowOut :: Y
  }



data Actor
  = Person
  | System

data Head
  = Open
  | Closed

data Direction
  = LeftToRight
  | RightToLeft
  | ToSelf


data Layer
    = Layer Int (List Session)


newtype Session =
    Session (Tuple Start End)


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
