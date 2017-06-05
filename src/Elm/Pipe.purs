module Elm.Pipe
    ( applyFn, (<|)
    , applyFnFlipped, (|>)
    , compose, (<<)
    , composeFlipped, (>>)
    ) where

import Prelude ( (<<<), (>>>), ($), (#) )



infixr 9 compose as <<

-- | Function composition, passing results along in the suggested direction. For
-- | example, the following code checks if the square root of a number is odd:
-- |
-- |     not << isEven << sqrt
-- |
-- | You can think of this operator as equivalent to the following:
-- |
-- |     (g << f)  ==  (\x -> g (f x))
-- |
-- | So our example expands out to something like this:
-- |
-- |     \n -> not (isEven (sqrt n))
-- |
-- | Equivalent to Purescript's `<<<`.
compose :: ∀ a b c. (b -> c) -> (a -> b) -> (a -> c)
compose = (<<<)


infixl 9 composeFlipped as >>

-- | Function composition, passing results along in the suggested direction. For
-- | example, the following code checks if the square root of a number is odd:
-- |
-- |     sqrt >> isEven >> not
-- |
-- | This direction of function composition seems less pleasant than `(<<)` which
-- | reads nicely in expressions like: `filter (not << isRegistered) students`
-- |
-- | Equivalent to Purescript's `>>>`.
composeFlipped :: ∀ a b c. (a -> b) -> (b -> c) -> (a -> c)
composeFlipped = (>>>)

infixl 0 applyFnFlipped as |>

-- | Forward function application `x |> f == f x`. This function is useful
-- | for avoiding parentheses and writing code in a more natural way.
-- | Consider the following code to create a pentagon:
-- |
-- |     scale 2 (move (10,10) (filled blue (ngon 5 30)))
-- |
-- | This can also be written as:
-- |
-- |     ngon 5 30
-- |       |> filled blue
-- |       |> move (10,10)
-- |       |> scale 2
-- |
-- | Equivalent to Purescript's `#`.
applyFnFlipped :: ∀ a b. a -> (a -> b) -> b
applyFnFlipped = (#)


infixr 0 applyFn as <|

-- | Backward function application `f <| x == f x`. This function is useful for
-- | avoiding parentheses. Consider the following code to create a text element:
-- |
-- |     leftAligned (monospace (fromString "code"))
-- |
-- | This can also be written as:
-- |
-- |     leftAligned <| monospace <| fromString "code"
-- |
-- | Equivalent to Purescript's `$`.
applyFn :: ∀ a b. (a -> b) -> a -> b
applyFn = ($)
