module Grodan.Util where

import List exposing (head, tail, reverse)

import Grodan.Model exposing (..)

unsafeHead : List a -> a
unsafeHead xs =
  case (head xs) of
    Just head' -> head'

unsafeLast : List a -> a
unsafeLast = unsafeHead << reverse

unsafeTail : List a -> List a
unsafeTail xs =
  case (tail xs) of
    Just tail' -> tail'

near : Float -> Float -> Float -> Bool
near k c n = n >= k-c && n <= k+c

within : Float -> Positioned a -> Positioned b -> Bool
within range a b = (a.x |> near b.x range)
                && (a.y |> near b.y range)
