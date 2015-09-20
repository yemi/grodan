module Grodan.Util where

import List exposing (head, tail, reverse)

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
