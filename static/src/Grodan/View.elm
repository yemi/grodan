module Grodan.View where

import Graphics.Element exposing (container, middle, Element, show, rightAligned, leftAligned)
import Graphics.Collage exposing (collage, rect, circle, filled, move, rotate, toForm, moveY, group)

import Color exposing (blue, green, red, orange, brown, yellow, charcoal)

import List exposing (map)

import Text

import Grodan.Config exposing (..)
import Grodan.Model exposing (..)

txt : (Text.Text -> Text.Text) -> String -> Element
txt f = Text.fromString
        >> Text.color yellow
        >> Text.monospace
        >> f
        >> leftAligned

drawFrog frog =
  circle 40
    |> filled green
    |> move (frog.x, frog.y)

drawObject obj =
  let
    color = case obj.objectType of
      Rock -> charcoal
      Food f -> case f of
        Berries -> red
        Bugs -> brown
  in
    circle 20
      |> filled color
      |> move (obj.x, obj.y)

drawScore score =
  toString score
    |> Text.fromString
    |> Text.height 40
    |> Text.color yellow
    |> rightAligned
    |> toForm
    |> move (halfWidth - 100, halfHeight - 100)

display (w,h) game =
  let
    objects = group <| map drawObject (fst game.objects)
    background = rect gameWidth gameHeight |> filled blue
    life = txt (Text.height 40) (toString game.frog.life)
             |> toForm
             |> move (-halfWidth + 100, halfHeight - 100)
  in
    container w h middle <|
      collage gameWidth gameHeight <|
        [ background
        , objects
        , drawScore game.score
        , life
        , drawFrog game.frog
        ]
