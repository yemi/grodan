module Grodan where

import Window
import Signal exposing (Signal, (<~), (~))
import Debug exposing (log)
import Keyboard exposing (arrows, space)
import AnimationFrame

import Graphics.Element exposing (Element)

import Grodan.Model exposing (Game, Input, defaultGame)
import Grodan.Update exposing (stepGame)
import Grodan.View exposing (display)

main : Signal Element
main = display <~ Window.dimensions ~ gameState

gameState : Signal Game
gameState = Signal.foldp stepGame defaultGame input

input : Signal Input
input = Signal.sampleOn AnimationFrame.frame (Input <~ space
                                                     ~ arrows
                                                     ~ AnimationFrame.frame)
