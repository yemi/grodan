module Grodan.Model where

import Random exposing (Seed, initialSeed)

import Time exposing (Time)

import Grodan.Config exposing (..)

-- INPUTS

type alias Arrows = { x: Int , y: Int }
type alias Input = { space: Bool, arrows: Arrows, delta: Time }

-- MODEL

type alias Positioned a = { a | x:Float, y:Float }
type alias Moving     a = { a | vx:Float, vy:Float }

type alias Frog = Moving (Positioned { life: Int })

type FoodType = Berries | Bugs

type ObjectType = Rock | Food FoodType

type alias Object = Positioned
  { objectType: ObjectType
  , isConsumed: Bool
  , isCrashed: Bool
  }

type alias Objects = List Object

type alias Score = Int

type State = Play | Pause | Lost

type alias Game =
  { frog: Frog
  , objects: (Objects, Seed)
  , score: Score
  , state: State
  }

defaultGame : Game
defaultGame =
  let
    objects =
      [
        {
          x=-250, y=0, objectType=Food Bugs, isConsumed=False, isCrashed=False
        },
        {
          x=250, y=400, objectType=Rock, isConsumed=False, isCrashed=False
        },
        {
          x=-100, y=800, objectType=Rock, isConsumed=False, isCrashed=False
        },
        {
          x=300, y=1200, objectType=Food Berries, isConsumed=False, isCrashed=False
        }
      ]
  in
    { objects = (objects, initialSeed 0)
    , frog = { x=0, y=-400, vx=frogSpeed, vy=0, life=100 }
    , score = 0
    , state = Pause
    }
