module Grodan.Main where

import Window
import Signal exposing (Signal, (<~), (~))
import Debug exposing (log)
import List exposing (map, head, tail, reverse, foldr)
import Time exposing (Time, timestamp)
import Keyboard exposing (arrows, space)
import Random exposing (Seed, initialSeed, generate, float)
import Text

import Graphics.Element exposing (container, middle, Element, show, rightAligned, leftAligned)
import Graphics.Collage exposing (collage, rect, circle, filled, move, rotate, toForm, moveY, group)

import Color exposing (blue, green, red, orange, brown, yellow, charcoal)

import AnimationFrame

-- CONFIG

(gameWidth,gameHeight) = (768,1024)
(halfWidth,halfHeight) = (toFloat gameWidth / 2, toFloat gameHeight / 2)

itemSpacing = 400
objectSpeed = 4
frogSpeed = 8
maxItemRadius = 100

offCanvasY = halfHeight + maxItemRadius

-- HELPERS

last : List a -> a
last xs = 
  case (head <| reverse xs) of
    Just last' -> last'

head' : List a -> a
head' xs = 
  case (head xs) of
    Just head' -> head'

tail' : List a -> List a 
tail' xs = 
  case (tail xs) of
    Just tail' -> tail'

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


-- UPDATE

near : number -> number -> number -> Bool
near k c n = n >= k-c && n <= k+c

within : Float -> Positioned a -> Positioned b -> Bool
within range a b = (a.x |> near b.x range)
                && (a.y |> near b.y range)

isFood : Object -> Bool
isFood obj = 
  case obj.objectType of
    Rock -> False
    Food _ -> True

isFloatingFood : Object -> Bool
isFloatingFood obj =
  if isFood obj then 
   case obj.objectType of
     Food Berries -> True
     _ -> False
  else 
    False

frogAction : Object -> (Frog, (Objects, Seed), Score) -> (Frog, (Objects, Seed), Score)
frogAction object (frog, (objects, seed), score) =
  let 
    shouldEat = within 100 frog object && isFood object
    shouldRemoveObject = within 100 frog object && isFloatingFood object
    isCrash = within 40 frog object && not (isFloatingFood object)
    score' = if shouldEat && not object.isConsumed then score + 1 else score
    frog' = { frog | life <- if isCrash && not object.isCrashed then frog.life - 20 else frog.life }
    object' = { object | isConsumed <- object.isConsumed || shouldEat, isCrashed <- object.isCrashed || isCrash }
    (objects', seed') = if shouldRemoveObject then appendNewObject (objects, seed) else (object'::objects, seed)
  in
    (frog', (objects', seed'), score')

generateObjectType : Seed -> (ObjectType, Seed)
generateObjectType seed =
  let
    (num, seed') = generate (float 0 1) seed
    objectType = 
      if | num < 0.5 -> Rock
         | num < 0.7 -> Food Berries
         | otherwise -> Food Bugs
  in
    (objectType, seed')

appendNewObject : (Objects, Seed) -> (Objects, Seed)
appendNewObject (objects, seed) = 
  let 
    (distance, seed') = generate (float 0 itemSpacing) seed
    yPos = .y (last objects) + distance
    (xPos, seed'') = generate (float -halfWidth halfWidth) seed'
    (objectType, seed''') = generateObjectType seed''
    newObject = 
      { x=xPos
      , y=yPos
      , objectType=objectType
      , isConsumed=False
      , isCrashed=False 
      }
  in
    (objects ++ [newObject], seed''')

updateObjects : (Objects, Seed) -> (Objects, Seed)
updateObjects (objects, seed) = 
  let
    firstObject = head' objects
    tailObjects = tail' objects
    lastObject = last objects
  in
    if | firstObject.y <= -offCanvasY -> appendNewObject (tailObjects, seed)
       | lastObject.y <= offCanvasY -> appendNewObject (objects, seed)
       | otherwise -> (objects, seed)

stepObject : Float -> Object -> Object
stepObject complexity object =
  { object | y <- object.y - (objectSpeed * complexity) }

stepFrog : Int -> Frog -> (Objects, Seed) -> Score -> (Frog, (Objects, Seed), Score)
stepFrog horArrow frog (objects, seed) score =
  let 
    frog' = if | horArrow == -1 -> { frog | x <- max (-halfWidth + 60) <| frog.x - frog.vx }
               | horArrow == 1  -> { frog | x <- min (halfWidth - 60) <| frog.x + frog.vx }
               | otherwise -> frog
  in
    foldr frogAction (frog', ([], seed), score) objects

getComplexity : Score -> Float
getComplexity = (flip (/)) 80 << (+) 100 << toFloat --TODO create some awesome way to decide the complexity 

stepGame : Input -> Game -> Game
stepGame input ({state} as game) =
  let
    func = if | state == Play -> stepPlay
              | state == Pause -> stepPause
              | otherwise -> stepGameOver
  in
    func input game
 
stepPlay : Input -> Game -> Game
stepPlay {delta, arrows} game = 
  let 
    (objects, seed) = game.objects 
    (frog', (objects', seed'), score) = stepFrog arrows.x game.frog (objects, seed) game.score
    complexity = getComplexity score 
    state = if | frog'.life <= 0 -> Lost
               | otherwise -> Play
  in
    { game | objects <- updateObjects (map (stepObject complexity) objects', seed') 
           , frog <- frog' 
           , score <- score 
           , state <- state}

stepPause : Input -> Game -> Game
stepPause {space} game =
  let
    state = if space then Play else Pause
  in
    { game | state <- state }

stepGameOver : Input -> Game -> Game
stepGameOver {space} game = if space then defaultGame else game


-- VIEW

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


-- SIGNALS

main : Signal Element
main = display <~ Window.dimensions ~ gameState

gameState =
  Signal.foldp stepGame defaultGame input

input : Signal Input
input = Signal.sampleOn AnimationFrame.frame (Input <~ space
                                                     ~ arrows
                                                     ~ AnimationFrame.frame)

