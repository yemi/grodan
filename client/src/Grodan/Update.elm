module Grodan.Update where

import List exposing (map, foldr)

import Random exposing (Seed, initialSeed, generate, float)

import Grodan.Config exposing (..)
import Grodan.Model exposing (..)
import Grodan.Util exposing (..)

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
    yPos = .y (unsafeLast objects) + distance
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
    firstObject = unsafeHead objects
    tailObjects = unsafeTail objects
    lastObject = unsafeLast objects
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
