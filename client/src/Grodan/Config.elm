module Grodan.Config where

(gameWidth,gameHeight) = (768,1024)
(halfWidth,halfHeight) = (toFloat gameWidth / 2, toFloat gameHeight / 2)

itemSpacing = 400
objectSpeed = 4
frogSpeed = 8
maxItemRadius = 100

offCanvasY = halfHeight + maxItemRadius
