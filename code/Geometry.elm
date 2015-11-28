module Geometry where

type alias Transform = { a: Float
                       , b: Float
                       , c: Float
                       , d: Float }

type alias Point = { x: Float
                   , y: Float }

type alias Size = { w: Float
                  , h: Float }

type alias Bounds = { minp: Point
                    , maxp: Point }

width : Bounds -> Float
width {minp, maxp} = maxp.x - minp.x

height : Bounds -> Float
height {minp, maxp} = maxp.y - minp.y

point : Float -> Float -> Point
point x y = {x=x, y=y}

transform : Transform -> Float -> Point -> Point
transform {a,b,c,d} scale {x,y} =
  let
    x' = scale * (a*x + b)
    y' = scale * (c*y + d)
  in
    point x' y'

untransform : Transform -> Float -> Point -> Point
untransform {a,b,c,d} scale {x,y} =
  let
    x' = (x / scale - b ) / a
    y' = (y / scale - d ) / c
  in
    point x' y'


createBounds : Point -> Bounds
createBounds p = { minp = p
                 , maxp = p }

extendBounds : Point -> Bounds -> Bounds
extendBounds {x,y} {minp, maxp} =
  let
    xmin = min minp.x x
    xmax = max maxp.x x
    ymin = min minp.y y
    ymax = max maxp.y y
  in
    { minp = point xmin ymin
    , maxp = point xmax ymax }

bounds : List Point -> Bounds
bounds x = case x of
             [] -> createBounds (point 0 0)
             x :: xs ->
               let
                 baseBounds = createBounds x
               in
                 List.foldl extendBounds baseBounds xs
