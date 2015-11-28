module Maps.Geometry where

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

scalePoint : Point -> Size -> Point
scalePoint {x,y} {w,h} = point (w * x) (h * y)

distSq : Point -> Point -> Float
distSq {x,y} p = (x - p.x)*(x - p.x) + (y - p.y)*(y - p.y)

subtractPoint : Point -> Point -> Point
subtractPoint {x,y} p = point (x - p.x) (y - p.y)

addPoint : Point -> Point -> Point
addPoint {x,y} p = point (x + p.x) (y + p.y)

trim : Float -> Float
trim = truncate >> toFloat

roundPoint : Point -> Point
roundPoint {x,y} = point (trim x) (trim y)
