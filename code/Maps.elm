module Maps where

type alias Transform = { a: Float
                       , b: Float
                       , c: Float
                       , d: Float }

type alias ZoomLevel = Int

type alias Point = { x: Float
                   , y: Float }

type alias Size = { w: Float
                  , h: Float }

type alias LatLng = { lat: Float
                    , lng: Float }

type alias Bounds = { minp: Point
                    , maxp: Point }

type alias LatLngBounds = { minp: LatLng
                          , maxp: LatLng }

type alias TileLayer =  { minZoom: Int
                        , maxZoom: Int }

type alias TileCoord = { x: Int, y: Int, z: Int, xoff: Int, yoff: Int }

type alias Map = { center: LatLng
                 , size: Size
                 , zoom: ZoomLevel
                 , tileSize: Size}


earthRadius : Float
earthRadius = 6378137.0

maxLatitude: Float
maxLatitude = 85.0511287798

ln : Float -> Float
ln = logBase e

exp : Float -> Float
exp = (^) e

deg2rad : Float -> Float
deg2rad = (*) (pi / 180.0)

rad2deg : Float -> Float
rad2deg = (*) (180.0 / pi)

width : Bounds -> Float
width {minp, maxp} = maxp.x - minp.x

height : Bounds -> Float
height {minp, maxp} = maxp.y - minp.y

epsg4326Transform : Transform
epsg4326Transform = { a = 1.0 / 180.0
                    , b = 1.0
                    , c = -1.0 / 180.0
                    , d = 0.5}

epsg3857Transform : Transform
epsg3857Transform =
  let
    s = 0.5 / (pi * earthRadius)
  in
    { a = s
    , b = 0.5
    , c = -s
    , d = 0.5}

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


project: LatLng -> Point
project latlng =
  let
    lat = max (min maxLatitude latlng.lat) -maxLatitude
    latSin = sin (deg2rad lat)

    x = earthRadius * (deg2rad latlng.lng)

    i = ((1.0 + latSin) / (1.0 - latSin))

    y = earthRadius * (ln i) / 2.0
  in
    { x=x
    , y=y}

unproject : Point -> LatLng
unproject point =
  { lat = rad2deg (2.0 * atan(exp(point.y / earthRadius)) - (pi / 2.0))
  , lng = rad2deg(point.x / earthRadius) }


scale : ZoomLevel -> Float
scale zoom = 2.0 ^ (toFloat zoom)

pointAtTile : ZoomLevel -> Point -> Point
pointAtTile zl p =
  untransform epsg3857Transform (scale zl) p

tileAtPoint : ZoomLevel -> Point -> Point
tileAtPoint zl p =
  transform epsg3857Transform (scale zl) p

tileSize = 256

range : Int -> Int -> List Int
range x y =
  if x > y then
     []
  else
    let
      zeros = List.repeat (y - x + 1) 0
    in
      List.indexedMap (\i _ -> x + i) zeros

pairRange : (Int, Int) -> (Int, Int) -> List (Int, Int)
pairRange (x1, y1) (x2, y2) =
  let
    r1 = range x1 x2
    r2 = range y1 y2

    mapper a = List.map (\b -> (a,b)) r2
  in
    List.concatMap mapper r1

tileBoundsForView : ZoomLevel -> Point -> Size -> Bounds
tileBoundsForView zl p {w,h} =
  let
    {x,y} = roundPoint (tileAtPoint zl p)
    w' = toFloat (truncate (w / tileSize / 2.0 + 0.5))
    h' = toFloat (truncate (h / tileSize / 2.0 + 0.5))

    p1 = point (x - w') (y - h')
    p2 = point (x + w') (y + h')
  in
    bounds [p1, p2]

trim : Float -> Float
trim = truncate >> toFloat

roundPoint : Point -> Point
roundPoint {x,y} = point (trim x) (trim y)

tilesForView : ZoomLevel -> Point -> Size -> List TileCoord
tilesForView zl center s =
  let
    {minp, maxp} = tileBoundsForView zl center s

    xmin = truncate minp.x
    ymin = truncate minp.y

    xmax = truncate maxp.x
    ymax = truncate maxp.y

    pairs = pairRange (xmin, ymin) (xmax, ymax)
  in
    List.map (\(x,y) -> { x=x
                        , y=y
                        , z=zl
                        , xoff=x-xmin
                        , yoff=y-ymin}) pairs

scalePoint : Point -> Size -> Point
scalePoint {x,y} {w,h} = point (w * x) (h * y)

subtractPoint : Point -> Point -> Point
subtractPoint {x,y} p = point (x - p.x) (y - p.y)

addPoint : Point -> Point -> Point
addPoint {x,y} p = point (x + p.x) (y + p.y)

pixelOrigin : Map -> Point
pixelOrigin {zoom, tileSize, center} =
  let
    {x,y} = tileAtPoint zoom (project center)
    xoff = (x - trim x) * tileSize.w
    yoff = (y - trim y) * tileSize.h
  in
    point xoff yoff

-- pixelOrigin {zoom, size, center} =
--   let
--     {w,h} = size
--     viewHalf = point (w / 2.0) (h / 2.0)
--     tp = tileAtPoint zoom (project center)
--   in
--     roundPoint (tp `subtractPoint` viewHalf)


tilePosition : Size -> Point -> Point -> Point
tilePosition size origin coord =
  (coord `scalePoint` size) `subtractPoint` origin
