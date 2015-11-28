module MapRender where

type alias Transform = { a: Float
                       , b: Float
                       , c: Float
                       , d: Float }

type alias ZoomLevel = Int

type alias Point = { x: Float
                   , y: Float }


type alias LatLng = { lat: Float
                    , lng: Float }

type alias Bounds = { minp: Point
                    , maxp: Point }


type alias LatLngBounds = { topLeft: LatLng
                          , bottomRight: LatLng
                          }

-- Tile Layer
type alias TileLayer =  { minZoom: Int
                        , maxZoom: Int }


type alias TileCoord = { x: Int, y: Int, z: Int }

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


tileAtPoint : ZoomLevel -> LatLng -> Point
tileAtPoint zl ll =
  let
    pp = project ll
    s = scale zl
  in
    transform epsg3857Transform s pp
