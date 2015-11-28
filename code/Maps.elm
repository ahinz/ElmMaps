module Maps where

import Geography exposing (..)
import Geometry exposing (..)

type alias TileLayer =  { minZoom: Int
                        , maxZoom: Int }

type alias TileCoord = { x: Int, y: Int, z: Int, xoff: Int, yoff: Int }

type alias EventState = { lastPoint: Maybe Point }

type alias Map = { center: LatLng
                 , size: Size
                 , zoom: ZoomLevel
                 , tileSize: Size
                 , events: EventState }

defaultEventState : EventState
defaultEventState = { lastPoint = Nothing }

emptyMap : Map
emptyMap =  { center = {lat=0, lng=0}
            , size = {w=0, h=0}
            , zoom = zoomLevel 0
            , tileSize = {w=0, h=0}
            , events = defaultEventState }


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
                        , z=truncate (zoom zl)
                        , xoff=x-xmin
                        , yoff=y-ymin}) pairs


pixelOrigin : Map -> Point
pixelOrigin {zoom, tileSize, center} =
  let
    {x,y} = tileAtPoint zoom (project center)
    xoff = (x - trim x) * tileSize.w
    yoff = (y - trim y) * tileSize.h
  in
    point xoff yoff


tilePosition : Size -> Point -> Point -> Point
tilePosition size origin coord =
  (coord `scalePoint` size) `subtractPoint` origin
