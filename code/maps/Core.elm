module Maps.Core where

import Maps.Geography exposing (..)
import Maps.Geometry exposing (..)
import Maps.Util as U

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

tileSize = 256

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

tilesForView : ZoomLevel -> Point -> Size -> List TileCoord
tilesForView zl center s =
  let
    {minp, maxp} = tileBoundsForView zl center s

    xmin = truncate minp.x
    ymin = truncate minp.y

    xmax = truncate maxp.x
    ymax = truncate maxp.y

    pairs = U.pairRange (xmin, ymin) (xmax, ymax)
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
