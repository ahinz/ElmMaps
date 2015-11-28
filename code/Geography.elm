module Geography where

import Geometry exposing (..)
import Math exposing (ln, exp, deg2rad, rad2deg)

type ZoomLevel = ZoomLevel Float

type alias LatLng = { lat: Float
                    , lng: Float }

type alias LatLngBounds = { minp: LatLng
                          , maxp: LatLng }

zoomLevel : Int -> ZoomLevel
zoomLevel = toFloat >> ZoomLevel

zoom : ZoomLevel -> Float
zoom (ZoomLevel z) = z

earthRadius : Float
earthRadius = 6378137.0

maxLatitude : Float
maxLatitude = 85.0511287798

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

scale : ZoomLevel -> Float
scale (ZoomLevel zoom) = 2.0 ^ zoom

scalePoint : Point -> Size -> Point
scalePoint {x,y} {w,h} = point (w * x) (h * y)

distSq : Point -> Point -> Float
distSq {x,y} p = (x - p.x)*(x - p.x) + (y - p.y)*(y - p.y)

subtractPoint : Point -> Point -> Point
subtractPoint {x,y} p = point (x - p.x) (y - p.y)

addPoint : Point -> Point -> Point
addPoint {x,y} p = point (x + p.x) (y + p.y)
