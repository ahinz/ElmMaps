module Maps.Layer where

import Regex as R

import Maps.Core as M
import Maps.Util as U

import Maps.Geometry exposing (..)
import Maps.Geography exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class, id, src, key)


type alias TileLayer =
  { url: String
  , tileSize: Size
  , minZoom: ZoomLevel
  , maxZoom: ZoomLevel
  , opaque: Bool
  }

type alias TileCoord =
  { x: Int, y: Int, z: Int, xoff: Int, yoff: Int }

tileUrlBuilder : String -> String -> TileCoord -> String
tileUrlBuilder url s {x,y,z} =
  let
    matcher = \q -> let m = q.match
                    in
                      if m == "{s}" then
                        s
                      else if m == "{x}" then
                        toString x
                      else if m == "{y}" then
                        toString y
                      else if m == "{z}" then
                        toString z
                      else
                        m
  in
    R.replace R.All (R.regex "{s}|{x}|{y}|{z}") matcher url

tilesToUrls : String -> List TileCoord -> List String
tilesToUrls url coords =
  let
    abc = U.cycle ["a","b","c"] (List.length coords)
    build = tileUrlBuilder url
  in
    List.map2 build abc coords

tileBoundsForView : ZoomLevel -> Size -> Point -> Size -> Bounds
tileBoundsForView zl tileSize p {w,h} =
  let
    {x,y} = roundPoint (tileAtPoint zl p)
    x' = toFloat <| truncate (x / tileSize.w)
    y' = toFloat <| truncate (y / tileSize.h)

    w' = toFloat (truncate (w / tileSize.w + 0.5))
    h' = toFloat (truncate (h / tileSize.h + 0.5))

    p1 = point (x' - w') (y' - h')
    p2 = point (x' + w') (y' + h')
  in
    bounds [p1, p2]

tilesForView : ZoomLevel -> Size -> Point -> Size -> List TileCoord
tilesForView zl tileSize center s =
  let
    {minp, maxp} = tileBoundsForView zl tileSize center s

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


pixelOrigin : M.MapState -> Point
pixelOrigin {zoom, center, size} =
  let
    {x,y} = tileAtPoint zoom (project center)
    xoff = x - size.w / 2.0
    yoff = y - size.h / 2.0
  in
    point xoff yoff

tilePoint : Size -> TileCoord -> Point
tilePoint {w, h} {x, y} =
  point ((toFloat x) * w) ((toFloat y) * h)

tilePosition : ZoomLevel -> Size -> Point -> TileCoord -> Point
tilePosition zl s origin coord =
  let
    coordPt = tilePoint s coord
  in
    coordPt `subtractPoint` origin

px : Float -> String
px f = (toString <| truncate <| f) ++ "px"

createTileImg : Size -> String -> Point -> TileCoord -> Html
createTileImg {w,h} url {x,y} tileCoord =
  let
    istyle = style [ ("width", px w)
                   , ("height", px h)
                   , ("position", "absolute")
                   , ("left", px x)
                   , ("top", px y)]
  in
    img [ src url
        , key <| "point:"
                ++ (toString <| tileCoord.x) ++ ":"
                ++ (toString <| tileCoord.y) ++ ":"
                ++ (toString <| tileCoord.z)
        , istyle ] []

renderTileLayer : TileLayer -> M.MapState -> Signal.Address M.MapAction -> Html
renderTileLayer {url, tileSize, minZoom, maxZoom} map _ =
  let
    c = project map.center
    origin = pixelOrigin map

    tiles = tilesForView map.zoom tileSize c map.size

    pos = tilePosition map.zoom tileSize origin

    positions = List.map pos tiles
    urls = tilesToUrls url tiles

    imgs = List.map3 (createTileImg tileSize) urls positions tiles
  in
    div [] imgs


createTileLayer : TileLayer -> M.Layer
createTileLayer tl =
  { renderer = renderTileLayer tl
  , opaque = tl.opaque }
