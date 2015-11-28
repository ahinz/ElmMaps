module Maps.Layer where

import Regex as R

import Maps.Core as M
import Maps.Util as U

import Maps.Geometry exposing (..)
import Maps.Geography exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class, id, src)


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
    w' = toFloat (truncate (w / tileSize.w / 2.0 + 0.5))
    h' = toFloat (truncate (h / tileSize.h / 2.0 + 0.5))

    p1 = point (x - w') (y - h')
    p2 = point (x + w') (y + h')
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


pixelOrigin : M.MapState -> Size -> Point
pixelOrigin {zoom, center} tileSize =
  let
    {x,y} = tileAtPoint zoom (project center)
    xoff = (x - trim x) * tileSize.w
    yoff = (y - trim y) * tileSize.h
  in
    point xoff yoff

tileOffsetPoint : TileCoord -> Point
tileOffsetPoint {xoff, yoff} = point (toFloat xoff) (toFloat yoff)

tilePosition : Size -> Point -> TileCoord -> Point
tilePosition size origin coord =
  ((tileOffsetPoint coord) `scalePoint` size) `subtractPoint` origin

px : Float -> String
px f = (toString <| truncate <| f) ++ "px"

createTileImg : Size -> String -> Point -> Html
createTileImg {w,h} url {x,y} =
  let
    istyle = style [ ("width", px w)
                   , ("height", px h)
                   , ("position", "absolute")
                   , ("left", px x)
                   , ("top", px y)]
  in
    img [ src url
        , istyle ] []

renderTileLayer : TileLayer -> M.MapState -> Html
renderTileLayer {url, tileSize, minZoom, maxZoom} map =
  let
    c = project map.center
    origin = pixelOrigin map tileSize

    tiles = tilesForView map.zoom tileSize c map.size

    pos = tilePosition tileSize origin

    positions = List.map pos tiles
    urls = tilesToUrls url tiles

    imgs = List.map2 (createTileImg tileSize) urls positions
  in
    div [] imgs


createTileLayer : TileLayer -> M.Layer
createTileLayer tl =
  { renderer = renderTileLayer tl
  , opaque = tl.opaque }
