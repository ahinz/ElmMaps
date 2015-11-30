module Maps.Vector where

import Color as C

import Maps.Core as M
import Maps.Util as U

import Maps.Geometry exposing (..)
import Maps.Geography exposing (..)

import Graphics.Collage as GC
import Html exposing (..)

type VShape = SimpleFormShape { position: LatLng
                              , form: GC.Form }
            | DynamicScaleFormShape
              { position: LatLng
              , createShape: ZoomLevel -> LatLng -> GC.Shape
              , format: GC.Shape -> GC.Form }

formFromShape : ZoomLevel -> VShape -> (GC.Form, LatLng)
formFromShape zl v =
  case v of
    SimpleFormShape { position, form } -> (form, position)
    DynamicScaleFormShape { position, createShape, format } ->
      (format <| createShape zl position, position)


position : ZoomLevel -> Point -> VShape -> GC.Form
position zl offset formShape =
  let
    (form, position) = formFromShape zl formShape

    {x,y} = tileAtPoint zl <| project position
    offset2 = tileAtPoint zl offset

    dx = (x - offset2.x)
    dy = (y - offset2.y) * -1
  in
    GC.move (dx, dy) form

shapesToLayerRenderer : List VShape -> M.MapState -> Html
shapesToLayerRenderer shapes {center, size, zoom} =
  let
    w = round size.w
    h = round size.h

    offset = project center

    forms = List.map (position zoom offset) shapes
  in
    fromElement <| GC.collage w h forms

shapesToLayer : List VShape -> M.Layer
shapesToLayer v =
  { renderer = shapesToLayerRenderer v
  , opaque = False }


circleCreator : Float -> ZoomLevel -> LatLng -> GC.Shape
circleCreator r zl c =
  let
    pc = project c
    tc = tileAtPoint zl pc

    tr = point r 0 |> addPoint pc |> tileAtPoint zl
  in
    GC.circle <| abs (tr.x - tc.x)

createCircle : LatLng -> Float -> (GC.Shape -> GC.Form) -> VShape
createCircle c r f =
  DynamicScaleFormShape
  { position = c
  , createShape = circleCreator r
  , format = f }
