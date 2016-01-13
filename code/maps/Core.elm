module Maps.Core where

import Maps.Geography exposing (..)
import Maps.Geometry exposing (..)
import Maps.Util as U

import Html exposing (Html)

type alias EventState = { lastPoint: Maybe Point }

type alias MapState =
  { center: LatLng
  , size: Size
  , zoom: ZoomLevel }

type MapAction = DblClick Point
               | MouseDown Point
               | MouseUp Point
               | MouseMoved Point
               | MouseDragged Point Point
               -- | LayerClick LatLng Point

type alias Layer =
  { renderer: MapState -> Signal.Address MapAction -> Html
  , opaque: Bool }

type alias Map = { state: MapState
                 , layers: List Layer
                 , events: EventState }


defaultEventState : EventState
defaultEventState = { lastPoint = Nothing }

emptyMap : Map
emptyMap =  { state = { center = {lat=0, lng=0}
                      , size = {w=0, h=0}
                      , zoom = zoomLevel 0 }
            , layers = []
            , events = defaultEventState }
