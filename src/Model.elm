module Model exposing (Model, init, KeyframeProperty)

import Msg exposing (..)

import Array exposing (Array)

-- Properties of an animation keyframe
type alias KeyframeProperty =
    { time: String
    , spline: String
    }
-- Model

type alias Model =
    { pathData: List (Int, Array (String))
    , strokeWidth: String
    , strokeColor: String
    , fillColor: String
    , duration: String
    , repeatCount: String
    , begin: String
    , pointInsertIndex: Maybe Int
    , focusedPointField: Maybe (Int, Int)
    , animationFrameAmount: Int
    , keyframeProperties: Array KeyframeProperty
    }

init : (Model, Cmd Msg)
init =
    ( { pathData = []
      , strokeWidth = "5"
      , strokeColor = "black"
      , fillColor = "none"
      , duration = "1s"
      , repeatCount = "indefinite"
      , begin = "0s"
      , pointInsertIndex = Nothing
      , focusedPointField = Nothing
      , animationFrameAmount = 1
      , keyframeProperties = Array.fromList [{time = "", spline = ""}]
      }
    , Cmd.none)


