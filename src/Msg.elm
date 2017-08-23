module Msg exposing (Msg(..))


type Msg
    = PointUpdated Int Int String
    | AddPoint
    | RemovePoint Int
    | StrokeWidthUpdated String
    | StrokeColorUpdated String
    | FillColorUpdated String
    | DurationUpdated String
    | RepeatCountUpdated String
    | BeginUpdated String
    | PointInsertIndexChange String
    | AnimFieldFocused Int Int
    | AnimFieldBlurred
    | AddAnimationFrame
    | KeyframeTimeChanged Int String
    | KeyframeSplineChanged Int String

