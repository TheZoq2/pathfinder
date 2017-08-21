
import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes
import Result
import Array exposing (Array)


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
      }
    , Cmd.none)


-- Messages

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


-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PointUpdated targetPoint targetKey pointString ->
            let
                updateFunc (index, val) =
                    if index == targetPoint then
                        (index, Array.set targetKey pointString val)
                    else
                        (index, val)
                pathData =
                    List.map
                        updateFunc
                        model.pathData
            in
                ({model | pathData = pathData}, Cmd.none)
        AddPoint ->
            let
                targetIndex = case model.pointInsertIndex of
                    Just val -> val
                    Nothing -> List.length model.pathData

                newIdMaper (i,val) = if i >= targetIndex then (i+1, val) else (i,val)
                withNewIds = List.map newIdMaper model.pathData

                _ = Debug.log "With new ids" withNewIds

                pathData = List.sortBy (\(index,_) -> index) <| ((targetIndex, Array.repeat model.animationFrameAmount "") :: withNewIds)
                _ = Debug.log " " pathData
            in
                ({model | pathData = pathData}, Cmd.none)
        RemovePoint targetId ->
            let
                pathData =
                    List.indexedMap (,)
                    <| List.filterMap
                        (\(id, val) -> if id /= targetId then Just val else Nothing)
                        model.pathData
            in
                ({model | pathData = pathData}, Cmd.none)
        StrokeWidthUpdated value ->
            ({model | strokeWidth = value}, Cmd.none)
        StrokeColorUpdated value ->
            ({model | strokeColor = value}, Cmd.none)
        FillColorUpdated value ->
            ({model | fillColor = value}, Cmd.none)
        DurationUpdated value ->
            ({model | duration = value}, Cmd.none)
        RepeatCountUpdated value ->
            ({model | repeatCount = value}, Cmd.none)
        BeginUpdated value ->
            ({model | begin = value}, Cmd.none)
        PointInsertIndexChange value ->
            ({model | pointInsertIndex = Result.toMaybe <| String.toInt value}, Cmd.none)
        AnimFieldFocused id keyId ->
            ({model | focusedPointField = Just (id, keyId)}, Cmd.none)
        AnimFieldBlurred ->
            ({model | focusedPointField = Nothing}, Cmd.none)
        AddAnimationFrame ->
            let
                animationFrameAmount = model.animationFrameAmount+1

                pathUpdateFunction : (Int, Array String) -> (Int, Array String)
                pathUpdateFunction (id, data) =
                    ( id
                    , Array.fromList ((Array.toList data) ++ [""])
                    )

                pathData = List.map pathUpdateFunction model.pathData
            in
                ({model | animationFrameAmount = animationFrameAmount, pathData = pathData}, Cmd.none)


-- View

inputField : String -> (String -> Msg) -> Html Msg
inputField name msg =
    div []
        [ div []
             [ label [] [text name]
             ]
        , div []
            [ input [onInput msg] []
            ]
        ]

pathInputFields : Model -> Html Msg
pathInputFields model =
    let
        pointRow (id, val) =
            let
                textFieldAttributes keyIndex val =
                    ( if model.focusedPointField == Just (id,keyIndex) then
                        []
                    else
                        [value val]
                    )
                    ++
                        [ onInput (PointUpdated id keyIndex)
                        , onFocus (AnimFieldFocused id keyIndex)
                        , onBlur AnimFieldBlurred
                        ]

                textFields =
                    List.map (\(keyIndex, val) -> td [] [input (textFieldAttributes keyIndex val) []])
                    <| List.indexedMap (,)
                    <| Array.toList val
            in
                tr []
                    ( [ td [] [text <| toString id] ]
                      ++
                      textFields
                      ++
                      [ td [] [button [onClick (RemovePoint id)] [text "x"]]
                      ]
                    )
    in
        table []
            <| List.map
                pointRow
                model.pathData


propertyInputFields : Html Msg
propertyInputFields =
    div [style [("display", "flex"), ("flex-wrap", "wrap")]]
        [ inputField "stroke-width" StrokeWidthUpdated
        , inputField "stroke-color" StrokeColorUpdated
        , inputField "fill-color" FillColorUpdated
        , inputField "repeatCount" RepeatCountUpdated
        , inputField "duration" DurationUpdated
        , inputField "begin" BeginUpdated
        ]

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ pathInputFields model
            , div []
                [ button [onClick AddPoint] [text "Add point"]
                , input [onInput PointInsertIndexChange, placeholder "insert before"] []
                ]
            , div []
                [button [onClick AddAnimationFrame] [text "Add frame"]]
            , propertyInputFields
            ]
        , drawSvg model
        , p [] [text <| pathDataForFrame 0 model.pathData]
        ]


pointsToPathData : List String -> String
pointsToPathData points =
    List.foldr (++) ""
        <| points


pathDataForFrame : Int -> List (Int, Array String) -> String
pathDataForFrame frameIndex points =
    pointsToPathData
        <| List.map (Maybe.withDefault "default")
        <| List.map (\(_, vals) -> Array.get frameIndex vals) points


animationFrames : Model -> List (Svg.Svg Msg)
animationFrames model =
    if model.animationFrameAmount == 2 then
        [ Svg.animate
            [ Svg.Attributes.attributeName "d"
            , Svg.Attributes.from <| pathDataForFrame 0 model.pathData
            , Svg.Attributes.to <| pathDataForFrame 1 model.pathData
            , Svg.Attributes.dur model.duration
            , Svg.Attributes.begin model.begin
            , Svg.Attributes.repeatCount model.repeatCount
            ]
            []
        ]
    else
        []


drawSvg : Model -> Html Msg
drawSvg model =
    let
        path = Svg.path 
            [ Svg.Attributes.d <| pathDataForFrame 0 model.pathData]
            (animationFrames model)
    in
        Svg.svg
            [ Svg.Attributes.viewBox "0 0 100 100"
            , Svg.Attributes.width "150px"
            , Svg.Attributes.height "150px"
            , Svg.Attributes.strokeWidth model.strokeWidth
            , Svg.Attributes.stroke model.strokeColor
            , Svg.Attributes.fill model.fillColor
            ]
            [path]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- Program

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
