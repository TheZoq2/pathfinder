
import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes
import Result
import Array exposing (Array)

import Msg exposing (Msg(..))
import Model exposing (..)


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

                keyframeProperties = Array.push (KeyframeProperty "" "") model.keyframeProperties
            in
                ( { model
                    | animationFrameAmount = animationFrameAmount
                    , pathData = pathData
                    , keyframeProperties = keyframeProperties
                  }
                , Cmd.none
                )
        KeyframeTimeChanged id val ->
            let
                oldValue = Maybe.withDefault (KeyframeProperty "" "") <| Array.get id model.keyframeProperties
                newValue = {oldValue | time = val}

                newProperties = Array.set id newValue model.keyframeProperties
            in
                ({model | keyframeProperties = newProperties}, Cmd.none)
        KeyframeSplineChanged id val ->
            let
                oldValue = Maybe.withDefault (KeyframeProperty "" "") <| Array.get id model.keyframeProperties
                newValue = {oldValue | spline = val}

                newProperties = Array.set id newValue model.keyframeProperties
            in
                ({model | keyframeProperties = newProperties}, Cmd.none)


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


keyframeInputFields : Model -> Html Msg
keyframeInputFields model =
    let
        row id =
            tr []
                [ td [] [input [onInput <| KeyframeTimeChanged id, placeholder "time"] []]
                , td [] [input [onInput <| KeyframeSplineChanged id, placeholder "spline"] []]
                ]
    in
        table []
            <| (++) [tr [] [td [] [text "time"], td [] [text "spline"]]] 
                <| List.map row
                <| List.range 0 (model.animationFrameAmount-1)

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
        , keyframeInputFields model
        , debugOutput model
        ]


debugOutput : Model -> Html Msg
debugOutput model =
    div []
        [ p [] [text <| pathDataForFrame 0 model.pathData]
        , p [] [text <| getSplineString model]
        , p [] [text <| getTimeString model]
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



mergeKeyframeStrings : List String -> String
mergeKeyframeStrings values =
    List.foldr (++) "" <| List.intersperse ";" <| values

getSplineString : Model -> String
getSplineString model =
    mergeKeyframeStrings
        <| List.map (\val -> val.spline)
        <| Array.toList
        <| Array.slice 1 model.animationFrameAmount model.keyframeProperties

getTimeString : Model -> String
getTimeString model =
    mergeKeyframeStrings <| List.map (\val -> val.time) <|Array.toList model.keyframeProperties


animationValues : Model -> String
animationValues model =
    mergeKeyframeStrings
        <| List.map (\id -> pathDataForFrame id model.pathData) 
        <| List.range 0 (model.animationFrameAmount - 1)

animationFrames : Model -> List (Svg.Svg Msg)
animationFrames model =
    if model.animationFrameAmount > 1 then
        [ Svg.animate
            [ Svg.Attributes.attributeName "d"
            , Svg.Attributes.values <| animationValues model
            , Svg.Attributes.dur model.duration
            , Svg.Attributes.begin model.begin
            , Svg.Attributes.repeatCount model.repeatCount
            , Svg.Attributes.keyTimes <| getTimeString model
            , Svg.Attributes.keySplines <| getSplineString model
            , Svg.Attributes.calcMode "spline"
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
