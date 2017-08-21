
import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes
import Result


-- Model

type alias Model =
    { pathData: List (Int, String)
    , strokeWidth: String
    , strokeColor: String
    , fillColor: String
    , pointInsertIndex: Maybe Int
    , focusedPointField: Maybe Int
    }

init : (Model, Cmd Msg)
init =
    (Model [] "1" "black" "none" Nothing Nothing, Cmd.none)


-- Messages

type Msg
    = PointUpdated Int String
    | AddPoint
    | RemovePoint Int
    | StrokeWidthUpdated String
    | StrokeColorUpdated String
    | FillColorUpdated String
    | PointInsertIndexChange String
    | AnimFieldFocused Int
    | AnimFieldBlurred


-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PointUpdated targetIndex pointString ->
            let
                updateFunc (index, val) =
                    if index == targetIndex then
                        (index, pointString)
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

                pathData = List.sortBy (\(index,_) -> index) <| ((targetIndex,"") :: withNewIds)
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
        PointInsertIndexChange value ->
            ({model | pointInsertIndex = Result.toMaybe <| String.toInt value}, Cmd.none)
        AnimFieldFocused id ->
            ({model | focusedPointField = Just id}, Cmd.none)
        AnimFieldBlurred ->
            ({model | focusedPointField = Nothing}, Cmd.none)


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
                textFieldAttributes =
                    ( if model.focusedPointField == Just id then
                        []
                    else
                        [value val]
                    )
                    ++
                        [ onInput (PointUpdated id)
                        , onFocus (AnimFieldFocused id)
                        , onBlur AnimFieldBlurred
                        ]
            in
            tr []
                [ td [] [text <| toString id]
                , td [] [input textFieldAttributes []]
                , td [] [button [onClick (RemovePoint id)] [text "x"]]
                ]
    in
        table []
            <| List.map
                pointRow
                model.pathData

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ pathInputFields model
            , div []
                [ button [onClick AddPoint] [text "Add point"]
                , input [onInput PointInsertIndexChange, placeholder "target id"] []
                ]
            , inputField "stroke-width" StrokeWidthUpdated
            , inputField "stroke-color" StrokeColorUpdated
            , inputField "fill-color" FillColorUpdated
            ]
        , drawSvg model
        , p [] [text <| pointsToPathData model.pathData]
        ]


pointsToPathData : List (Int, String) -> String
pointsToPathData points =
    List.foldr (++) ""
        <| List.map (\(_, val) -> val ++ " ") points


drawSvg : Model -> Html Msg
drawSvg model =
    let
        path = Svg.path [Svg.Attributes.d <| pointsToPathData model.pathData] []
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
