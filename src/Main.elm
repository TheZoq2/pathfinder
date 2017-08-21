
import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes


-- Model

type alias Model =
    { pathData: String
    , strokeWidth: String
    , strokeColor: String
    , fillColor: String
    }

init : (Model, Cmd Msg)
init =
    (Model "" "1" "black" "none", Cmd.none)


-- Messages

type Msg
    = PathUpdated String
    | StrokeWidthUpdated String
    | StrokeColorUpdated String
    | FillColorUpdated String


-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PathUpdated pathString ->
            ({model | pathData = pathString}, Cmd.none)
        StrokeWidthUpdated value ->
            ({model | strokeWidth = value}, Cmd.none)
        StrokeColorUpdated value ->
            ({model | strokeColor = value}, Cmd.none)
        FillColorUpdated value ->
            ({model | fillColor = value}, Cmd.none)


-- View

inputField : String -> (String -> Msg) -> Html Msg
inputField name msg =
    div []
        [ div []
             [ label [] [text name]
             ]
        , div []
            [ input [onInput msg, style [("width", "100%")]] []
            ]
        ]

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ inputField "path" PathUpdated
            , inputField "stroke-width" StrokeWidthUpdated
            , inputField "stroke-color" StrokeColorUpdated
            , inputField "fill-color" FillColorUpdated
            ]
        , drawSvg model
        ]



drawSvg : Model -> Html Msg
drawSvg model =
    let
        path = Svg.path
            [ Svg.Attributes.d model.pathData
            ]
            []
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
