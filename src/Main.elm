module Main exposing (..)

import Browser
import Html exposing (Html, button, div, node, text)
import Html.Attributes as Attr exposing (class, classList, href, rel)
import Html.Events exposing (onClick)
import Set exposing (Set)


gridSize : ( Int, Int )
gridSize =
    ( 30, 30 )


type Msg
    = Play
    | Pause
    | ToggleGridElement ( Int, Int )
    | Reset


type alias Model =
    { gameState : GameState
    , activeCells : Set ( Int, Int )
    }


type GameState
    = Playing
    | Paused


init : ( Model, Cmd Msg )
init =
    ( { gameState = Paused, activeCells = Set.empty }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            ( { model | gameState = Playing }, Cmd.none )

        Pause ->
            ( { model | gameState = Paused }, Cmd.none )

        ToggleGridElement ( x, y ) ->
            if Set.member ( x, y ) model.activeCells then
                ( { model | activeCells = Set.remove ( x, y ) model.activeCells }, Cmd.none )

            else
                ( { model | activeCells = Set.insert ( x, y ) model.activeCells }, Cmd.none )

        Reset ->
            ( { model | activeCells = Set.empty }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ node "link" [ rel "stylesheet", href "/styles.css" ] []
        , case model.gameState of
            Playing ->
                button [ onClick Pause ] [ text "Pause" ]

            Paused ->
                button [ onClick Play ] [ text "Play" ]
        , div [ class "game-of-life" ]
            [ renderGrid model ]
        ]


renderGrid : Model -> Html Msg
renderGrid model =
    div [ class "row" ] <|
        (List.range -(Tuple.first gridSize) (Tuple.first gridSize)
            |> List.map
                (\x ->
                    div [ class "column" ] <|
                        (List.range -(Tuple.second gridSize) (Tuple.second gridSize)
                            |> List.map
                                (\y ->
                                    div
                                        [ classList [ ( "item", True ), ( "alive", Set.member ( x, y ) model.activeCells ) ]
                                        , Attr.attribute "y" <| String.fromInt y
                                        , Attr.attribute "x" <| String.fromInt x
                                        , onClick <| ToggleGridElement ( x, y )
                                        ]
                                        []
                                )
                        )
                )
        )
