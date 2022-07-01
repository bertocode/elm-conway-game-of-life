module Main exposing (..)

import Browser
import Html exposing (Html, button, div, node, text)
import Html.Attributes as Attr exposing (class, classList, href, rel)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Time


gridSize : ( Int, Int )
gridSize =
    ( 30, 25 )


type Msg
    = Play
    | Pause
    | ToggleGridElement ( Int, Int )
    | Reset
    | Tick Time.Posix


type alias Model =
    { gameState : GameState
    , activeCells : Set ( Int, Int )
    }


type GameState
    = Playing
    | Paused


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        ]


neighbourOfACell : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
neighbourOfACell ( boardX, boardY ) ( x, y ) =
    [ ( x - 1, y - 1 ), ( x - 1, y ), ( x - 1, y + 1 ), ( x, y - 1 ), ( x, y + 1 ), ( x + 1, y - 1 ), ( x + 1, y ), ( x + 1, y + 1 ) ]
        |> List.filter
            (\( neighbourX, neighbourY ) ->
                if neighbourX < -boardX || neighbourY < -boardY || neighbourX > boardX || neighbourY > boardY then
                    False

                else
                    True
            )


evolveCells : ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
evolveCells grid activeCells =
    Set.toList activeCells
        |> List.map (neighbourOfACell grid)
        |> List.concat
        |> Set.fromList
        |> Set.union activeCells
        |> Set.filter
            (\( currentX, currentY ) ->
                let
                    currentNeighbours =
                        neighbourOfACell grid ( currentX, currentY )
                            |> List.filter (\( x, y ) -> Set.member ( x, y ) activeCells)
                            |> List.length
                in
                case currentNeighbours of
                    2 ->
                        if Set.member ( currentX, currentY ) activeCells then
                            True

                        else
                            False

                    3 ->
                        True

                    _ ->
                        False
            )


init : ( Model, Cmd Msg )
init =
    ( { gameState = Paused, activeCells = Set.empty }, Cmd.none )


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

        Tick _ ->
            case model.gameState of
                Paused ->
                    ( model, Cmd.none )

                Playing ->
                    ( { model | activeCells = evolveCells gridSize model.activeCells }, Cmd.none )


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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
