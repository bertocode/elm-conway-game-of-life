module Main exposing (..)

import Browser
import Html exposing (Html, button, div, node, option, select, text)
import Html.Attributes as Attr exposing (class, classList, href, rel, selected, value)
import Html.Events exposing (onClick, onInput)
import Random
import Set exposing (Set)
import Time


gridSize : ( Int, Int )
gridSize =
    ( 30, 25 )


type Msg
    = Play
    | Pause
    | ToggleGridElement ( Int, Int )
    | ChangeTickSpeed String
    | Reset
    | Tick Time.Posix
    | GetRandomGameOfLife
    | NewGrid (Set ( Int, Int ))


type alias Model =
    { gameState : GameState
    , activeCells : Set ( Int, Int )
    , tickSpeed : TickSpeed
    , gridSize : ( Int, Int )
    }


type GameState
    = Playing
    | Paused


type TickSpeed
    = Fast
    | Normal
    | Slow


init : ( Model, Cmd Msg )
init =
    ( { gameState = Paused
      , activeCells = Set.empty
      , tickSpeed = Normal
      , gridSize = gridSize
      }
    , Cmd.none
    )


tickSpeedToTick : TickSpeed -> Float
tickSpeedToTick tickSpeed =
    case tickSpeed of
        Slow ->
            1000.0

        Normal ->
            600.0

        Fast ->
            300.0


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (tickSpeedToTick model.tickSpeed) Tick
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
            ( { model | activeCells = Set.empty, gameState = Paused }, Cmd.none )

        Tick _ ->
            case model.gameState of
                Paused ->
                    ( model, Cmd.none )

                Playing ->
                    ( { model | activeCells = evolveCells gridSize model.activeCells }, Cmd.none )

        ChangeTickSpeed tickString ->
            let
                tickSpeed =
                    case tickString of
                        "slow" ->
                            Slow

                        "fast" ->
                            Fast

                        _ ->
                            Normal
            in
            ( { model | tickSpeed = tickSpeed }, Cmd.none )

        GetRandomGameOfLife ->
            let
                ( gridX, gridY ) =
                    model.gridSize

                randomGrid =
                    Random.int -(max gridX gridY) (max gridX gridY)
                        |> Random.andThen (\len -> Random.list len <| Random.pair (Random.int -gridX gridX) (Random.int -gridY gridY))
                        |> Random.map Set.fromList
                        |> Random.map
                            (Set.filter
                                (\( x, y ) ->
                                    if x > gridX || y > gridY || x < -gridX || y < -gridY then
                                        False

                                    else
                                        True
                                )
                            )
            in
            ( model, Random.generate NewGrid randomGrid )

        NewGrid newGrid ->
            ( { model | activeCells = newGrid }, Cmd.none )


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
        , div [ class "button-bar" ]
            [ case model.gameState of
                Playing ->
                    button [ onClick Pause ] [ text "Pause" ]

                Paused ->
                    button [ onClick Play ] [ text "Play" ]
            , button [ onClick Reset ] [ text "Reset" ]
            , select [ onInput ChangeTickSpeed ]
                [ option [ value "fast" ] [ text "Fast" ]
                , option [ value "normal", selected True ] [ text "Normal" ]
                , option [ value "slow" ] [ text "Slow" ]
                ]
            , button [ onClick GetRandomGameOfLife ] [ text "Get Random" ]
            ]
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
