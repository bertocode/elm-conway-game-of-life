module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (evolveCells)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Game of Life cell tester"
        [ describe "evolveCells"
            [ test "a lonely cell should die" <|
                \_ ->
                    let
                        gridSize =
                            ( 1, 1 )

                        lonelyCell =
                            Set.singleton ( 0, 0 )
                    in
                    Expect.equal Set.empty (evolveCells gridSize lonelyCell)
            , test "cell with two neighbours should remain to next generation" <|
                \_ ->
                    let
                        gridSize =
                            ( 1, 1 )

                        cellsAlive =
                            Set.fromList [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]

                        expectedAliveCell =
                            ( 0, 0 )
                    in
                    Expect.equal True (Set.member expectedAliveCell (evolveCells gridSize cellsAlive))
            , test "cell with three non-cosecutive neighbours should be alive next generation" <|
                \_ ->
                    let
                        gridSize =
                            ( 1, 1 )

                        cellsAlive =
                            Set.fromList [ ( -1, -1 ), ( -1, 1 ), ( 1, 0 ) ]

                        expectedAliveCell =
                            ( 0, 0 )
                    in
                    Expect.equal True (Set.member expectedAliveCell (evolveCells gridSize cellsAlive))
            , test "cell should die from over population if more than 3 neighbours" <|
                \_ ->
                    let
                        gridSize =
                            ( 1, 1 )

                        cellsAlive =
                            Set.fromList [ ( -1, -1 ), ( -1, 0 ), ( 0, 0 ), ( -1, 1 ), ( 1, 0 ) ]

                        expectedDeadCell =
                            ( 0, 0 )
                    in
                    Expect.equal False (Set.member expectedDeadCell (evolveCells gridSize cellsAlive))
            ]
        ]
