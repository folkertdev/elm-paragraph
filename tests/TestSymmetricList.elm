module TestSymmetricList exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import SymmetricList exposing (..)
import Test exposing (..)


example =
    empty
        |> cons 42
        |> cons 1
        |> cons 2


suite : Test
suite =
    describe "symmetric list"
        [ test "singleton x = cons x empty" <|
            \_ ->
                singleton 42
                    |> Expect.equal (cons 42 empty)
        , test "length (singleton x) = 1" <|
            \_ ->
                singleton 42
                    |> length
                    |> Expect.equal 1
        , test "head picks correct item" <|
            \_ ->
                head example
                    |> Expect.equal (Just 2)
        , test "last picks correct item" <|
            \_ ->
                empty
                    |> cons 1
                    |> cons 2
                    |> last
                    |> Expect.equal (Just 1)
        , test "cons adds to the front" <|
            \_ ->
                empty
                    |> cons 1
                    |> cons 2
                    |> head
                    |> Expect.equal (Just 2)
        , test "snoc adds to the back" <|
            \_ ->
                empty
                    |> snoc 1
                    |> snoc 2
                    |> head
                    |> Expect.equal (Just 1)
        , test "tail behaves as expected" <|
            \_ ->
                List.range 1 10
                    |> fromList
                    |> tail
                    |> toList
                    |> Expect.equal (List.range 2 10)
        , test "init behaves as expected" <|
            \_ ->
                List.range 1 10
                    |> fromList
                    |> init
                    |> toList
                    |> Expect.equal (List.range 1 9)
        ]
