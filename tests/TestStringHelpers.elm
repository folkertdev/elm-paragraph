module TestStringHelpers exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Paragraph
import StringHelpers exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "string helpers"
        [ wordsKeepingWhitespace
        , test "undo is like String.join" <|
            \_ ->
                StringHelpers.undo 42 [ [ 1, 2 ], [ 3, 4 ], [ 64 ] ]
                    |> Expect.equal [ 1, 2, 42, 3, 4, 42, 64 ]
        , test "do is like split" <|
            \_ ->
                StringHelpers.do 3 [ 1, 2, 3, 4, 5 ]
                    |> Expect.equal [ [ 1, 2 ], [ 4, 5 ] ]
        , test "paras splits on empty list" <|
            \_ ->
                StringHelpers.paras [ [ 1, 2 ], [], [ 4, 5 ] ]
                    |> Expect.equal [ [ [ 1, 2 ] ], [ [ 4, 5 ] ] ]
        , test "paras does nothing for non-empty lists" <|
            \_ ->
                StringHelpers.paras [ [ 1, 2 ], [ 4, 5 ] ]
                    |> Expect.equal [ [ [ 1, 2 ], [ 4, 5 ] ] ]
        , test "unparas << paras = id for non-empty list" <|
            \_ ->
                StringHelpers.paras [ [ 1, 2 ], [ 4, 5 ] ]
                    |> StringHelpers.unparas
                    |> Expect.equal [ [ 1, 2 ], [ 4, 5 ] ]
        , test "unparas << paras = id for empty list" <|
            \_ ->
                StringHelpers.paras [ [ 1, 2 ], [], [ 4, 5 ] ]
                    |> StringHelpers.unparas
                    |> Expect.equal [ [ 1, 2 ], [], [ 4, 5 ] ]
        , test "parse" <|
            \_ ->
                StringHelpers.parse paragraph1
                    |> Expect.equal paragraph1Words
        , test "parse empty string" <|
            \_ ->
                StringHelpers.parse ""
                    |> Expect.equal [ [ [ "" ] ] ]
        , test "parse single space" <|
            \_ ->
                StringHelpers.parse " "
                    |> Expect.equal [ [ [ "", " " ] ] ]
        , test "unparse" <|
            \_ ->
                StringHelpers.unparse paragraph1Words
                    |> Expect.equal paragraph1
        , fuzz Fuzz.string "unparse << parse = id" <|
            \string ->
                let
                    sanitize =
                        String.replace "\t" "" << String.replace "\n" ""
                in
                StringHelpers.parse (sanitize string)
                    |> StringHelpers.unparse
                    |> Expect.equal (String.trimLeft (sanitize string))
        , fuzz (Fuzz.list (Fuzz.list Fuzz.int)) "unparas << paras = id" <|
            \list ->
                let
                    sanitize =
                        List.filter (not << List.isEmpty)
                in
                StringHelpers.paras (sanitize list)
                    |> StringHelpers.unparas
                    |> Expect.equal (sanitize list)
        ]


wordsKeepingWhitespace : Test
wordsKeepingWhitespace =
    describe "wordsKeepingWhitespace"
        [ test "spaces get added at the front of the word" <|
            \_ ->
                StringHelpers.wordsKeepingWhitespace "one two three four"
                    |> Expect.equal [ "one", " two", " three", " four" ]
        , test "spaces in the middle are untouched" <|
            \_ ->
                StringHelpers.wordsKeepingWhitespace "one two     three four"
                    |> Expect.equal [ "one", " two", "     three", " four" ]
        , test "leading spaces are untouched" <|
            -- TODO is this what we want?
            \_ ->
                StringHelpers.wordsKeepingWhitespace "  one two three four"
                    |> Expect.equal [ "", "  one", " two", " three", " four" ]
        , test "trailing spaces are untouched" <|
            -- TODO is this what we want?
            \_ ->
                StringHelpers.wordsKeepingWhitespace "one two three four  "
                    |> Expect.equal [ "one", " two", " three", " four", "  " ]
        ]


paragraph1 =
    "Þá er Ísland fannst og byggðist af Noregi, var Adríánus páfi í Róma og Jóhannes eftir hann, sá er hinn fimmti var með því nafni í postuligu sæti, en Hlöðver Hlöðversson keisari fyrir norðan fjall, en Leó og Alexander son hans yfir Miklagarði; þá var Haraldur hárfagri konungur yfir Noregi, en Eiríkur Eymundarson í Svíþjóð og Björn son hans, en Gormur hinn gamli að Danmörk, en Elfráður hinn ríki í Englandi og Játvarður son hans, en Kjarvalur að Dyflinni, Sigurður jarl hinn ríki í Orkneyjum."


paragraph1Words =
    [ [ [ "Þá", " er", " Ísland", " fannst", " og", " byggðist", " af", " Noregi,", " var", " Adríánus", " páfi", " í", " Róma", " og", " Jóhannes", " eftir", " hann,", " sá", " er", " hinn", " fimmti", " var", " með", " því", " nafni", " í", " postuligu", " sæti,", " en", " Hlöðver", " Hlöðversson", " keisari", " fyrir", " norðan", " fjall,", " en", " Leó", " og", " Alexander", " son", " hans", " yfir", " Miklagarði;", " þá", " var", " Haraldur", " hárfagri", " konungur", " yfir", " Noregi,", " en", " Eiríkur", " Eymundarson", " í", " Svíþjóð", " og", " Björn", " son", " hans,", " en", " Gormur", " hinn", " gamli", " að", " Danmörk,", " en", " Elfráður", " hinn", " ríki", " í", " Englandi", " og", " Játvarður", " son", " hans,", " en", " Kjarvalur", " að", " Dyflinni,", " Sigurður", " jarl", " hinn", " ríki", " í", " Orkneyjum." ] ] ]
