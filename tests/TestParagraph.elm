module TestParagraph exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Paragraph exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "paragraph"
        [ test "consistency test; max = 80" <|
            \_ ->
                format { maximumWidth = 80, optimalWidth = round (0.9 * 80), stringWidth = String.length } paragraph1
                    |> Expect.equal expected80
        , test "consistency test; max = 90 " <|
            \_ ->
                format { maximumWidth = 90, optimalWidth = round (0.9 * 90), stringWidth = String.length } paragraph1
                    |> Expect.equal expected90
        ]


expected90 =
    "Þá er Ísland fannst og byggðist af Noregi, var Adríánus páfi í Róma og Jóhannes eftir\nhann, sá er hinn fimmti var með því nafni í postuligu sæti, en Hlöðver Hlöðversson\nkeisari fyrir norðan fjall, en Leó og Alexander son hans yfir Miklagarði; þá var\nHaraldur hárfagri konungur yfir Noregi, en Eiríkur Eymundarson í Svíþjóð og Björn\nson hans, en Gormur hinn gamli að Danmörk, en Elfráður hinn ríki í Englandi og\nJátvarður son hans, en Kjarvalur að Dyflinni, Sigurður jarl hinn ríki í Orkneyjum."


expected80 =
    "Þá er Ísland fannst og byggðist af Noregi, var Adríánus páfi í Róma og\nJóhannes eftir hann, sá er hinn fimmti var með því nafni í postuligu sæti,\nen Hlöðver Hlöðversson keisari fyrir norðan fjall, en Leó og Alexander\nson hans yfir Miklagarði; þá var Haraldur hárfagri konungur yfir Noregi,\nen Eiríkur Eymundarson í Svíþjóð og Björn son hans, en Gormur hinn gamli\nað Danmörk, en Elfráður hinn ríki í Englandi og Játvarður son hans, en\nKjarvalur að Dyflinni, Sigurður jarl hinn ríki í Orkneyjum."


paragraph1 =
    "Þá er Ísland fannst og byggðist af Noregi, var Adríánus páfi í Róma og Jóhannes eftir hann, sá er hinn fimmti var með því nafni í postuligu sæti, en Hlöðver Hlöðversson keisari fyrir norðan fjall, en Leó og Alexander son hans yfir Miklagarði; þá var Haraldur hárfagri konungur yfir Noregi, en Eiríkur Eymundarson í Svíþjóð og Björn son hans, en Gormur hinn gamli að Danmörk, en Elfráður hinn ríki í Englandi og Játvarður son hans, en Kjarvalur að Dyflinni, Sigurður jarl hinn ríki í Orkneyjum."
