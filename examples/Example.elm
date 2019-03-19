module Example exposing (main)

{-| A small example app that uses a range input for the maximum width of some text.
-}

import Browser
import Dict exposing (Dict)
import Html
import Html.Attributes exposing (..)
import Html.Events
import Paragraph
import Svg
import Svg.Attributes exposing (fill, height, stroke, strokeDasharray, strokeWidth, transform, width, x1, x2, y1, y2)


type alias Model =
    { maximumWidth : String, optimalWidth : String }


type Msg
    = MaximumWidthChange String
    | OptimalWidthChange String


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( { maximumWidth = "1000", optimalWidth = "100" }, Cmd.none )
        , view = \model -> { title = "elm paragraph example", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MaximumWidthChange new ->
            ( { model | maximumWidth = new }, Cmd.none )

        OptimalWidthChange new ->
            ( { model | optimalWidth = new }, Cmd.none )


svgHeight : Int
svgHeight =
    700


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ googleFonts
        , Svg.svg [ width model.maximumWidth, height (String.fromInt svgHeight), style "border-right" "1px solid black" ] <|
            let
                maximumWidth =
                    String.toFloat model.maximumWidth
                        |> Maybe.withDefault 500

                optimalWidth =
                    String.toFloat model.optimalWidth
                        |> Maybe.withDefault 90
                        |> (\v -> (v / 100) * maximumWidth)

                paragraphs =
                    List.map
                        (Paragraph.lines
                            { maximumWidth = pixels maximumWidth
                            , optimalWidth = pixels optimalWidth
                            , stringWidth = stringWidth
                            }
                        )
                        kafli1Paragraphs
            in
            [ Svg.g [ width (String.fromInt (pixels maximumWidth)), Html.Attributes.style "border-right" "1px solid black", stroke "black", strokeWidth "2px" ] (drawParagraphs paragraphs)
            , Svg.line [ x1 "0", x2 "0", y1 "0", y2 (String.fromInt svgHeight), stroke "black", transform ("translate(" ++ String.fromFloat optimalWidth ++ ", 0)"), strokeDasharray "10 20", Svg.Attributes.opacity "0.5" ] []
            ]
        , Html.div []
            [ Html.input [ type_ "range", id "maximumWidth", name "maximumWidth", Html.Attributes.min "100", Html.Attributes.max "1000", value model.maximumWidth, step "10", Html.Events.onInput MaximumWidthChange ] []
            , Html.label [ for "maximumWidth" ] [ Html.text <| "Maximum Width: " ++ model.maximumWidth ++ "px" ]
            ]
        , Html.div []
            [ Html.input [ type_ "range", id "optimalWidth", name "optimalWidth", Html.Attributes.min "0", Html.Attributes.max "100", value model.optimalWidth, step "1", Html.Events.onInput OptimalWidthChange ] []
            , Html.label [ for "optimalWidth" ] [ Html.text <| "Optimal Width: " ++ model.optimalWidth ++ "%" ]
            ]
        ]


googleFonts : Html.Html msg
googleFonts =
    -- because JS loads the font, there is a small readjustment when the font gets loaded.
    Html.div []
        [ Html.node "style"
            []
            [ Html.text """.setFont { font-family: 'Source Sans Pro', sans-serif; font-style: italic; font-weight: 400; font-size: 1em;}"""
            ]
        , Html.node "link" [ Html.Attributes.href font.url, Html.Attributes.rel "stylesheet" ] []
        ]


{-| Some numbers based on my work decoding font files. These are specific to the font
-}
font =
    { lineHeight = 1253
    , unitsPerEm = 1000
    , pixelsAt1Em = 16
    , url = "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,400i"
    }


offset i =
    (toFloat (i * font.lineHeight) / toFloat font.unitsPerEm) * font.pixelsAt1Em


drawParagraphs paragraphs =
    let
        go linesSoFar remaining done =
            case remaining of
                [] ->
                    List.reverse done

                first :: rest ->
                    let
                        new =
                            drawParagraph
                                { offsetX = 0
                                , offsetY = offset linesSoFar -- toFloat (linesSoFar * font.lineHeight) / toFloat font.unitsPerEm * toFloat font.pixelsAt1Em |> ceiling |> toFloat
                                }
                                first
                    in
                    go (linesSoFar + List.length first) rest (new :: done)
    in
    go 0 paragraphs []


drawParagraph { offsetX, offsetY } lines =
    Svg.g [ transform ("translate(" ++ String.fromFloat 0 ++ ", " ++ String.fromFloat offsetY ++ ")") ]
        (List.indexedMap
            (\i line ->
                let
                    lineOffsetY =
                        offset (i + 1)
                in
                drawLine { offsetY = round lineOffsetY, offsetX = 0 } line
            )
            lines
        )


drawLine { offsetX, offsetY } text =
    Svg.text_
        [ fill "black"
        , stroke "none"
        , Svg.Attributes.class "setFont"
        , transform ("translate(" ++ String.fromInt 0 ++ ", " ++ String.fromInt offsetY ++ ")")
        ]
        [ Svg.text text ]


pixels width =
    -- these are some magic numbers based on the SourceSansPro font and font.pixelsAt1Empx font size
    round ((width / font.pixelsAt1Em) * toFloat font.unitsPerEm)


kafli1Paragraphs =
    String.split "\n\n" texts
        |> List.map (String.replace "\n" " ")


{-| Some text in various scripts
-}
texts =
    texts1


texts1 =
    """
# D

D

DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD DDDDDDDDDDDDDDDDDDDDDDDDDDDD DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD

# First paragraph of Landnámabok

Þá er Ísland fannst og byggðist af Noregi, var Adríánus páfi í Róma og Jóhannes eftir hann, sá er hinn fimmti var með því nafni í postuligu sæti, en Hlöðver Hlöðversson keisari fyrir norðan fjall, en Leó og Alexander son hans yfir Miklagarði; þá var Haraldur hárfagri konungur yfir Noregi, en Eiríkur Eymundarson í Svíþjóð og Björn son hans, en Gormur hinn gamli að Danmörk, en Elfráður hinn ríki í Englandi og Játvarður son hans, en Kjarvalur að Dyflinni, Sigurður jarl hinn ríki í Orkneyjum.

# The Frisian national anthem

Frysk bloed tsjoch op! Wol no ris brûze en siede,
en bûnzje troch ús ieren om!
Flean op! Wy sjonge it bêste lân fan d'ierde,
it Fryske lân fol eare en rom.

Klink dan en daverje fier yn it rûn
Dyn âlde eare, o Fryske grûn! (2x)

# The Ethiopian national anthem. The Amharic script is not part of the used font so the result can be slightly off.
For instance select the `ም` character. You'll see glyph is not fully contained in the selection.

የዜግነት ክብር በ ኢትዮጵያችን ጸንቶ
ታየ ሕዝባዊነት ዳር እስከዳር በርቶ
ለሰላም ለፍትህ ለሕዝቦች ነጻነት
በእኩልነት በፍቅር ቆመናል ባንድነት
መሠረተ ፅኑ ሰብዕናን ያልሻርን
ሕዝቦች ነን ለሥራ በሥራ የኖርን
ድንቅ የባህል መድረክ ያኩሪ ቅርስ ባለቤት
የተፈጥሮ ጸጋ የጀግና ሕዝብ እናት
እንጠብቅሻለን አለብን አደራ
ኢትዮጵያችን ኑሪ እኛም ባንቺ እንኩራ።

# Du gamla, du fria

Du gamla du fria du fjällhöga nord,
du tysta du glädjerika sköna,
jag hälsar dig värnaste land uppå jord,
din sol din himmel dina ängder gröna
din sol din himmel dina ängder gröna
    """


stringWidth =
    String.foldl (\c accum -> charWidth c + accum) 0


charWidth char =
    case Dict.get char sourceSansProItWidthDict.otherWidths of
        Nothing ->
            sourceSansProItWidthDict.defaultWidth

        Just w ->
            w


sourceSansProItWidthDict =
    { defaultWidth = 628
    , otherWidths =
        Dict.fromList
            [ ( ' ', 192 )
            , ( '!', 282 )
            , ( '"', 407 )
            , ( '#', 479 )
            , ( '$', 480 )
            , ( '%', 791 )
            , ( '&', 582 )
            , ( '\'', 242 )
            , ( '(', 291 )
            , ( ')', 293 )
            , ( '*', 403 )
            , ( '+', 480 )
            , ( ',', 242 )
            , ( '-', 299 )
            , ( '.', 242 )
            , ( '/', 340 )
            , ( '0', 480 )
            , ( '1', 480 )
            , ( '2', 480 )
            , ( '3', 480 )
            , ( '4', 480 )
            , ( '5', 480 )
            , ( '6', 480 )
            , ( '7', 480 )
            , ( '8', 480 )
            , ( '9', 480 )
            , ( ':', 242 )
            , ( ';', 242 )
            , ( '<', 480 )
            , ( '=', 480 )
            , ( '>', 480 )
            , ( '?', 409 )
            , ( '@', 814 )
            , ( 'A', 510 )
            , ( 'B', 583 )
            , ( 'C', 550 )
            , ( 'D', 592 )
            , ( 'E', 503 )
            , ( 'F', 473 )
            , ( 'G', 588 )
            , ( 'H', 622 )
            , ( 'I', 252 )
            , ( 'J', 456 )
            , ( 'K', 559 )
            , ( 'L', 462 )
            , ( 'M', 705 )
            , ( 'N', 619 )
            , ( 'O', 633 )
            , ( 'P', 550 )
            , ( 'Q', 633 )
            , ( 'R', 555 )
            , ( 'S', 506 )
            , ( 'T', 510 )
            , ( 'U', 616 )
            , ( 'V', 496 )
            , ( 'W', 756 )
            , ( 'X', 494 )
            , ( 'Y', 461 )
            , ( 'Z', 512 )
            , ( '[', 292 )
            , ( '\\', 340 )
            , ( ']', 293 )
            , ( '^', 479 )
            , ( '_', 483 )
            , ( '`', 517 )
            , ( 'a', 537 )
            , ( 'b', 536 )
            , ( 'c', 435 )
            , ( 'd', 535 )
            , ( 'e', 481 )
            , ( 'f', 282 )
            , ( 'g', 531 )
            , ( 'h', 519 )
            , ( 'i', 237 )
            , ( 'j', 238 )
            , ( 'k', 476 )
            , ( 'l', 248 )
            , ( 'm', 799 )
            , ( 'n', 525 )
            , ( 'o', 515 )
            , ( 'p', 535 )
            , ( 'q', 537 )
            , ( 'r', 342 )
            , ( 's', 402 )
            , ( 't', 325 )
            , ( 'u', 528 )
            , ( 'v', 448 )
            , ( 'w', 707 )
            , ( 'x', 429 )
            , ( 'y', 448 )
            , ( 'z', 410 )
            , ( '{', 290 )
            , ( '|', 231 )
            , ( '}', 293 )
            , ( '~', 480 )
            , ( '\u{00A0}', 192 )
            , ( '¡', 282 )
            , ( '¢', 480 )
            , ( '£', 480 )
            , ( '¤', 480 )
            , ( '¥', 480 )
            , ( '¦', 231 )
            , ( '§', 477 )
            , ( '¨', 517 )
            , ( '©', 665 )
            , ( 'ª', 354 )
            , ( '«', 412 )
            , ( '¬', 480 )
            , ( '\u{00AD}', 299 )
            , ( '®', 406 )
            , ( '¯', 517 )
            , ( '°', 313 )
            , ( '±', 480 )
            , ( '²', 353 )
            , ( '³', 351 )
            , ( '´', 517 )
            , ( 'µ', 526 )
            , ( '¶', 535 )
            , ( '·', 242 )
            , ( '¸', 517 )
            , ( '¹', 356 )
            , ( 'º', 348 )
            , ( '»', 412 )
            , ( '¼', 748 )
            , ( '½', 772 )
            , ( '¾', 764 )
            , ( '¿', 409 )
            , ( 'À', 510 )
            , ( 'Á', 510 )
            , ( 'Â', 510 )
            , ( 'Ã', 510 )
            , ( 'Ä', 510 )
            , ( 'Å', 510 )
            , ( 'Æ', 803 )
            , ( 'Ç', 550 )
            , ( 'È', 503 )
            , ( 'É', 503 )
            , ( 'Ê', 503 )
            , ( 'Ë', 503 )
            , ( 'Ì', 252 )
            , ( 'Í', 252 )
            , ( 'Î', 252 )
            , ( 'Ï', 252 )
            , ( 'Ð', 610 )
            , ( 'Ñ', 619 )
            , ( 'Ò', 633 )
            , ( 'Ó', 633 )
            , ( 'Ô', 633 )
            , ( 'Õ', 633 )
            , ( 'Ö', 633 )
            , ( '×', 480 )
            , ( 'Ø', 633 )
            , ( 'Ù', 616 )
            , ( 'Ú', 616 )
            , ( 'Û', 616 )
            , ( 'Ü', 616 )
            , ( 'Ý', 461 )
            , ( 'Þ', 562 )
            , ( 'ß', 555 )
            , ( 'à', 537 )
            , ( 'á', 537 )
            , ( 'â', 537 )
            , ( 'ã', 537 )
            , ( 'ä', 537 )
            , ( 'å', 537 )
            , ( 'æ', 761 )
            , ( 'ç', 435 )
            , ( 'è', 481 )
            , ( 'é', 481 )
            , ( 'ê', 481 )
            , ( 'ë', 481 )
            , ( 'ì', 237 )
            , ( 'í', 237 )
            , ( 'î', 237 )
            , ( 'ï', 237 )
            , ( 'ð', 524 )
            , ( 'ñ', 525 )
            , ( 'ò', 515 )
            , ( 'ó', 515 )
            , ( 'ô', 515 )
            , ( 'õ', 515 )
            , ( 'ö', 515 )
            , ( '÷', 480 )
            , ( 'ø', 515 )
            , ( 'ù', 528 )
            , ( 'ú', 528 )
            , ( 'û', 528 )
            , ( 'ü', 528 )
            , ( 'ý', 448 )
            , ( 'þ', 535 )
            , ( 'ÿ', 448 )
            , ( 'Ā', 510 )
            , ( 'ā', 537 )
            , ( 'Ă', 510 )
            , ( 'ă', 537 )
            , ( 'Ą', 510 )
            , ( 'ą', 537 )
            , ( 'Ć', 550 )
            , ( 'ć', 435 )
            , ( 'Ĉ', 550 )
            , ( 'ĉ', 435 )
            , ( 'Ċ', 550 )
            , ( 'ċ', 435 )
            , ( 'Č', 550 )
            , ( 'č', 435 )
            , ( 'Ď', 592 )
            , ( 'ď', 542 )
            , ( 'Đ', 610 )
            , ( 'đ', 535 )
            , ( 'Ē', 503 )
            , ( 'ē', 481 )
            , ( 'Ĕ', 503 )
            , ( 'ĕ', 481 )
            , ( 'Ė', 503 )
            , ( 'ė', 481 )
            , ( 'Ę', 503 )
            , ( 'ę', 481 )
            , ( 'Ě', 503 )
            , ( 'ě', 481 )
            , ( 'Ĝ', 588 )
            , ( 'ĝ', 531 )
            , ( 'Ğ', 588 )
            , ( 'ğ', 531 )
            , ( 'Ġ', 588 )
            , ( 'ġ', 531 )
            , ( 'Ģ', 588 )
            , ( 'ģ', 531 )
            , ( 'Ĥ', 622 )
            , ( 'ĥ', 519 )
            , ( 'Ħ', 643 )
            , ( 'ħ', 519 )
            , ( 'Ĩ', 252 )
            , ( 'ĩ', 237 )
            , ( 'Ī', 252 )
            , ( 'ī', 237 )
            , ( 'Ĭ', 252 )
            , ( 'ĭ', 237 )
            , ( 'Į', 252 )
            , ( 'į', 237 )
            , ( 'İ', 252 )
            , ( 'ı', 237 )
            , ( 'Ĵ', 456 )
            , ( 'ĵ', 238 )
            , ( 'Ķ', 559 )
            , ( 'ķ', 476 )
            , ( 'ĸ', 476 )
            , ( 'Ĺ', 462 )
            , ( 'ĺ', 248 )
            , ( 'Ļ', 462 )
            , ( 'ļ', 248 )
            , ( 'Ľ', 462 )
            , ( 'ľ', 242 )
            , ( 'Ŀ', 462 )
            , ( 'ŀ', 331 )
            , ( 'Ł', 462 )
            , ( 'ł', 252 )
            , ( 'Ń', 619 )
            , ( 'ń', 525 )
            , ( 'Ņ', 619 )
            , ( 'ņ', 525 )
            , ( 'Ň', 619 )
            , ( 'ň', 525 )
            , ( 'ŉ', 739 )
            , ( 'Ō', 633 )
            , ( 'ō', 515 )
            , ( 'Ŏ', 633 )
            , ( 'ŏ', 515 )
            , ( 'Ő', 633 )
            , ( 'ő', 515 )
            , ( 'Œ', 816 )
            , ( 'œ', 812 )
            , ( 'Ŕ', 555 )
            , ( 'ŕ', 342 )
            , ( 'Ŗ', 555 )
            , ( 'ŗ', 342 )
            , ( 'Ř', 555 )
            , ( 'ř', 342 )
            , ( 'Ś', 506 )
            , ( 'ś', 402 )
            , ( 'Ŝ', 506 )
            , ( 'ŝ', 402 )
            , ( 'Ş', 506 )
            , ( 'ş', 402 )
            , ( 'Š', 506 )
            , ( 'š', 402 )
            , ( 'Ţ', 510 )
            , ( 'ţ', 325 )
            , ( 'Ť', 510 )
            , ( 'ť', 325 )
            , ( 'Ũ', 616 )
            , ( 'ũ', 528 )
            , ( 'Ū', 616 )
            , ( 'ū', 528 )
            , ( 'Ŭ', 616 )
            , ( 'ŭ', 528 )
            , ( 'Ů', 616 )
            , ( 'ů', 528 )
            , ( 'Ű', 616 )
            , ( 'ű', 528 )
            , ( 'Ų', 616 )
            , ( 'ų', 528 )
            , ( 'Ŵ', 756 )
            , ( 'ŵ', 707 )
            , ( 'Ŷ', 461 )
            , ( 'ŷ', 448 )
            , ( 'Ÿ', 461 )
            , ( 'Ź', 512 )
            , ( 'ź', 410 )
            , ( 'Ż', 512 )
            , ( 'ż', 410 )
            , ( 'Ž', 512 )
            , ( 'ž', 410 )
            , ( 'ƀ', 519 )
            , ( 'Ə', 636 )
            , ( 'ƒ', 480 )
            , ( 'Ơ', 633 )
            , ( 'ơ', 517 )
            , ( 'Ư', 629 )
            , ( 'ư', 543 )
            , ( 'Ǎ', 510 )
            , ( 'ǎ', 537 )
            , ( 'Ǐ', 252 )
            , ( 'ǐ', 237 )
            , ( 'Ǒ', 633 )
            , ( 'ǒ', 515 )
            , ( 'Ǔ', 616 )
            , ( 'ǔ', 528 )
            , ( 'Ǖ', 616 )
            , ( 'ǖ', 528 )
            , ( 'Ǘ', 616 )
            , ( 'ǘ', 528 )
            , ( 'Ǚ', 616 )
            , ( 'ǚ', 528 )
            , ( 'Ǜ', 616 )
            , ( 'ǜ', 528 )
            , ( 'Ǧ', 588 )
            , ( 'ǧ', 531 )
            , ( 'Ǫ', 633 )
            , ( 'ǫ', 515 )
            , ( 'Ș', 506 )
            , ( 'ș', 402 )
            , ( 'Ț', 510 )
            , ( 'ț', 325 )
            , ( 'ȷ', 238 )
            , ( 'Ƀ', 610 )
            , ( 'ɑ', 537 )
            , ( 'ə', 478 )
            , ( 'ɡ', 531 )
            , ( 'ʰ', 353 )
            , ( 'ʲ', 164 )
            , ( 'ʳ', 224 )
            , ( 'ʷ', 479 )
            , ( 'ʸ', 307 )
            , ( 'ʹ', 242 )
            , ( 'ʻ', 242 )
            , ( 'ʼ', 242 )
            , ( 'ʾ', 151 )
            , ( 'ʿ', 163 )
            , ( 'ˆ', 517 )
            , ( 'ˇ', 517 )
            , ( 'ˈ', 108 )
            , ( 'ˉ', 262 )
            , ( 'ˊ', 213 )
            , ( 'ˋ', 213 )
            , ( 'ˌ', 108 )
            , ( '˘', 517 )
            , ( '˙', 517 )
            , ( '˚', 517 )
            , ( '˛', 517 )
            , ( '˜', 517 )
            , ( '˝', 517 )
            , ( 'ˡ', 165 )
            , ( 'ˢ', 271 )
            , ( 'ˣ', 295 )
            , ( 'π', 558 )
            , ( 'ᵃ', 354 )
            , ( 'ᵇ', 360 )
            , ( 'ᵈ', 365 )
            , ( 'ᵉ', 323 )
            , ( 'ᵍ', 354 )
            , ( 'ᵏ', 325 )
            , ( 'ᵐ', 536 )
            , ( 'ᵒ', 348 )
            , ( 'ᵖ', 358 )
            , ( 'ᵗ', 222 )
            , ( 'ᵘ', 355 )
            , ( 'ᵛ', 307 )
            , ( 'ᶜ', 292 )
            , ( 'ᶠ', 194 )
            , ( 'ᶻ', 279 )
            , ( 'Ḇ', 583 )
            , ( 'ḇ', 536 )
            , ( 'Ḍ', 592 )
            , ( 'ḍ', 535 )
            , ( 'Ḏ', 592 )
            , ( 'ḏ', 535 )
            , ( 'Ḗ', 503 )
            , ( 'ḗ', 481 )
            , ( 'Ḡ', 588 )
            , ( 'ḡ', 531 )
            , ( 'Ḥ', 622 )
            , ( 'ḥ', 519 )
            , ( 'Ḫ', 622 )
            , ( 'ḫ', 519 )
            , ( 'Ḵ', 559 )
            , ( 'ḵ', 476 )
            , ( 'Ḷ', 462 )
            , ( 'ḷ', 248 )
            , ( 'Ḹ', 462 )
            , ( 'ḹ', 248 )
            , ( 'Ḻ', 462 )
            , ( 'ḻ', 248 )
            , ( 'Ṃ', 705 )
            , ( 'ṃ', 799 )
            , ( 'Ṅ', 619 )
            , ( 'ṅ', 525 )
            , ( 'Ṇ', 619 )
            , ( 'ṇ', 525 )
            , ( 'Ṉ', 619 )
            , ( 'ṉ', 525 )
            , ( 'Ṓ', 633 )
            , ( 'ṓ', 515 )
            , ( 'Ṛ', 555 )
            , ( 'ṛ', 342 )
            , ( 'Ṝ', 555 )
            , ( 'ṝ', 342 )
            , ( 'Ṟ', 555 )
            , ( 'ṟ', 342 )
            , ( 'Ṡ', 506 )
            , ( 'ṡ', 402 )
            , ( 'Ṣ', 506 )
            , ( 'ṣ', 402 )
            , ( 'Ṭ', 510 )
            , ( 'ṭ', 325 )
            , ( 'Ṯ', 510 )
            , ( 'ṯ', 325 )
            , ( 'Ẁ', 756 )
            , ( 'ẁ', 707 )
            , ( 'Ẃ', 756 )
            , ( 'ẃ', 707 )
            , ( 'Ẅ', 756 )
            , ( 'ẅ', 707 )
            , ( 'Ẏ', 461 )
            , ( 'ẏ', 448 )
            , ( 'Ẓ', 512 )
            , ( 'ẓ', 410 )
            , ( 'ẗ', 325 )
            , ( 'ẞ', 633 )
            , ( 'Ạ', 510 )
            , ( 'ạ', 537 )
            , ( 'Ả', 510 )
            , ( 'ả', 537 )
            , ( 'Ấ', 510 )
            , ( 'ấ', 537 )
            , ( 'Ầ', 510 )
            , ( 'ầ', 537 )
            , ( 'Ẩ', 510 )
            , ( 'ẩ', 537 )
            , ( 'Ẫ', 510 )
            , ( 'ẫ', 537 )
            , ( 'Ậ', 510 )
            , ( 'ậ', 537 )
            , ( 'Ắ', 510 )
            , ( 'ắ', 537 )
            , ( 'Ằ', 510 )
            , ( 'ằ', 537 )
            , ( 'Ẳ', 510 )
            , ( 'ẳ', 537 )
            , ( 'Ẵ', 510 )
            , ( 'ẵ', 537 )
            , ( 'Ặ', 510 )
            , ( 'ặ', 537 )
            , ( 'Ẹ', 503 )
            , ( 'ẹ', 481 )
            , ( 'Ẻ', 503 )
            , ( 'ẻ', 481 )
            , ( 'Ẽ', 503 )
            , ( 'ẽ', 481 )
            , ( 'Ế', 503 )
            , ( 'ế', 481 )
            , ( 'Ề', 503 )
            , ( 'ề', 481 )
            , ( 'Ể', 503 )
            , ( 'ể', 481 )
            , ( 'Ễ', 503 )
            , ( 'ễ', 481 )
            , ( 'Ệ', 503 )
            , ( 'ệ', 481 )
            , ( 'Ỉ', 252 )
            , ( 'ỉ', 237 )
            , ( 'Ị', 252 )
            , ( 'ị', 237 )
            , ( 'Ọ', 633 )
            , ( 'ọ', 515 )
            , ( 'Ỏ', 633 )
            , ( 'ỏ', 515 )
            , ( 'Ố', 633 )
            , ( 'ố', 515 )
            , ( 'Ồ', 633 )
            , ( 'ồ', 515 )
            , ( 'Ổ', 633 )
            , ( 'ổ', 515 )
            , ( 'Ỗ', 633 )
            , ( 'ỗ', 515 )
            , ( 'Ộ', 633 )
            , ( 'ộ', 515 )
            , ( 'Ớ', 633 )
            , ( 'ớ', 517 )
            , ( 'Ờ', 633 )
            , ( 'ờ', 517 )
            , ( 'Ở', 633 )
            , ( 'ở', 517 )
            , ( 'Ỡ', 633 )
            , ( 'ỡ', 517 )
            , ( 'Ợ', 633 )
            , ( 'ợ', 517 )
            , ( 'Ụ', 616 )
            , ( 'ụ', 528 )
            , ( 'Ủ', 616 )
            , ( 'ủ', 528 )
            , ( 'Ứ', 629 )
            , ( 'ứ', 543 )
            , ( 'Ừ', 629 )
            , ( 'ừ', 543 )
            , ( 'Ử', 629 )
            , ( 'ử', 543 )
            , ( 'Ữ', 629 )
            , ( 'ữ', 543 )
            , ( 'Ự', 629 )
            , ( 'ự', 543 )
            , ( 'Ỳ', 461 )
            , ( 'ỳ', 448 )
            , ( 'Ỵ', 461 )
            , ( 'ỵ', 448 )
            , ( 'Ỷ', 461 )
            , ( 'ỷ', 448 )
            , ( 'Ỹ', 461 )
            , ( 'ỹ', 448 )
            , ( '\u{2007}', 480 )
            , ( '‒', 480 )
            , ( '–', 463 )
            , ( '—', 769 )
            , ( '―', 769 )
            , ( '‘', 242 )
            , ( '’', 242 )
            , ( '‚', 242 )
            , ( '“', 407 )
            , ( '”', 407 )
            , ( '„', 407 )
            , ( '†', 435 )
            , ( '‡', 435 )
            , ( '•', 295 )
            , ( '…', 959 )
            , ( '\u{202F}', 121 )
            , ( '‰', 1144 )
            , ( '′', 242 )
            , ( '″', 408 )
            , ( '‹', 252 )
            , ( '›', 252 )
            , ( '⁄', 84 )
            , ( '⁰', 357 )
            , ( 'ⁱ', 163 )
            , ( '⁴', 354 )
            , ( '⁵', 351 )
            , ( '⁶', 353 )
            , ( '⁷', 353 )
            , ( '⁸', 353 )
            , ( '⁹', 353 )
            , ( '⁽', 227 )
            , ( '⁾', 226 )
            , ( 'ⁿ', 355 )
            , ( '₀', 357 )
            , ( '₁', 356 )
            , ( '₂', 353 )
            , ( '₃', 351 )
            , ( '₄', 354 )
            , ( '₅', 351 )
            , ( '₆', 353 )
            , ( '₇', 353 )
            , ( '₈', 353 )
            , ( '₉', 353 )
            , ( '₍', 227 )
            , ( '₎', 226 )
            , ( 'ₔ', 320 )
            , ( '₡', 480 )
            , ( '₤', 480 )
            , ( '₦', 480 )
            , ( '₧', 480 )
            , ( '₫', 480 )
            , ( '€', 480 )
            , ( '₱', 480 )
            , ( '₲', 480 )
            , ( '₵', 480 )
            , ( '₹', 480 )
            , ( '₺', 480 )
            , ( 'ℓ', 394 )
            , ( '℗', 665 )
            , ( '℠', 601 )
            , ( '™', 611 )
            , ( 'Ω', 658 )
            , ( '℮', 800 )
            , ( '⅓', 778 )
            , ( '⅔', 795 )
            , ( '⅛', 773 )
            , ( '⅜', 787 )
            , ( '⅝', 787 )
            , ( '⅞', 770 )
            , ( '←', 593 )
            , ( '↑', 593 )
            , ( '→', 593 )
            , ( '↓', 593 )
            , ( '∂', 519 )
            , ( '∆', 565 )
            , ( '∏', 646 )
            , ( '∑', 485 )
            , ( '−', 480 )
            , ( '∕', 84 )
            , ( '∙', 244 )
            , ( '√', 537 )
            , ( '∞', 752 )
            , ( '∫', 321 )
            , ( '≈', 480 )
            , ( '≠', 480 )
            , ( '≤', 480 )
            , ( '≥', 480 )
            , ( '⌜', 292 )
            , ( '⌝', 293 )
            , ( '⌞', 292 )
            , ( '⌟', 293 )
            , ( '■', 895 )
            , ( '▲', 895 )
            , ( '△', 895 )
            , ( '▶', 895 )
            , ( '▷', 895 )
            , ( '▼', 895 )
            , ( '▽', 895 )
            , ( '◀', 895 )
            , ( '◁', 895 )
            , ( '◆', 895 )
            , ( '◉', 895 )
            , ( '◊', 499 )
            , ( '☐', 765 )
            , ( '☑', 766 )
            , ( '♪', 480 )
            , ( '✓', 542 )
            , ( '❒', 895 )
            , ( '⸢', 292 )
            , ( '⸣', 293 )
            , ( '⸤', 292 )
            , ( '⸥', 293 )
            , ( 'ﬀ', 552 )
            , ( 'ﬁ', 519 )
            , ( 'ﬂ', 530 )
            ]
    }
