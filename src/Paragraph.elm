module Paragraph exposing (Options, format, lines)

{-| This package picks (close to) optimal points to break a paragraph into lines.

Based on the paper [Bridging the Algorithm Gap: A Linear-time Functional Program for Paragraph Formatting][paper]

[paper]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.33.7923&rep=rep1&type=pdf

@docs Options, format, lines

-}

import SymmetricList exposing (SymmetricList)


{-| Formatting options

  - **maximumWidth**: the maximum width of a line
  - **optimalWidth**: the optimal width of a line. Often this is slightly smaller than the maximum width
  - **stringWidth**: function that calculates the width of a string. `String.length` usually gives good results, but other options (based on a font, or something creative) can be used as well.

> **Note:** if a word is longer than the maximum width of a line it is left as-is. The line is broken after the word.

-}
type alias Options =
    { maximumWidth : Int, optimalWidth : Int, stringWidth : String -> Int }


{-| Format a string using the given options

The output is one string where the lines are broken with newline `\n` characters.

-}
format : Options -> String -> String
format options =
    unparse << List.map (paragraph options << List.concat) << parse


{-| Format a string using the given options, returning the individual lines
-}
lines : Options -> String -> List String
lines options =
    List.map (String.join " ") << unparas << List.map (paragraph options << List.concat) << parse


type alias Text =
    List Word


type alias Word =
    String


type alias Line =
    List Word


type alias State =
    { candidates : SymmetricList Paragraph, width : Int, length : Int }


type alias Paragraph =
    { width : Int, waste : Int, length : Int }


undo : a -> List (List a) -> List a
undo a =
    let
        insert xs ys =
            xs ++ [ a ] ++ ys
    in
    Maybe.withDefault [] << fold1 insert identity


do : a -> List a -> List (List a)
do a list =
    case list of
        [] ->
            [ [] ]

        first :: rest ->
            let
                break p q xs =
                    if p == q then
                        [] :: xs

                    else
                        case xs of
                            [] ->
                                (q :: []) :: []

                            y :: ys ->
                                (q :: y) :: ys

                start p q =
                    break p q [ [] ]
            in
            fold1 (break a) (start a) list
                |> Maybe.withDefault [ [] ]


paras =
    List.filter (not << List.isEmpty) << do []


unparas =
    undo []


parse : String -> List (List Line)
parse =
    paras << List.map String.words << String.lines


unparse : List (List Line) -> String
unparse =
    String.join "\n" << List.map (String.join " ") << unparas



-- Bowels


ceildiv : Int -> Int -> Int
ceildiv n m =
    (n + m - 1) // m


isSingleton : List a -> Bool
isSingleton list =
    case list of
        [ _ ] ->
            True

        _ ->
            False


single : Paragraph -> Bool
single p =
    p.length == 0


stepr : Options -> Int -> State -> State
stepr options w state =
    let
        waste : Paragraph -> Int
        waste p =
            if single p then
                0

            else
                p.waste + (options.optimalWidth - width_hd p) ^ 2

        width_hd : Paragraph -> Int
        width_hd p =
            tot_width - p.width - 1

        tot_width : Int
        tot_width =
            w + 1 + state.width

        tot_len : Int
        tot_len =
            1 + state.length

        -- adds a new line to the front of a paragraph
        new : Paragraph -> Paragraph
        new p =
            if single p then
                { width = state.width, waste = 0, length = state.length }

            else
                { width = state.width, waste = p.width + (options.optimalWidth - old_width_hd p) ^ 2, length = state.length }

        old_width_hd : Paragraph -> Int
        old_width_hd p =
            state.width - p.width - 1

        discardBadCandidates : SymmetricList Paragraph -> SymmetricList Paragraph
        discardBadCandidates ps_pq =
            case SymmetricList.unsnoc ps_pq of
                Nothing ->
                    ps_pq

                Just ( ps_p, q ) ->
                    case SymmetricList.last ps_p of
                        Nothing ->
                            -- ps_pq is a singleton
                            if width_hd q > options.maximumWidth then
                                SymmetricList.empty

                            else
                                ps_pq

                        Just p ->
                            if waste p <= waste q || width_hd q > options.maximumWidth then
                                -- current candidate is bad, discard it and continue
                                discardBadCandidates ps_p

                            else
                                ps_pq

        add : Paragraph -> SymmetricList Paragraph -> SymmetricList Paragraph
        add p qr_rs =
            let
                default =
                    SymmetricList.cons p qr_rs
            in
            case SymmetricList.uncons qr_rs of
                Nothing ->
                    -- qr_rs is empty
                    default

                Just ( q, r_rs ) ->
                    case SymmetricList.head r_rs of
                        Just r ->
                            if k p q <= k q r then
                                add p r_rs

                            else
                                default

                        Nothing ->
                            -- qr_rs is a singleton
                            default

        k : Paragraph -> Paragraph -> Int
        k p q =
            let
                wp0 =
                    width_hd p

                wq0 =
                    width_hd q

                rq0 =
                    options.maximumWidth - wq0 + 1
            in
            if single q && p.width == 0 then
                min (options.optimalWidth - wp0) rq0

            else if single q then
                rq0

            else
                min (ceildiv (waste p - waste q) (2 * (wq0 - wp0))) rq0
    in
    case SymmetricList.last state.candidates of
        Nothing ->
            -- TODO error
            Debug.todo "error"

        Just last ->
            let
                input =
                    add (new last) state.candidates

                newCandidates =
                    discardBadCandidates input
            in
            { candidates = newCandidates, width = tot_width, length = tot_len }


startr : Int -> State
startr width =
    -- if width <= options.maximumWidth then
    -- NOTE this will proceed even if a word is longer than the maximum allowed width
    { candidates = SymmetricList.singleton { width = 0, waste = 0, length = 0 }
    , width = width
    , length = 1
    }


{-| paragraph : List String -> List (List String)
-}
paragraph : Options -> Text -> List Line
paragraph options words =
    let
        zs : List State
        zs =
            List.map options.stringWidth words
                |> scan1 (stepr options) startr
    in
    case zs of
        [] ->
            []

        first :: rest ->
            let
                targetLength =
                    first.length

                wordLengths =
                    List.filterMap (Maybe.map .length << SymmetricList.last << .candidates) zs
            in
            tile words ( wordLengths, targetLength )


tile : Text -> ( List Int, Int ) -> List Line
tile words ( wordLengths, targetLength ) =
    case wordLengths of
        [] ->
            []

        m :: ms ->
            let
                remainingSpace =
                    targetLength - m

                ( usedWords, remainingWords ) =
                    splitAt remainingSpace words
            in
            usedWords :: tile remainingWords ( List.drop remainingSpace (m :: ms), m )


scan1 f g list =
    let
        g_ a =
            [ g a ]

        f_ a s =
            case s of
                [] ->
                    s

                first :: rest ->
                    f a first :: s
    in
    fold1 f_ g_ list
        |> Maybe.withDefault []


fold1 f g list =
    case list of
        [] ->
            Nothing

        [ a ] ->
            Just (g a)

        a :: x ->
            Maybe.map (\xs -> f a xs) (fold1 f g x)


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


paragraph1 =
    "Þá er Ísland fannst og byggðist af Noregi, var Adríánus páfi í Róma og Jóhannes eftir hann, sá er hinn fimmti var með því nafni í postuligu sæti, en Hlöðver Hlöðversson keisari fyrir norðan fjall, en Leó og Alexander son hans yfir Miklagarði; þá var Haraldur hárfagri konungur yfir Noregi, en Eiríkur Eymundarson í Svíþjóð og Björn son hans, en Gormur hinn gamli að Danmörk, en Elfráður hinn ríki í Englandi og Játvarður son hans, en Kjarvalur að Dyflinni, Sigurður jarl hinn ríki í Orkneyjum."


kafli1 =
    String.split " "
        """Þá er Ísland fannst og byggðist af Noregi, var Adríánus páfi í Róma og Jóhannes eftir hann, sá er hinn fimmti var með því nafni í postuligu sæti, en Hlöðver Hlöðversson keisari fyrir norðan fjall, en Leó og Alexander son hans yfir Miklagarði; þá var Haraldur hárfagri konungur yfir Noregi, en Eiríkur Eymundarson í Svíþjóð og Björn son hans, en Gormur hinn gamli að Danmörk, en Elfráður hinn ríki í Englandi og Játvarður son hans, en Kjarvalur að Dyflinni, Sigurður jarl hinn ríki í Orkneyjum.

Svo segja vitrir menn, að úr Noregi frá Staði sé sjö dægra sigling í vestur til Horns á Íslandi austanverðu, en frá Snæfellsnesi, þar er skemmst er, er fjögurra dægra haf í vestur til Grænlands. En svo er sagt, ef siglt er úr Björgyn rétt í vestur til Hvarfsins á Grænlandi, að þá mun siglt vera tylft fyrir sunnan Ísland. Frá Reykjanesi á sunnanverðu Íslandi er fimm dægra haf til Jölduhlaups á Írlandi (í suður; en frá Langanesi á norðanverðu Íslandi er) fjögurra dægra haf norður til Svalbarða í hafsbotn.

Svo er sagt, að menn skyldu fara úr Noregi til Færeyja; nefna sumir til Naddodd víking; en þá rak vestur í haf og fundu þar land mikið. Þeir gengu upp í Austfjörðum á fjall eitt hátt og sáust um víða, ef þeir sæju reyki eða nokkur líkindi til þess, að landið væri byggt, og sáu þeir það ekki.

Þeir fóru aftur um haustið til Færeyja; og er þeir sigldu af landinu, féll snær mikill á fjöll, og fyrir það kölluðu þeir landið Snæland. Þeir lofuðu mjög landið.

Þar heitir nú Reyðarfjall í Austfjörðum, er þeir höfðu að komið. Svo sagði Sæmundur prestur hinn fróði.

Maður hét Garðar Svavarsson, sænskur að ætt; hann fór að leita Snælands að tilvísan móður sinnar framsýnnar. Hann kom að landi fyrir austan Horn hið eystra; þar var þá höfn. Garðar sigldi umhverfis landið og vissi, að það var eyland. Hann var um veturinn norður í Húsavík á Skjálfanda og gerði þar hús.

Um vorið, er hann var búinn til hafs, sleit frá honum mann á báti, er hét Náttfari, og þræl og ambátt. Hann byggði þar síðan, er heitir Náttfaravík.

Garðar fór þá til Noregs og lofaði mjög landið. Hann var faðir Una, föður Hróars Tungugoða. Eftir það var landið kallað Garðarshólmur, og var þá skógur milli fjalls og fjöru.
"""
