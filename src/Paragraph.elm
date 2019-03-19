module Paragraph exposing (Options, format, lines)

{-| Pick (close to) optimal points to break a paragraph into lines.

@docs Options, format, lines

-}

import StringHelpers exposing (Line, Word)
import SymmetricList exposing (SymmetricList)


{-|

  - **maximumWidth**: the maximum width of a line
  - **optimalWidth**: the optimal width of a line. Often this is slightly smaller than the maximum width
  - **stringWidth**: function that calculates the width of a string. `String.length` usually gives good results, but other options (based on a font, or something creative) can be used as well.

> **Note:** if a word is longer than the maximum width of a line it is left as-is. The line is broken after the word.

-}
type alias Options =
    { maximumWidth : Int, optimalWidth : Int, stringWidth : String -> Int }


{-| Format a string using the given options

    options : Options
    options =
        { maximumWidth = 15
        , optimalWidth = 12
        , stringWidth = String.length
        }

    Paragraph.format options
        "A delightful language for reliable webapps"
        --> "A delightful\nlanguage\nfor reliable\nwebapps"

-}
format : Options -> String -> String
format options =
    StringHelpers.unparse << List.map (paragraph options << List.concat) << StringHelpers.parse


{-| Get the individual lines of the formatted paragraph

    options : Options
    options =
        { maximumWidth = 15
        , optimalWidth = 12
        , stringWidth = String.length
        }

    Paragraph.lines options
        "A delightful language for reliable webapps"
    --> ["A delightful","language","for reliable","webapps"]

-}
lines : Options -> String -> List String
lines options text =
    text
        |> StringHelpers.parse
        |> List.map (paragraph options << List.concat)
        |> StringHelpers.unparas
        |> List.map (String.trimLeft << String.join "")


type alias Text =
    List Word


type alias State =
    { candidates : SymmetricList Paragraph, width : Int, numberOfWords : Int }


type alias Paragraph =
    { width : Int, waste : Int, numberOfWords : Int }



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
    p.numberOfWords == 0


stepr : Options -> Int -> State -> State
stepr options wordWidth state =
    let
        waste : Paragraph -> Int
        waste p =
            if single p then
                0

            else
                p.waste + (options.optimalWidth - width_hd p) ^ 2

        width_hd : Paragraph -> Int
        width_hd p =
            tot_width - p.width

        tot_width : Int
        tot_width =
            wordWidth + state.width

        tot_len : Int
        tot_len =
            1 + state.numberOfWords

        -- adds a new line to the front of a paragraph
        new : Paragraph -> Paragraph
        new p =
            if single p then
                { width = state.width, waste = 0, numberOfWords = state.numberOfWords }

            else
                { width = state.width, waste = p.width + (options.optimalWidth - old_width_hd p) ^ 2, numberOfWords = state.numberOfWords }

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
                            -- the actual condition that decised whether to keep a candidate
                            if waste p <= waste q || width_hd q > options.maximumWidth then
                                -- current candidate is worse than the next one, discard it and continue
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
            -- should not happen
            state

        Just last ->
            let
                input =
                    add (new last) state.candidates

                newCandidates =
                    discardBadCandidates input
            in
            { candidates = newCandidates, width = tot_width, numberOfWords = tot_len }


startr : Int -> State
startr width =
    -- if width <= options.maximumWidth then
    -- NOTE this will proceed even if a word is longer than the maximum allowed width
    -- also the last word on a line has no waste
    { candidates = SymmetricList.singleton { width = 0, waste = 0, numberOfWords = 0 }
    , width = width
    , numberOfWords = 1
    }


{-| paragraph : List String -> List (List String)
-}
paragraph : Options -> Text -> List Line
paragraph options words =
    -- TODO more opportunities for fusion exist
    let
        states : List State
        states =
            scan1 (stepr options << options.stringWidth) (startr << options.stringWidth) words
    in
    case states of
        [] ->
            []

        first :: rest ->
            let
                targetLength =
                    first.numberOfWords
                        |> Debug.log "target length"

                wordLengths =
                    List.filterMap (Maybe.map .numberOfWords << SymmetricList.last << .candidates) states
                        |> Debug.log "word lengths"
            in
            tileWords words wordLengths targetLength


tileWords : Text -> List Int -> Int -> List Line
tileWords =
    tileWordsHelp []


tileWordsHelp : List Line -> Text -> List Int -> Int -> List Line
tileWordsHelp accum words wordLengths targetLength =
    case wordLengths of
        [] ->
            List.reverse accum

        m :: ms ->
            let
                wordsRemaining =
                    targetLength - m

                ( usedWords, remainingWords ) =
                    splitAt wordsRemaining words
            in
            tileWordsHelp (usedWords :: accum) remainingWords (List.drop (wordsRemaining - 1) ms) m


scan1 : (a -> b -> b) -> (a -> b) -> List a -> List b
scan1 f g list =
    let
        g_ a =
            [ g a ]

        f_ a s =
            case s of
                [] ->
                    []

                first :: rest ->
                    f a first :: s
    in
    case list of
        [] ->
            []

        x :: xs ->
            fold1 f_ g_ ( x, xs )


fold1 : (a -> b -> b) -> (a -> b) -> ( a, List a ) -> b
fold1 f g ( first, rest ) =
    case rest of
        [] ->
            g first

        x :: xs ->
            f first (fold1 f g ( x, xs ))


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    splitAtHelper n [] xs


splitAtHelper : Int -> List a -> List a -> ( List a, List a )
splitAtHelper n taken remaining =
    if n <= 0 then
        ( List.reverse taken, remaining )

    else
        case remaining of
            [] ->
                ( List.reverse taken, [] )

            x :: xs ->
                splitAtHelper (n - 1) (x :: taken) xs
