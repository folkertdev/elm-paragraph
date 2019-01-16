module SymmetricList exposing (SymmetricList, cons, empty, fromList, head, init, isEmpty, isSingleton, last, length, singleton, snoc, tail, toList, uncons, unsnoc)

{-| A Symmetric List is a type of container with interesting performance properties.

In particular, operations on both ends of the list are (on average) constant-time. That means that looking at or removing the last element of a `SymmetricList` is no slower than removing the first element.
In contrast, finding the last element of a regular list means walking over the whole list.


## Implemenation

Briefly, a list `x` is represented as a pair of lists `(y,z)` such that `toList (y,z) = x`, where the `toList` function is defined by `toList (y,z) = y ++ reverse z`.
Moreover, the following invariant is maintained: if either of the two lists is empty, the other is empty or a singleton.

In relation to normal lists, these conditions are true:

    SymmetricList.head = List.head << SymmetricList.toList
    SymmetricList.toList << SymmetricList.tail = List.tail << SymmetricList.toList

-}


type SymmetricList a
    = SymmetricList (List a) (List a)


toList : SymmetricList a -> List a
toList (SymmetricList x y) =
    x ++ List.reverse y


fromList : List a -> SymmetricList a
fromList =
    List.foldr cons empty


length : SymmetricList a -> Int
length (SymmetricList x y) =
    List.length x + List.length y


isSingletonList list =
    case list of
        [ _ ] ->
            True

        _ ->
            False


isSingleton (SymmetricList x y) =
    (List.isEmpty x && isSingletonList y) || (isSingletonList x && List.isEmpty y)


isEmpty symlist =
    case symlist of
        SymmetricList [] [] ->
            True

        _ ->
            False


empty : SymmetricList a
empty =
    SymmetricList [] []


singleton : a -> SymmetricList a
singleton x =
    cons x empty


head : SymmetricList a -> Maybe a
head (SymmetricList x y) =
    if not (List.isEmpty x) then
        List.head x

    else
        List.head y


last : SymmetricList a -> Maybe a
last (SymmetricList y x) =
    if not (List.isEmpty x) then
        List.head x

    else
        List.head y


uncons : SymmetricList a -> Maybe ( a, SymmetricList a )
uncons symlist =
    case head symlist of
        Just v ->
            Just ( v, tail symlist )

        Nothing ->
            Nothing


unsnoc : SymmetricList a -> Maybe ( SymmetricList a, a )
unsnoc symlist =
    case last symlist of
        Just v ->
            Just ( init symlist, v )

        Nothing ->
            Nothing


cons : a -> SymmetricList a -> SymmetricList a
cons a (SymmetricList x y) =
    if not (List.isEmpty y) then
        SymmetricList (a :: x) y

    else
        SymmetricList [ a ] x


snoc : a -> SymmetricList a -> SymmetricList a
snoc a (SymmetricList y x) =
    if not (List.isEmpty y) then
        SymmetricList y (a :: x)

    else
        SymmetricList x [ a ]


tail (SymmetricList x y) =
    let
        ( y0, y1 ) =
            splitAt (List.length y // 2) y
    in
    case x of
        [] ->
            empty

        [ _ ] ->
            SymmetricList (List.reverse y1) y0

        _ :: rest ->
            SymmetricList rest y


init (SymmetricList y x) =
    let
        ( y0, y1 ) =
            splitAt (List.length y // 2) y
    in
    case x of
        [] ->
            empty

        [ _ ] ->
            SymmetricList y0 (List.reverse y1)

        _ :: rest ->
            SymmetricList y rest


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )
