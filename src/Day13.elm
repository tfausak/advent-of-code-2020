module Day13 exposing (part1, part2)


part1 : String -> String
part1 string =
    string
        |> String.words
        |> listToTuple
        |> Maybe.andThen
            (\( time, ids ) ->
                Maybe.map
                    (Tuple.pair (List.filterMap String.toInt (String.split "," ids)))
                    (String.toInt time)
            )
        |> Maybe.andThen
            (\( ids, time ) ->
                List.minimum (List.map (\id -> ( id - modBy id time, id )) ids)
            )
        |> Maybe.map (\( d, id ) -> String.fromInt (d * id))
        |> Maybe.withDefault ":("


part2 : String -> String
part2 input =
    input
        |> String.words
        |> List.drop 1
        |> List.head
        |> Maybe.andThen
            (\schedule ->
                schedule
                    |> String.split ","
                    |> List.indexedMap Tuple.pair
                    |> List.filterMap
                        (\( index, id ) ->
                            id
                                |> String.toInt
                                |> Maybe.map (Tuple.pair index)
                        )
                    |> List.sortBy (\( _, x ) -> negate x)
                    |> List.map (\( i, x ) -> ( modBy x (x - i), x ))
                    |> sieve
            )
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ":("


sieve : List ( Int, Int ) -> Maybe Int
sieve xs =
    case xs of
        [] ->
            Nothing

        ( x, n ) :: ys ->
            Just (sieveWith x n ys)


sieveWith : Int -> Int -> List ( Int, Int ) -> Int
sieveWith candidate step list =
    case list of
        [] ->
            candidate

        ( x, n ) :: rest ->
            if modBy n candidate == x then
                sieveWith candidate (lcm step n) rest

            else
                sieveWith (candidate + step) step list


lcm : Int -> Int -> Int
lcm a b =
    let
        d =
            gcd a b
    in
    if d == 1 then
        a * b

    else
        (a * b) // d


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (modBy b a)


listToTuple : List a -> Maybe ( a, a )
listToTuple list =
    case list of
        first :: second :: _ ->
            Just ( first, second )

        _ ->
            Nothing
