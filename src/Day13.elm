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
part2 _ =
    "TODO"


listToTuple : List a -> Maybe ( a, a )
listToTuple list =
    case list of
        first :: second :: _ ->
            Just ( first, second )

        _ ->
            Nothing
