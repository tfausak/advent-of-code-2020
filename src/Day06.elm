module Day06 exposing (part1, part2)

import Set exposing (Set, intersect)


part1 : String -> String
part1 =
    String.split "\n\n"
        >> List.map
            (String.words
                >> List.map stringToSet
                >> unionSets
                >> Set.size
            )
        >> List.sum
        >> String.fromInt


stringToSet : String -> Set Char
stringToSet =
    String.toList >> Set.fromList


unionSets : List (Set comparable) -> Set comparable
unionSets =
    List.foldl Set.union Set.empty


part2 : String -> String
part2 =
    String.split "\n\n"
        >> List.map
            (String.words
                >> List.map stringToSet
                >> intersectSets
                >> Set.size
            )
        >> List.sum
        >> String.fromInt


intersectSets : List (Set comparable) -> Set comparable
intersectSets xs =
    case xs of
        [] ->
            Set.empty

        x :: ys ->
            List.foldl Set.intersect x ys
