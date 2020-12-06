module Day06 exposing (part1, part2)

import Set exposing (Set)


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
part2 _ =
    "TODO day 6 part 2"
