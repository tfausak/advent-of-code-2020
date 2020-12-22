module Day21 exposing (part1, part2)
import Elf
import Set exposing (Set)

type alias Food =
    { ingredients : Set String
    , allergens : Set String }

part1 : String -> String
part1 string =
    let
        foods = string
            |> String.replace ")" ""
            |> String.trimRight
            |> String.lines
            |> List.map (\ line -> line
                |> String.split " (contains "
                |> Elf.listToTuple
                |> Maybe.map (\ ( ingredients, allergens ) ->
                    { ingredients = ingredients
                        |> String.words
                        |> Set.fromList
                    , allergens = allergens
                        |> String.split ", "
                        |> Set.fromList
                    }))
        _ = Debug.log "foods"
    in "TODO day 21 part 1"


part2 : String -> String
part2 _ =
    "TODO day 21 part 2"
