module Day22 exposing (part1, part2)

import Dequeue
import Elf


part1 : String -> String
part1 string =
    let
        parseDeck deck =
            deck
                |> String.lines
                |> List.filterMap String.toInt
                |> Dequeue.fromList

        play round one two =
            case ( Dequeue.popFront one, Dequeue.popFront two ) of
                ( Just ( h1, t1 ), Just ( h2, t2 ) ) ->
                    if h1 > h2 then
                        play (round + 1) (t1 |> Dequeue.pushBack h1 |> Dequeue.pushBack h2) t2

                    else
                        play (round + 1) t1 (t2 |> Dequeue.pushBack h2 |> Dequeue.pushBack h1)

                _ ->
                    ( round, ( one, two ) )

        solve tuple =
            tuple
                |> Elf.mapBoth parseDeck parseDeck
                |> Elf.uncurry (play 1)
                |> Tuple.second
                |> Elf.mapBoth Dequeue.toList Dequeue.toList
                |> Elf.uncurry (++)
                |> List.reverse
                |> List.indexedMap (\i x -> x * (i + 1))
                |> List.sum
                |> String.fromInt
    in
    string
        |> String.split "\n\n"
        |> Elf.listToTuple
        |> Maybe.map solve
        |> Maybe.withDefault ":("


part2 : String -> String
part2 _ =
    "TODO day 22 part 2"
