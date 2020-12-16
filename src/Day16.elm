module Day16 exposing (part1, part2)

import Maybe.Extra


part1 : String -> String
part1 string =
    string
        |> parseNotes
        |> Maybe.map
            (\notes ->
                notes.nearby
                    |> List.concat
                    |> List.filter
                        (\value ->
                            notes.rules
                                |> List.concatMap .ranges
                                |> List.any (isValid value)
                                |> not
                        )
                    |> List.sum
                    |> String.fromInt
            )
        |> Maybe.withDefault ":("


part2 : String -> String
part2 _ =
    "TODO day 16 part 2"


isValid : Int -> Range -> Bool
isValid value range =
    range.low <= value && value <= range.high


type alias Notes =
    { rules : Rules
    , mine : Ticket
    , nearby : Tickets
    }


type alias Rules =
    List Rule


type alias Rule =
    { field : String
    , ranges : Ranges
    }


type alias Ranges =
    List Range


type alias Range =
    { low : Int
    , high : Int
    }


type alias Tickets =
    List Ticket


type alias Ticket =
    List Int


parseNotes : String -> Maybe Notes
parseNotes string =
    case String.split "\n\n" string of
        [ rules, mine, nearby ] ->
            Maybe.map3 Notes
                (parseRules rules)
                (parseMyTicket mine)
                (parseNearbyTickets nearby)

        _ ->
            Nothing


parseRules : String -> Maybe Rules
parseRules string =
    string
        |> String.lines
        |> Maybe.Extra.traverse parseRule


parseRule : String -> Maybe Rule
parseRule string =
    case String.split ": " string of
        [ field, ranges ] ->
            Maybe.map (Rule field) (parseRanges ranges)

        _ ->
            Nothing


parseRanges : String -> Maybe Ranges
parseRanges string =
    string
        |> String.split " or "
        |> Maybe.Extra.traverse parseRange


parseRange : String -> Maybe Range
parseRange string =
    case String.split "-" string of
        [ low, high ] ->
            Maybe.map2 Range
                (String.toInt low)
                (String.toInt high)

        _ ->
            Nothing


parseMyTicket : String -> Maybe Ticket
parseMyTicket string =
    case String.words string of
        [ "your", "ticket:", ticket ] ->
            parseTicket ticket

        _ ->
            Nothing


parseTicket : String -> Maybe Ticket
parseTicket string =
    string
        |> String.split ","
        |> Maybe.Extra.traverse String.toInt


parseNearbyTickets : String -> Maybe Tickets
parseNearbyTickets string =
    case String.words string of
        "nearby" :: "tickets:" :: tickets ->
            Maybe.Extra.traverse parseTicket tickets

        _ ->
            Nothing
