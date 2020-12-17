module Day16 exposing (part1, part2)

import List.Extra
import Maybe.Extra
import Set exposing (Set)


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
part2 string = string
    |> parseNotes
    |> Maybe.map (\ notes ->
        let
            validTickets = notes.nearby
                |> List.filter (isValidTicket notes.rules)
            ruleCandidates = notes.rules
                |> List.map (\ rule -> validTickets
                    |> List.map (\ ticket -> ticket
                        |> List.indexedMap Tuple.pair
                        |> List.filterMap (\ ( index, value ) -> if inRule rule value then Just index else Nothing)
                        |> Set.fromList)
                    |> intersections
                    |> Set.toList
                    |> Tuple.pair rule.field)
                |> List.sortBy (\ (_, indexes) -> List.length indexes)
            solveWith solutions candidates = case candidates of
                ( field, [ index ] ) :: rest -> solveWith
                    (( field, index ) :: solutions)
                    (List.map (\ (k, is) -> (k, List.filter (\ i -> i /= index) is)) rest)
                _ -> solutions
            targetIndexes = ruleCandidates
                |> solveWith []
                |> List.filterMap (\ ( field, index ) -> if String.startsWith "departure" field then Just index else Nothing)
                |> Set.fromList
        in notes.mine
        |> List.indexedMap Tuple.pair
        |> List.filter (\ ( index, _ ) -> Set.member index targetIndexes)
        |> List.map Tuple.second
        |> List.product
        |> String.fromInt)
    |> Maybe.withDefault ":("

intersections : List (Set comparable) -> Set comparable
intersections xs = case xs of
    [] -> Set.empty
    x :: ys -> List.foldl Set.intersect x ys

isValidTicket : Rules -> Ticket -> Bool
isValidTicket rules ticket = List.all (\ value -> inRules rules value) ticket

inRules : Rules -> Int -> Bool
inRules rules value = List.any (\ rule -> inRule rule value) rules

inRule : Rule -> Int -> Bool
inRule rule value = List.any (\ range -> inRange range value) rule.ranges

inRange : Range -> Int -> Bool
inRange range value = range.low <= value && value <= range.high


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
