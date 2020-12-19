module Day18 exposing (part1, part2)


part1 : String -> String
part1 =
    String.trim
        >> String.lines
        >> List.filterMap (chunk >> List.filterMap toTok >> evaluate [])
        >> List.sum
        >> String.fromInt


part2 : String -> String
part2 _ =
    "TODO day 18 part 2"


chunk : String -> List String
chunk =
    String.replace "(" "( " >> String.replace ")" " )" >> String.words


toTok : String -> Maybe Tok
toTok string =
    case string of
        "+" ->
            Just Add

        "*" ->
            Just Mul

        "(" ->
            Just Opn

        ")" ->
            Just Cls

        _ ->
            Maybe.map Num (String.toInt string)


type Tok
    = Add
    | Mul
    | Opn
    | Cls
    | Num Int


evaluate : List Tok -> List Tok -> Maybe Int
evaluate stack items =
    case ( stack, items ) of
        ( (Num y) :: Add :: (Num x) :: rest, _ ) ->
            evaluate (Num (y + x) :: rest) items

        ( (Num y) :: Mul :: (Num x) :: rest, _ ) ->
            evaluate (Num (y * x) :: rest) items

        ( Cls :: (Num x) :: Opn :: rest, _ ) ->
            evaluate (Num x :: rest) items

        ( [ Num x ], [] ) ->
            Just x

        ( _, item :: rest ) ->
            evaluate (item :: stack) rest

        ( _, [] ) ->
            Nothing
