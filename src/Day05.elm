module Day05 exposing (part1, part2)


part1 : String -> String
part1 string =
    string
        |> String.trimRight
        |> String.lines
        |> List.filterMap findSeat
        |> List.map seatId
        |> List.maximum
        |> Maybe.withDefault -1
        |> String.fromInt


findSeat : String -> Maybe ( Int, Int )
findSeat string =
    findSeatWith ( 0, 127 ) ( 0, 7 ) (String.toList string)


findSeatWith : ( Int, Int ) -> ( Int, Int ) -> List Char -> Maybe ( Int, Int )
findSeatWith ( rl, rh ) ( cl, ch ) xs =
    case xs of
        [] ->
            if rl == rh && cl == ch then
                Just ( rl, cl )

            else
                Nothing

        x :: ys ->
            case x of
                'F' ->
                    findSeatWith ( rl, rh - ((rh - rl + 1) // 2) ) ( cl, ch ) ys

                'B' ->
                    findSeatWith ( rl + ((rh - rl + 1) // 2), rh ) ( cl, ch ) ys

                'L' ->
                    findSeatWith ( rl, rh ) ( cl, ch - ((ch - cl + 1) // 2) ) ys

                'R' ->
                    findSeatWith ( rl, rh ) ( cl + ((ch - cl + 1) // 2), ch ) ys

                _ ->
                    Nothing


seatId : ( Int, Int ) -> Int
seatId ( row, column ) =
    row * 8 + column


part2 : String -> String
part2 string =
    string
        |> String.trimRight
        |> String.lines
        |> List.filterMap findSeat
        |> List.map seatId
        |> List.sort
        |> findMySeat
        |> Maybe.withDefault -1
        |> String.fromInt


findMySeat : List Int -> Maybe Int
findMySeat xs =
    case xs of
        x :: y :: zs ->
            let
                z =
                    x + 1
            in
            if z == y then
                findMySeat (y :: zs)

            else
                Just z

        _ ->
            Nothing
