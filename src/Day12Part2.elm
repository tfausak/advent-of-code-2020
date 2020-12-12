module Day12Part2 exposing (solve)


solve : String -> String
solve string =
    string
        |> String.words
        |> List.filterMap stringToInstruction
        |> List.foldl handleInstruction startingShip
        |> .position
        |> manhattanDistance startingShip.position
        |> String.fromInt


stringToInstruction : String -> Maybe Instruction
stringToInstruction string =
    string
        |> String.uncons
        |> Maybe.andThen
            (\( char, rest ) ->
                Maybe.map2 Instruction
                    (charToAction char)
                    (String.toInt rest)
            )


charToAction : Char -> Maybe Action
charToAction char =
    case char of
        'N' ->
            Just (Move North)

        'S' ->
            Just (Move South)

        'E' ->
            Just (Move East)

        'W' ->
            Just (Move West)

        'L' ->
            Just (Rotate Counterclockwise)

        'R' ->
            Just (Rotate Clockwise)

        'F' ->
            Just Forward

        _ ->
            Nothing


handleInstruction : Instruction -> Ship -> Ship
handleInstruction instruction ship =
    case instruction.action of
        Move direction ->
            { ship | waypoint = move direction instruction.value ship.waypoint }

        Rotate rotation ->
            { ship | waypoint = rotate rotation (instruction.value // 90) ship.waypoint }

        Forward ->
            { ship | position = forward ship.waypoint instruction.value ship.position }


move : Direction -> Int -> Position -> Position
move direction value position =
    case direction of
        North ->
            { position | y = position.y + value }

        South ->
            { position | y = position.y - value }

        East ->
            { position | x = position.x + value }

        West ->
            { position | x = position.x - value }


rotate : Rotation -> Int -> Position -> Position
rotate rotation value position =
    case rotation of
        Clockwise ->
            applyN rotateRight (modBy 4 value) position

        Counterclockwise ->
            rotate Clockwise (negate value) position


applyN : (a -> a) -> Int -> a -> a
applyN f n x =
    if n < 1 then
        x

    else
        applyN f (n - 1) (f x)


rotateRight : Position -> Position
rotateRight position =
    { x = position.y
    , y = negate position.x
    }


forward : Position -> Int -> Position -> Position
forward waypoint value position =
    { x = position.x + (value * waypoint.x)
    , y = position.y + (value * waypoint.y)
    }


startingShip : Ship
startingShip =
    { position = { x = 0, y = 0 }
    , waypoint = { x = 10, y = 1 }
    }


manhattanDistance : Position -> Position -> Int
manhattanDistance p q =
    abs (p.x - q.x) + abs (p.y - q.y)


type alias Instruction =
    { action : Action
    , value : Int
    }


type Action
    = Move Direction
    | Rotate Rotation
    | Forward


type Direction
    = North
    | South
    | East
    | West


type Rotation
    = Clockwise
    | Counterclockwise


type alias Ship =
    { position : Position
    , waypoint : Position
    }


type alias Position =
    { x : Int
    , y : Int
    }
