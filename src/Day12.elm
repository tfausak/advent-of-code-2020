module Day12 exposing (part1, part2)


part1 : String -> String
part1 string =
    string
        |> stringToInstructions
        |> List.foldl handleInstruction startingShip
        |> .position
        |> manhattanDistance startingShip.position
        |> String.fromInt


manhattanDistance : Position -> Position -> Int
manhattanDistance p q =
    abs (p.x - q.x) + abs (p.y - q.y)


handleInstruction : Instruction -> Ship -> Ship
handleInstruction instruction ship =
    case instruction.action of
        MoveNorth ->
            { ship | position = move North instruction.value ship.position }

        MoveSouth ->
            { ship | position = move South instruction.value ship.position }

        MoveEast ->
            { ship | position = move East instruction.value ship.position }

        MoveWest ->
            { ship | position = move West instruction.value ship.position }

        TurnLeft ->
            { ship | direction = applyN (instruction.value // 90) turnLeft ship.direction }

        TurnRight ->
            { ship | direction = applyN (instruction.value // 90) turnRight ship.direction }

        MoveForward ->
            case ship.direction of
                North ->
                    handleInstruction { instruction | action = MoveNorth } ship

                South ->
                    handleInstruction { instruction | action = MoveSouth } ship

                East ->
                    handleInstruction { instruction | action = MoveEast } ship

                West ->
                    handleInstruction { instruction | action = MoveWest } ship


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


turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        North ->
            West

        West ->
            South

        South ->
            East

        East ->
            North


applyN : Int -> (a -> a) -> a -> a
applyN n f x =
    if n < 1 then
        x

    else
        applyN (n - 1) f (f x)


turnRight : Direction -> Direction
turnRight direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


type alias Ship =
    { position : Position
    , direction : Direction
    }


type alias Position =
    { x : Int
    , y : Int
    }


startingShip : Ship
startingShip =
    { position = { x = 0, y = 0 }
    , direction = East
    }


type Direction
    = North
    | South
    | East
    | West


stringToInstructions : String -> List Instruction
stringToInstructions string =
    string
        |> String.words
        |> List.filterMap stringToInstruction


type alias Instruction =
    { action : Action
    , value : Int
    }


stringToInstruction : String -> Maybe Instruction
stringToInstruction string =
    String.uncons string
        |> Maybe.andThen
            (\( char, rest ) ->
                Maybe.map2 Instruction
                    (charToAction char)
                    (String.toInt rest)
            )


type Action
    = MoveNorth
    | MoveSouth
    | MoveEast
    | MoveWest
    | TurnLeft
    | TurnRight
    | MoveForward


charToAction : Char -> Maybe Action
charToAction char =
    case char of
        'N' ->
            Just MoveNorth

        'S' ->
            Just MoveSouth

        'E' ->
            Just MoveEast

        'W' ->
            Just MoveWest

        'L' ->
            Just TurnLeft

        'R' ->
            Just TurnRight

        'F' ->
            Just MoveForward

        _ ->
            Nothing


part2 : String -> String
part2 _ =
    "TODO day 12 part 2"
