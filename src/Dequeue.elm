module Dequeue exposing (Dequeue, fromList, popFront, pushBack, toList)


type alias Dequeue a =
    { front : List a, back : List a }


fromList : List a -> Dequeue a
fromList xs =
    { front = xs, back = [] }


pushBack : a -> Dequeue a -> Dequeue a
pushBack x d =
    { d | back = x :: d.back }


popFront : Dequeue a -> Maybe ( a, Dequeue a )
popFront d =
    case d.front of
        x :: rest ->
            Just ( x, { d | front = rest } )

        [] ->
            case List.reverse d.back of
                x :: rest ->
                    Just ( x, { front = rest, back = [] } )

                [] ->
                    Nothing


toList : Dequeue a -> List a
toList d =
    d.front ++ List.reverse d.back
