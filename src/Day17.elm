module Day17 exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as E


type alias Model =
    { cycle : Int
    , input : String
    }


type Msg
    = UpdateInput String


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( { cycle = 0
                  , input = String.join "\n" [ ".#.", "..#", "###" ]
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.textarea [ E.onInput UpdateInput ] [ Html.text model.input ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput input ->
            ( { model | cycle = 0, input = input }, Cmd.none )


type alias Point =
    { x : Int
    , y : Int
    , z : Int
    }


getNeighbors : Point -> List Point
getNeighbors origin =
    let
        deltas =
            [ -1, 0, 1 ]

        fz dx dy dz =
            if dx == 0 && dy == 0 && dz == 0 then
                Nothing

            else
                Just { x = origin.x + dx, y = origin.y + dy, z = origin.z + dz }

        fy dx dy =
            List.filterMap (fz dx dy) deltas

        fx dx =
            List.concatMap (fy dx) deltas
    in
    List.concatMap fx deltas
