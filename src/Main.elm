port module Main exposing (main)

import Ansi
import Ansi.Color exposing (Location(..))
import Ansi.Cursor
import Ansi.Font


main : Program Int Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { player : Pnt
    }


type alias Pnt =
    { x : Int
    , y : Int
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    render
        { player = { x = 0, y = 0 }
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    stdin Stdin


port stdin : (String -> msg) -> Sub msg


port stdout : String -> Cmd msg


type Msg
    = Stdin String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stdin str ->
            let
                player =
                    model.player
            in
            { model
                | player =
                    if Ansi.isUpArrow str then
                        { player | y = max (player.y - 1) 0 }

                    else if Ansi.isDownArrow str then
                        { player | y = min (player.y + 1) 20 }

                    else if Ansi.isLeftArrow str then
                        { player | x = max (player.x - 1) 0 }

                    else if Ansi.isRightArrow str then
                        { player | x = min (player.x + 1) 80 }

                    else
                        player
            }
                |> render


render : Model -> ( Model, Cmd Msg )
render model =
    ( model
    , [ Ansi.Cursor.hide
      , Ansi.Font.resetAll
      , Ansi.clearScreen
      , Ansi.setTitle "Micro Dungeon"
      , Ansi.Cursor.moveTo { row = model.player.y, column = model.player.x }
      , "☺"
      ]
        |> String.concat
        |> stdout
    )
