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
    { input : String
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    render
        { input = ""
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
            { model
                | input =
                    -- Delete or Backspace (not sure about forward delete)
                    if str == "\u{007F}" || str == "\u{0008}" then
                        String.dropRight 1 model.input

                    else
                        model.input ++ str
            }
                |> render


render : Model -> ( Model, Cmd Msg )
render model =
    ( model
    , [ Ansi.Cursor.hide
      , Ansi.Font.resetAll
      , Ansi.clearScreen
      , Ansi.Cursor.moveTo { row = 0, column = 0 }
      , "☺"
      ]
        |> String.concat
        |> stdout
    )
