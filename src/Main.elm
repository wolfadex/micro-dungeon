port module Main exposing (main)

import Ansi
import Ansi.Color exposing (Location(..))
import Ansi.Cursor
import Ansi.Font
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


main : Program Int Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { player : Entity
    , debug : String
    }


type alias Entity =
    { position : Pnt
    , symbol : String
    }


type alias Pnt =
    { column : Int
    , row : Int
    }


boardMax : Pnt
boardMax =
    { column = 80
    , row = 20
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    render
        { player =
            { position = { column = 0, row = 0 }
            , symbol = "☺"
            }
        , debug = ""
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ stdin Stdin
        , keypress Keypress
        ]


port stdin : (String -> msg) -> Sub msg


port keypress : (Value -> msg) -> Sub msg


port stdout : String -> Cmd msg


type Msg
    = Stdin String
    | Keypress Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stdin str ->
            { model
                | player =
                    model.player
                        |> moveBy
                            (if Ansi.isUpArrow str then
                                { column = 0, row = -1 }

                             else if Ansi.isDownArrow str then
                                { column = 0, row = 1 }

                             else if Ansi.isLeftArrow str then
                                { column = -1, row = 0 }

                             else if Ansi.isRightArrow str then
                                { column = 1, row = 0 }

                             else
                                { column = 0, row = 0 }
                            )
            }
                |> render

        Keypress val ->
            { model
                | debug =
                    val
                        |> Json.Decode.decodeValue Ansi.decodeKey
                        |> Result.map Debug.toString
                        |> Result.mapError Debug.toString
                        |> (\r ->
                                case r of
                                    Ok s ->
                                        s

                                    Err s ->
                                        s
                           )
            }
                |> render


moveBy : Pnt -> Entity -> Entity
moveBy amount ({ position } as ent) =
    { ent
        | position =
            { column =
                (position.column + amount.column)
                    |> max 0
                    |> min boardMax.column
            , row =
                (position.row + amount.row)
                    |> max 0
                    |> min boardMax.row
            }
    }


render : Model -> ( Model, Cmd Msg )
render model =
    ( model
    , [ Ansi.Cursor.hide
      , Ansi.Font.resetAll
      , Ansi.clearScreen
      , Ansi.setTitle "Micro Dungeon"
      , model.player
            |> drawEntity
      ]
        |> String.concat
        |> stdout
    )


drawEntity : Entity -> String
drawEntity ent =
    ent.symbol
        |> drawAt ent.position


drawAt : { row : Int, column : Int } -> String -> String
drawAt loc str =
    Ansi.Cursor.moveTo loc ++ str
