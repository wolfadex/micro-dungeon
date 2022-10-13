port module Main exposing (main)

import Ansi
import Ansi.Color exposing (Color, Location(..))
import Ansi.Cursor
import Ansi.Font
import Json.Decode
import Json.Encode exposing (Value)
import Map exposing (Map, Pnt)
import Terminal


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
    , gameMap : Map Tile
    }


type alias Entity =
    { position : Pnt
    , symbol : String
    , color : Color
    }


type alias Tile =
    { walkable : Bool
    , transparent : Bool
    , symbol : String
    , color : Color
    }


floor : Tile
floor =
    { walkable = True
    , transparent = True
    , symbol = " "
    , color = gray
    }


wall : Tile
wall =
    { walkable = False
    , transparent = False
    , symbol = "#"
    , color = gray
    }


gray : Color
gray =
    Ansi.Color.rgb { red = 205, green = 205, blue = 205 }


boardMax : Pnt
boardMax =
    { column = 80
    , row = 20
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    render
        { player =
            { position =
                { column = boardMax.column // 2
                , row = boardMax.row // 2
                }
            , symbol = "☺"
            , color = Ansi.Color.green
            }
        , gameMap =
            Map.init
                { width = boardMax.column
                , height = boardMax.row
                }
                (\pnt ->
                    if pnt.row == 0 || pnt.row == boardMax.row then
                        wall

                    else if pnt.column == 0 || pnt.column == boardMax.column then
                        wall

                    else
                        floor
                )
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
      , Map.draw drawTile model.gameMap
      , model.player |> drawEntity
      ]
        |> String.concat
        |> stdout
    )


drawTile : Pnt -> Tile -> String
drawTile pnt tile =
    tile.symbol
        |> Terminal.color tile.color
        |> drawAt pnt


drawEntity : Entity -> String
drawEntity ent =
    ent.symbol
        |> Terminal.color ent.color
        |> drawAt ent.position


drawAt : { row : Int, column : Int } -> String -> String
drawAt loc str =
    Ansi.Cursor.moveTo loc ++ str
