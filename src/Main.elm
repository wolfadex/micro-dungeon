port module Main exposing (main)

import Ansi
import Ansi.Color exposing (Location(..))
import Ansi.Cursor
import Ansi.Font
import Map exposing (..)
import Map.Pnt exposing (Pnt)
import Map.Rect exposing (Rect)
import Random exposing (Seed)
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
    , seed : Seed
    , gameMap : Map Tile
    }


boardMax : Pnt
boardMax =
    { column = 80
    , row = 45
    }


init : Int -> ( Model, Cmd Msg )
init randomSeedStarter =
    let
        ( gameMap, seed ) =
            Map.generate
                { columns = boardMax.column
                , rows = boardMax.row
                , roomAttempts = 30
                , roomExtents = { minSize = 6, maxSize = 10 }
                }
                (Random.initialSeed randomSeedStarter)
    in
    render
        { player =
            { position = { column = 40, row = 22 }
            , symbol = "☺"
            , color = Ansi.Color.green
            }
        , seed = seed
        , gameMap = gameMap
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ stdin Stdin
        ]


port stdin : (String -> msg) -> Sub msg



-- port keypress : (Value -> msg) -> Sub msg


port stdout : String -> Cmd msg


type Msg
    = Stdin String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stdin str ->
            { model
                | player =
                    model.player
                        |> moveBy model.gameMap
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


moveBy : Map Tile -> Pnt -> Entity -> Entity
moveBy m amount ({ position } as ent) =
    let
        nextPnt =
            { column =
                (position.column + amount.column)
                    |> max 1
                    |> min boardMax.column
            , row =
                (position.row + amount.row)
                    |> max 1
                    |> min boardMax.row
            }
    in
    case Map.get nextPnt m of
        Nothing ->
            ent

        Just tile ->
            if tile.walkable then
                { ent | position = nextPnt }

            else
                ent


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
