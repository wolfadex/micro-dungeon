port module Main exposing (main)

import Ansi
import Ansi.Color exposing (Location(..))
import Ansi.Cursor
import Ansi.Font
import AssocSet
import Map exposing (..)
import Map.Pnt exposing (Pnt)
import Map.Rect exposing (Rect)
import Map.Shapes
import Random exposing (Seed)
import Set exposing (Set)
import Terminal


possibleSymbols =
    """
֎
۞
๏
ᚙ
᳀
⁜
░
▒
▓
▩
◯
◦
◉
◢
◣
◤
◥
█
"""


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
    , hasSeen : AssocSet.Set Pnt
    , canSee : AssocSet.Set Pnt
    }


boardMax : Pnt
boardMax =
    { column = 80
    , row = 45
    }


init : Int -> ( Model, Cmd Msg )
init randomSeedStarter =
    let
        ( gameMap, seed, startPos ) =
            Map.generate
                { columns = boardMax.column
                , rows = boardMax.row
                , roomAttempts = 30
                , roomExtents = { minSize = 6, maxSize = 10 }
                }
                (Random.initialSeed randomSeedStarter)

        player =
            { position = startPos
            , symbol = "☺"
            , color = Ansi.Color.green
            }

        baseModel =
            { player = player
            , seed = seed
            , gameMap = gameMap
            , hasSeen = AssocSet.empty
            , canSee = AssocSet.empty
            }

        initialSeen =
            calculateVision baseModel
    in
    render
        { baseModel
            | hasSeen = initialSeen
            , canSee = initialSeen
        }


calculateVision : Model -> AssocSet.Set Pnt
calculateVision model =
    Map.Shapes.circle 8 model.player.position
        |> List.concatMap
            (\edgePnt ->
                Map.Shapes.line model.player.position edgePnt
                    |> keepUntil model.gameMap []
            )
        |> AssocSet.fromList


keepUntil : Map Tile -> List Pnt -> List Pnt -> List Pnt
keepUntil m result input =
    case input of
        [] ->
            List.reverse result

        next :: rest ->
            case Map.get next m of
                Nothing ->
                    keepUntil m (next :: result) []

                Just t ->
                    if t.transparent then
                        keepUntil m (next :: result) rest

                    else
                        keepUntil m (next :: result) []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ stdin Stdin
        ]


port stdin : (String -> msg) -> Sub msg


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
                |> setSeen
                |> render


setSeen : Model -> Model
setSeen model =
    let
        canSee =
            calculateVision model
    in
    { model
        | canSee = canSee
        , hasSeen = AssocSet.union model.hasSeen canSee
    }


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
      , Map.draw
            { hasSeen = model.hasSeen
            , canSee = model.canSee
            }
            model.gameMap
      , model.player |> drawEntity
      ]
        |> String.concat
        |> stdout
    )


drawAt : { row : Int, column : Int } -> String -> String
drawAt loc str =
    Ansi.Cursor.moveTo loc ++ str


drawEntity : Entity -> String
drawEntity ent =
    ent.symbol
        |> Terminal.color ent.color
        |> drawAt ent.position
