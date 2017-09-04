module Vimsnake exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard exposing (..)
import Char exposing (..)
import Time exposing (Time, millisecond)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import Random exposing (Seed, Generator)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { snake : List Point
    , dir : Direction
    , food : ( Int, Int )
    , seed : Seed
    , score : Int
    , dead : Bool
    , started : Bool
    }


type Direction
    = Left
    | Down
    | Up
    | Right


type alias Point =
    { x : Int, y : Int }


type Msg
    = Presses Char
    | Tick Time


seed0 : Seed
seed0 =
    Random.initialSeed 1


randomStuff =
    Random.step randomPoint seed0


food0 : ( Int, Int )
food0 =
    Tuple.first randomStuff


seed1 : Seed
seed1 =
    Tuple.second randomStuff


init : ( Model, Cmd Msg )
init =
    ( { snake =
            [ { x = 20, y = 10 }
            , { x = 18, y = 10 }
            , { x = 16, y = 10 }
            , { x = 14, y = 10 }
            , { x = 12, y = 10 }
            , { x = 10, y = 10 }
            ]
      , dir = Right
      , food = food0
      , seed = seed1
      , score = 0
      , dead = False
      , started = False
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ presses (\code -> Presses (fromCode code))
        , Time.every (60 * millisecond) Tick
        ]



-- UPDATE


randomPoint : Generator ( Int, Int )
randomPoint =
    Random.pair (Random.int 1 99) (Random.int 1 99)


oppositeDir : Direction -> Direction
oppositeDir direction =
    case direction of
        Left ->
            Right

        Down ->
            Up

        Up ->
            Down

        Right ->
            Left


redirect : Char -> Model -> Model
redirect code model =
    let
        direction =
            if code == 'h' then
                Left
            else if code == 'j' then
                Down
            else if code == 'k' then
                Up
            else if code == 'l' then
                Right
            else
                model.dir

        newDir =
            if direction == (oppositeDir model.dir) then
                model.dir
            else
                direction
    in
        { model | dir = newDir }


makeFood : Model -> Model
makeFood model =
    let
        randomStuff =
            Random.step (randomPoint) model.seed

        newFood =
            Tuple.first (randomStuff)

        newSeed =
            Tuple.second (randomStuff)
    in
        { model
            | food = newFood
            , seed = newSeed
            , score = model.score + 1
        }


makeHead : Direction -> Point -> Point
makeHead direction point =
    case direction of
        Left ->
            Point (point.x - 2) point.y

        Down ->
            Point point.x (point.y + 2)

        Up ->
            Point point.x (point.y - 2)

        Right ->
            Point (point.x + 2) point.y


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Presses code ->
            let
                initModel =
                    Tuple.first init

                newModel =
                    if model.dead then
                        if (code == '\x0D') then
                            { initModel | started = True }
                        else
                            model
                    else if not model.started then
                        { model | started = True }
                    else
                        redirect code model
            in
                ( newModel, Cmd.none )

        Tick newTime ->
            case model.snake of
                snakeHead :: _ ->
                    let
                        newSnakeHead =
                            makeHead model.dir snakeHead

                        keepLength =
                            length model.snake - 1

                        newSnake =
                            newSnakeHead :: (take keepLength model.snake)

                        snakeChecked =
                            { model | snake = newSnake }

                        closeToFood =
                            sqrt ((toFloat (newSnakeHead.x - (Tuple.first model.food)) ^ 2) + (toFloat (newSnakeHead.y - (Tuple.second model.food)) ^ 2))

                        foodChecked =
                            if closeToFood < 2.4 then
                                let
                                    bigSnakeHead =
                                        makeHead model.dir newSnakeHead

                                    biggerSnake =
                                        bigSnakeHead :: newSnake

                                    foodModel =
                                        makeFood snakeChecked
                                in
                                    { foodModel | snake = biggerSnake }
                            else
                                snakeChecked

                        boundsChecked =
                            if (newSnakeHead.x < 1) || (newSnakeHead.x > 99) || (newSnakeHead.y < 1) || (newSnakeHead.y > 99) then
                                { foodChecked | dead = True }
                            else if (member newSnakeHead model.snake) then
                                { foodChecked | dead = True }
                            else
                                foodChecked

                        deathChecked =
                            if not model.started || model.dead then
                                model
                            else
                                boundsChecked
                    in
                        ( deathChecked, Cmd.none )

                [] ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        snakeStrList =
            List.map (\point -> (toString point.x) ++ "," ++ (toString point.y)) model.snake

        snakeStr =
            foldr (++) "" <| intersperse " " <| snakeStrList

        boxStyle =
            Html.Attributes.style
                [ ( "border", "4px solid black" )
                , ( "position", "absolute" )
                , ( "left", "0" )
                , ( "right", "0" )
                , ( "top", "0" )
                , ( "bottom", "0" )
                , ( "margin", "auto" )
                , ( "max-width", "100%" )
                , ( "max-height", "100%" )
                , ( "overflow", "auto" )
                , ( "width", "400px" )
                , ( "height", "400px" )
                , ( "backgroundColor", "#D3D3D3" )
                ]

        titleStyle =
            Html.Attributes.style
                [ ( "text-align", "center" )
                , ( "padding-top", "5%" )
                , ( "color", "#98FB98" )
                , ( "text-shadow", "2px 2px 4px grey" )
                ]

        hideForIntro =
            if model.started then
                "inherit"
            else
                "none"

        message1 =
            if not model.started then
                Svg.text_ [ x "19", y "45", fontSize "12", fill "green" ] [ Svg.text "VimSnake" ]
            else if model.dead then
                Svg.text_ [ x "20", y "45", fontSize "10", fill "#AF1314" ] [ Svg.text "GAME OVER!" ]
            else
                Svg.text_ [] []

        message2 =
            if not model.started then
                Svg.text_ [ x "22", y "55", fontSize "5", fill "#474747" ] [ Svg.text "Press any key to start" ]
            else if model.dead then
                Svg.text_ [ x "26", y "53", fontSize "5", fill "#474747" ] [ Svg.text "Press enter to restart" ]
            else
                Svg.text_ [] []
    in
        div [ Html.Attributes.style [ ( "fontFamily", "Verdana, Geneva, sans-serif" ), ( "backgroundColor", "black" ), ( "height", "100%" ), ( "marginTop", "-20px" ) ] ]
            [ svg
                [ viewBox "0 0 100 100", Svg.Attributes.width "50%", boxStyle ]
                [ polyline [ fill "none", stroke "green", points snakeStr, display hideForIntro ] []
                , circle [ cx (toString (Tuple.first model.food)), cy (toString (Tuple.second model.food)), r "1", fill "#AF1314", stroke "#AF1314", display hideForIntro ] []
                , Svg.text_ [ x "2", y "6", fontSize "4", fill "black", display hideForIntro ] [ Svg.text ("Score: " ++ toString (model.score)) ]
                , message1
                , message2
                , image [ xlinkHref "./vim_keys.png", x "60", y "2", Svg.Attributes.height "10", display hideForIntro ] []
                ]
            ]
