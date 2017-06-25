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
    Random.initialSeed 12


food0 : ( Int, Int )
food0 =
    Tuple.first (Random.step randomPoint seed0)


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
      , seed = seed0
      , score = 0
      , dead = False
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
                newModel =
                    if model.dead then
                        if (code == '\x0D') then
                            Tuple.first init
                        else
                            model
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
                            if (newSnakeHead.x < 0) || (newSnakeHead.x > 100) || (newSnakeHead.y < 0) || (newSnakeHead.y > 100) then
                                { foodChecked | dead = True }
                            else if (member newSnakeHead model.snake) then
                                { foodChecked | dead = True }
                            else
                                foodChecked

                        deathChecked =
                            if model.dead == True then
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
                , ( "top", "20" )
                , ( "bottom", "0" )
                , ( "margin", "auto" )
                , ( "max-width", "100%" )
                , ( "max-height", "100%" )
                , ( "overflow", "auto" )
                , ( "width", "400px" )
                , ( "height", "400px" )
                ]

        message =
            if model.dead == True then
                "Game Over! Press enter to restart"
            else
                ""
    in
        div [ Html.Attributes.style [ ( "backgroundColor", "light grey" ) ] ]
            [ h1 [ Html.Attributes.style [ ( "text-align", "center" ) ] ] [ Html.text ("VIMSNAKE") ]
            , h3 [ Html.Attributes.style [ ( "text-align", "center" ) ] ] [ Html.text ("Score: " ++ toString (model.score)) ]
            , h2 [ Html.Attributes.style [ ( "text-align", "center" ), ( "font-style", "italic" ) ] ] [ Html.text message ]
            , svg
                [ viewBox "0 0 100 100", Svg.Attributes.width "50%", boxStyle ]
                [ polyline [ fill "none", stroke "green", points snakeStr ] []
                , circle [ cx (toString (Tuple.first model.food)), cy (toString (Tuple.second model.food)), r "1", fill "#AF1314", stroke "#AF1314" ] []
                ]
            ]
