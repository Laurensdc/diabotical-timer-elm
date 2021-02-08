module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events exposing (..)
import Json.Decode
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { itemSpawned : Spawn
    , inputGuess : String
    , theRightAnswer : Int
    , gameState : GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { itemSpawned = { time = 0, item = Red }
      , gameState = Loading
      , inputGuess = ""
      , theRightAnswer = 0
      }
    , Random.generate GeneratedItem itemSpawnedGenerator
    )



-- UPDATE


type Msg
    = GeneratingItem
    | GeneratedItem Spawn
    | InputChanged String
    | GuessSubmitted


type GameState
    = Loading
    | Guessed String
    | ItemSpawned Spawn


type alias Spawn =
    { time : Int, item : Item }


type Item
    = Red
    | Mega


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratingItem ->
            ( model, generateNewSpawn )

        GeneratedItem itemSpawned ->
            ( { model | gameState = ItemSpawned itemSpawned }, Cmd.none )

        InputChanged txt ->
            ( { model | inputGuess = txt }, Cmd.none )

        GuessSubmitted ->
            ( validateAnswer model, generateNewSpawn )


generateNewSpawn : Cmd Msg
generateNewSpawn =
    Random.generate GeneratedItem itemSpawnedGenerator



-- Generate spawn time for Red or Mega


validateAnswer : Model -> Model
validateAnswer model =
    let
        item =
            model.itemSpawned.item

        time =
            model.itemSpawned.time

        guess =
            Maybe.withDefault -1 (String.toInt model.inputGuess)

        theRightAnswer =
            case item of
                Red ->
                    if time + 25 >= 60 then
                        time + 25 - 60

                    else
                        time + 25

                Mega ->
                    if time + 35 >= 60 then
                        time + 35 - 60

                    else
                        time + 35
    in
    { model | theRightAnswer = theRightAnswer }


itemSpawnedGenerator : Random.Generator Spawn
itemSpawnedGenerator =
    Random.map2 Spawn timeGenerator itemGenerator


timeGenerator : Random.Generator Int
timeGenerator =
    Random.int 0 59


itemGenerator : Random.Generator Item
itemGenerator =
    Random.uniform Red [ Mega ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
    Ui.layoutWith
        { options =
            -- Global focus style
            [ Ui.focusStyle
                { borderColor = Just (Ui.rgb 0.2 0.2 0.8)
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
        (Ui.column
            -- Main UI component
            [ Ui.centerX
            , Ui.centerY
            , Ui.paddingEach { top = 0, right = 0, bottom = 200, left = 0 }
            , Ui.width Ui.shrink

            -- Global font styles
            , Font.family
                [ Font.external { name = "Open Sans", url = "https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,300;0,400;0,600;0,700;0,800;1,400&display=swap" }
                , Font.typeface "Verdana"
                , Font.sansSerif
                ]
            , Font.size 18
            , Font.color (color Text)
            , Ui.spacing 16
            ]
            (viewCoreGame model)
        )


viewCoreGame : Model -> List (Ui.Element Msg)
viewCoreGame model =
    case model.gameState of
        Loading ->
            [ Ui.none ]

        ItemSpawned spawn ->
            let
                spawncolor =
                    if spawn.item == Red then
                        Ui.rgb 1 0 0

                    else
                        Ui.rgb 0 1 1
            in
            [ Ui.paragraph []
                -- Red armor / mega spawned at
                [ Ui.el [ Font.bold, Font.color spawncolor ] (Ui.text (itemToString spawn.item))
                , Ui.text " spawned at "
                , Ui.el [ Font.bold ] (Ui.text (String.fromInt spawn.time))
                ]
            , Input.text
                [ onEnter GuessSubmitted
                , Ui.width (Ui.shrink |> Ui.minimum 80)
                , Input.focusedOnLoad
                ]
                { onChange = InputChanged
                , text = model.inputGuess
                , placeholder = Just (Input.placeholder [] (Ui.text "25"))
                , label = Input.labelLeft [ Font.semiBold ] (Ui.text "Next item at xx:")
                }
            , Input.button
                [ Border.width 1
                , Border.rounded 8
                , Ui.paddingXY 32 8
                , Ui.mouseOver [ Font.color <| Ui.rgb 1 1 1, Background.color <| Ui.rgb 0.2 0.2 0.2 ]
                , smoothTransition
                ]
                { onPress = Just GuessSubmitted
                , label = Ui.text "Guess"
                }
            ]

        Guessed x ->
            [ Ui.none ]


itemToString : Item -> String
itemToString item =
    case item of
        Red ->
            "Red armor"

        Mega ->
            "Mega"


onEnter : msg -> Ui.Attribute msg
onEnter msg =
    Ui.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )


smoothTransition : Ui.Attribute msg
smoothTransition =
    Ui.htmlAttribute (Html.Attributes.style "transition" "0.15s ease-in-out")


type AppColor
    = Text


color : AppColor -> Ui.Color
color col =
    case col of
        -- Ui.rgb255 0x33 0x33 0x33
        Text ->
            Ui.rgb 0.2 0.2 0.2
