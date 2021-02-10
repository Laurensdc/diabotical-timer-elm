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
    { inputGuess : String
    , currentSpawn : Spawn
    , lastGuessCorrectness : LastGuessCorrectness
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputGuess = ""
      , currentSpawn = { item = Red, time = 0 }
      , lastGuessCorrectness = HasntAnsweredYet
      }
    , Random.generate GeneratedSpawn itemSpawnedGenerator
    )



-- UPDATE


type Msg
    = GeneratingSpawn
    | GeneratedSpawn Spawn
    | InputGuessChanged String -- Input field
    | GuessSubmitted -- Spawned item & Int guess of player


type LastGuessCorrectness
    = Correct
    | Wrong
    | HasntAnsweredYet


type alias Spawn =
    { time : Int, item : Item }


type Item
    = Red
    | Mega


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratingSpawn ->
            ( model, generateNewSpawn )

        GeneratedSpawn itemSpawned ->
            ( { model | currentSpawn = itemSpawned }, Cmd.none )

        InputGuessChanged txt ->
            ( { model | inputGuess = txt }, Cmd.none )

        GuessSubmitted ->
            ( validateAnswer model, generateNewSpawn )


generateNewSpawn : Cmd Msg
generateNewSpawn =
    Random.generate GeneratedSpawn itemSpawnedGenerator



-- Generate spawn time for Red or Mega


validateAnswer : Model -> Model
validateAnswer model =
    let
        item =
            model.currentSpawn.item

        time =
            model.currentSpawn.time

        guess =
            Maybe.withDefault -1 (String.toInt model.inputGuess)

        theRightAnswer =
            Debug.log "theRightAnswer" <|
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

        correctness : LastGuessCorrectness
        correctness =
            if guess == theRightAnswer then
                Correct

            else
                Wrong
    in
    { model | lastGuessCorrectness = correctness, inputGuess = "" }


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
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow =
                    Just
                        { color = Ui.rgba 1 1 1 0.4
                        , offset = ( 0, 0 )
                        , blur = 5
                        , size = 2
                        }
                }
            ]
        }
        [ Background.color (color Bg) ]
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
    let
        ( feedbackColor, feedbackText ) =
            case model.lastGuessCorrectness of
                Correct ->
                    ( color Positive, "Joepie" )

                Wrong ->
                    ( color Negative, "Fuck ge suckt" )

                HasntAnsweredYet ->
                    ( color Text, "Antwoord, slet" )

        feedback =
            Ui.paragraph [ Font.color <| feedbackColor ] [ Ui.text feedbackText ]
    in
    [ viewSpawn model.currentSpawn

    -- inputGuess textfield
    , Input.text
        [ onEnter GuessSubmitted
        , Ui.width (Ui.shrink |> Ui.minimum 80)
        , Ui.height (Ui.px 40)
        , Ui.centerY
        , Input.focusedOnLoad
        , Font.color <| color TextInverted
        ]
        { onChange = InputGuessChanged
        , text = model.inputGuess
        , placeholder = Just (Input.placeholder [ Font.color <| color TextInverted ] (Ui.text ""))
        , label = Input.labelLeft [] (Ui.text "Next item at xx:")
        }

    -- Guess btn
    , Input.button
        [ Border.width 1
        , Border.rounded 8
        , Border.color <| color Text
        , Background.color <| color BgLight
        , Font.color <| color Text
        , Ui.paddingXY 32 8
        , Ui.mouseOver
            [ Font.color <| color Highlight
            , Border.color <| color Highlight
            ]
        , smoothTransition
        ]
        { onPress = Just GuessSubmitted
        , label = Ui.text "Guess"
        }
    , feedback
    ]


viewSpawn : Spawn -> Ui.Element Msg
viewSpawn spawn =
    -- Red armor / mega spawned at xx
    let
        spawncolor =
            if spawn.item == Red then
                Ui.rgb 1 0 0

            else
                Ui.rgb 0 1 1
    in
    Ui.paragraph []
        [ Ui.el [ Font.bold, Font.color spawncolor ] (Ui.text (itemToString spawn.item))
        , Ui.text " spawned at "
        , Ui.el [ Font.bold ] (Ui.text (String.fromInt spawn.time))
        ]


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
    | TextInverted
    | Bg
    | BgLight
    | Highlight
    | Negative
    | Positive


color : AppColor -> Ui.Color
color col =
    case col of
        Text ->
            Ui.rgb255 0xEE 0xEE 0xEE

        TextInverted ->
            Ui.rgb255 0x33 0x33 0x33

        Bg ->
            Ui.rgb255 0x22 0x28 0x31

        BgLight ->
            Ui.rgb255 0x39 0x3E 0x46

        Highlight ->
            Ui.rgb255 0xFF 0xD3 0x69

        Negative ->
            Ui.rgb255 248 102 113

        Positive ->
            Ui.rgb255 98 209 86
