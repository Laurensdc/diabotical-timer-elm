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


main : Program () Model Msg
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
    , pastGuesses : PastGuesses
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputGuess = ""
      , currentSpawn = { item = Red, time = 0 }
      , lastGuessCorrectness = HasntAnsweredYet
      , pastGuesses = []
      }
    , Random.generate GeneratedSpawn itemSpawnedGenerator
    )



-- UPDATE


type Msg
    = GeneratingSpawn
    | GeneratedSpawn Spawn
    | InputGuessChanged String
    | GuessSubmitted


type LastGuessCorrectness
    = Correct
    | Wrong
    | HasntAnsweredYet


type alias PastGuesses =
    List { spawn : Spawn, userGuess : Int, theCorrectGuess : Int }


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
            Debug.log "the right answer was" <|
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

        updatedPastGuesses =
            model.pastGuesses ++ [ { spawn = model.currentSpawn, userGuess = guess, theCorrectGuess = theRightAnswer } ]
    in
    { model | lastGuessCorrectness = correctness, pastGuesses = updatedPastGuesses, inputGuess = "" }


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
    -- Global UI 'around' main UI
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
    [ viewSpawn model.currentSpawn
    , viewInputGuess model.inputGuess
    , viewGuessButton
    , viewFeedback model.lastGuessCorrectness
    , viewPastGuesses model.pastGuesses
    ]


viewSpawn : Spawn -> Ui.Element Msg
viewSpawn spawn =
    -- Red armor / mega spawned at xx
    Ui.paragraph [ Font.variant Font.tabularNumbers, Font.center ]
        [ viewItemColored spawn.item
        , Ui.text " spawned at xx:"
        , Ui.el [ Font.bold ] (Ui.text (spawnTimeToStr spawn.time))
        ]


viewInputGuess : String -> Ui.Element Msg
viewInputGuess text =
    -- inputGuess textfield
    Input.text
        [ onEnter GuessSubmitted
        , Ui.width (Ui.shrink |> Ui.minimum 80)
        , Ui.height (Ui.px 40)
        , Ui.centerY
        , Input.focusedOnLoad
        , Font.color <| color TextInverted
        ]
        { onChange = InputGuessChanged
        , text = text
        , placeholder = Just (Input.placeholder [] (Ui.text ""))
        , label = Input.labelLeft [] (Ui.text "Next item at xx:")
        }


viewGuessButton : Ui.Element Msg
viewGuessButton =
    -- Guess btn
    Input.button
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


viewFeedback : LastGuessCorrectness -> Ui.Element Msg
viewFeedback lastGuessCorrectness =
    let
        ( feedbackColor, feedbackText ) =
            case lastGuessCorrectness of
                Correct ->
                    ( color Positive, "Joepie" )

                Wrong ->
                    ( color Negative, "Fuck ge suckt" )

                HasntAnsweredYet ->
                    ( color Text, "Antwoord, slet" )
    in
    Ui.paragraph [ Font.color <| feedbackColor ] [ Ui.text feedbackText ]


viewPastGuesses : PastGuesses -> Ui.Element Msg
viewPastGuesses pastGuesses =
    if List.length pastGuesses == 0 then
        Ui.none

    else
        let
            cellAttrs =
                [ Border.width 1
                , Border.color <| color BgLight
                , Ui.paddingXY 32 16
                , Font.center
                ]
        in
        Ui.table []
            { data = pastGuesses
            , columns =
                [ { header = Ui.el cellAttrs (Ui.text "Spawn")
                  , width = Ui.shrink
                  , view =
                        \guess ->
                            Ui.paragraph (cellAttrs ++ [ Font.alignRight, Font.variant Font.tabularNumbers ]) [ viewItemColored guess.spawn.item, Ui.text (" @ xx:" ++ spawnTimeToStr guess.spawn.time) ]
                  }
                , { header = Ui.el cellAttrs (Ui.text "Correct answer")
                  , width = Ui.shrink
                  , view = \guess -> Ui.el cellAttrs (Ui.text (String.fromInt guess.theCorrectGuess))
                  }
                , { header = Ui.el cellAttrs (Ui.text "Your guess")
                  , width = Ui.shrink
                  , view = \guess -> Ui.el cellAttrs (Ui.text (String.fromInt guess.userGuess))
                  }
                ]
            }


viewItemColored : Item -> Ui.Element msg
viewItemColored item =
    let
        spawncolor =
            if item == Red then
                Ui.rgb 1 0 0

            else
                Ui.rgb 0 1 1
    in
    Ui.el [ Font.bold, Font.color spawncolor ] (Ui.text (itemToString item))


itemToString : Item -> String
itemToString item =
    case item of
        Red ->
            "Red armor"

        Mega ->
            "Mega"


spawnTimeToStr : Int -> String
spawnTimeToStr time =
    if time < 10 && time >= 0 then
        "0" ++ String.fromInt time

    else
        String.fromInt time


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
