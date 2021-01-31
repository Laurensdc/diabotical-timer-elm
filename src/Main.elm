module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element as Ui
import Element.Input
import Html
import Html.Attributes exposing (..)
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
    { itemSpawned : ItemSpawned
    , inputGuess : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { itemSpawned = { time = 0, item = Red }
      , inputGuess = ""
      }
    , Random.generate GeneratedItem itemSpawnedGenerator
    )



-- UPDATE


type Msg
    = GeneratingItem
    | GeneratedItem ItemSpawned
    | InputChanged String
    | GuessSubmitted


type alias ItemSpawned =
    { time : Int, item : Item }


type Item
    = Red
    | Mega


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratingItem ->
            ( model, Random.generate GeneratedItem itemSpawnedGenerator )

        GeneratedItem itemSpawned ->
            ( { model | itemSpawned = itemSpawned }, Cmd.none )

        InputChanged txt ->
            ( { model | inputGuess = txt }, Cmd.none )

        GuessSubmitted ->
            ( model, Cmd.none )



-- Generate spawn time for Red or Mega


itemSpawnedGenerator : Random.Generator ItemSpawned
itemSpawnedGenerator =
    Random.map2 ItemSpawned timeGenerator itemGenerator


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
    Ui.layout [] <|
        Ui.column
            [ Ui.centerX, Ui.centerY, Ui.paddingEach { top = 0, right = 0, bottom = 200, left = 0 } ]
            [ Ui.text (itemToString model.itemSpawned.item ++ " spawned at " ++ String.fromInt model.itemSpawned.time)
            , Element.Input.text [ onEnter GuessSubmitted ]
                { onChange = InputChanged
                , text = model.inputGuess
                , placeholder = Just (Element.Input.placeholder [] (Ui.text "placeholder"))
                , label = Element.Input.labelLeft [] (Ui.text "Next item at xx:")
                }
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
