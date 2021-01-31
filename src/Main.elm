module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element as Ui
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { itemSpawned = { time = 0, item = Red }
      }
    , Random.generate GeneratedItem generateItem
    )



-- UPDATE


type Msg
    = GeneratingItem
    | GeneratedItem ItemSpawned


type alias ItemSpawned =
    { time : Int, item : Item }


type Item
    = Red
    | Mega


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratingItem ->
            ( model, Random.generate GeneratedItem generateItem )

        GeneratedItem generated ->
            ( { model | itemSpawned = generated }, Cmd.none )



-- Generate spawn time for Red or Mega


timeGenerator : Random.Generator Int
timeGenerator =
    Random.int 0 59


itemGenerator : Random.Generator Item
itemGenerator =
    Random.uniform Red [ Mega ]


generateItem : Random.Generator ItemSpawned
generateItem =
    Random.map2 ItemSpawned timeGenerator itemGenerator



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
            [ Ui.text (itemToString model.itemSpawned.item ++ " spawned at " ++ String.fromInt model.itemSpawned.time) ]


itemToString : Item -> String
itemToString item =
    case item of
        Red ->
            "Red armor"

        Mega ->
            "Mega"



-- ++ String.fromInt model.generatedNumber) ]
