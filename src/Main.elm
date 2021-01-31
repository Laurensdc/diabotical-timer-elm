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
    { generatedNumber : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { generatedNumber = 0
      }
    , Random.generate GeneratedNumber timeGenerator
    )



-- UPDATE


type Msg
    = GeneratingNumber
    | GeneratedNumber Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratingNumber ->
            ( model, Random.generate GeneratedNumber timeGenerator )

        GeneratedNumber generated ->
            ( { model | generatedNumber = generated }, Cmd.none )



-- Generate spawn time for Red or Mega


timeGenerator : Random.Generator Int
timeGenerator =
    Random.int 0 59



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
            [ Ui.text ("Red armor spawned at " ++ String.fromInt model.generatedNumber) ]
