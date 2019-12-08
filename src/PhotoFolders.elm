module PhotoFolders exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)

type alias Model =
  { selectedPhotoUrl : Maybe String
  }

initialModel : Model
initialModel =
  { selectedPhotoUrl = Nothing }

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Http.get
      { url = "http://elm-in-action.com/folders/list"
      , expect = Http.expectJson GotInitialModel modelDecoder
      }
  )

modelDecoder : Decoder Model
modelDecoder =
  Decode.succeed initialModel

type Msg
  = ClickedPhoto String
  | GotInitialModel (Result Http.Error Model)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
          ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
          ( model, Cmd.none )

view : Model -> Html Msg
view model =
    h1 [] [ text "The Grooviest Folders the world ever has seen" ]

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
