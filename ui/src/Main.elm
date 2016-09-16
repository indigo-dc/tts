module Main exposing (..)

import Html exposing (Html, div, h1, text, small)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import Http exposing (get, Error)
import Pages.Login.View as Login exposing (view)
import ProviderList.Decoder as ProviderList exposing (decodeProviderList)
import ProviderList.Model as ProviderList exposing (Model, initModel)
import Task exposing (perform)


-- programWithFlags later on so I can decide if user is logged in or not ...


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = RetrieveProviderList
    | ProviderList ProviderList.Model
    | Debug String
    | ProviderListFailed String


type Page
    = Login
    | Unknown


type alias Model =
    { version : String
    , redirectPath : String
    , activePage : Page
    , providerList : ProviderList.Model
    , text : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RetrieveProviderList ->
            ( model, Cmd.none )

        ProviderList providerlist ->
            ( { model | providerList = providerlist }, Cmd.none )

        Debug data ->
            ( { model | text = data }, Cmd.none )

        ProviderListFailed info ->
            ( { model | text = info }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div [ class "container" ]
            [ h1 []
                [ text "Token Translation Service"
                , text " "
                , small [] [ text model.version ]
                ]
            , mainContent model
            ]
        , div [] [ text model.text ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    case model.activePage of
        Login ->
            let
                context =
                    { providerList = model.providerList
                    , path = model.redirectPath
                    }
            in
                Login.view context

        Unknown ->
            text "what ever ..."


init : ( Model, Cmd Msg )
init =
    ( { version = "0.0.1-alpha"
      , redirectPath = "/oidc"
      , providerList = ProviderList.initModel
      , activePage = Login
      , text = ""
      }
    , getProviderList
    )


getProviderList : Cmd Msg
getProviderList =
    let
        url =
            "http://localhost:8080/api/v2/oidcp/"

        fail error =
            case error of
                Http.Timeout ->
                    ProviderListFailed "Timeout"

                Http.NetworkError ->
                    ProviderListFailed "Network error"

                Http.UnexpectedPayload load ->
                    ProviderListFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    ProviderListFailed ("bad response: " ++ body)

        success providerlist =
            ProviderList providerlist
    in
        Http.get ProviderList.decodeProviderList url
            |> Task.perform fail success
