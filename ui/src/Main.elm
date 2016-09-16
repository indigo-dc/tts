module Main exposing (..)

import Html exposing (Html, div, h1, text, small)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (get)
import Pages.Login.View as Login exposing (view)
import Provider.Model as Provider exposing (Model)
import ProviderList.Decoder as ProviderList exposing (decodeProviderList)
import ProviderList.Model as ProviderList exposing (Model, initModel)
import Task exposing (perform)


-- programWithFlags later on so I can decide if user is logged in or not ...


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
    | ProviderListFailed


type Page
    = Login
    | Unknown


type alias Model =
    { version : String
    , redirectPath : String
    , activePage : Page
    , providerList : ProviderList.Model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RetrieveProviderList ->
            ( model, Cmd.none )

        ProviderList providerlist ->
            ( { model | providerList = providerlist }, Cmd.none )

        ProviderListFailed ->
            ( model, Cmd.none )


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


init =
    ( { version = "0.0.1-alpha"
      , redirectPath = "/oidc"
      , providerList = ProviderList.initModel
      , activePage = Login
      }
    , getProviderList
    )


getProviderList =
    let
        url =
            "http://localhost:8080/api/v2/oidcp/"

        fail something =
            ProviderListFailed

        success providerlist =
            ProviderList providerlist
    in
        Task.perform fail success (Http.get ProviderList.decodeProviderList url)
