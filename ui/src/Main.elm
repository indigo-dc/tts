module Main exposing (..)

import Html exposing (Html, div, h1, text, small)
import Html.App as App
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Pages.Login.View as Login exposing (view)
import Provider.Model as Provider exposing (Model)


main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }


type Msg
    = Increment
    | Decrement


type Page
    = Login
    | Unknown


type alias Model =
    { version : String
    , redirectPath : String
    , activePage : Page
    , providerList : List Provider.Model
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model

        Decrement ->
            model


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
                    { provider = model.providerList
                    , path = model.redirectPath
                    }
            in
                Login.view context

        Unknown ->
            text "what ever ..."


initModel =
    { version = "0.0.1-alpha"
    , redirectPath = "/oidc"
    , providerList = []
    , activePage = Login
    }
