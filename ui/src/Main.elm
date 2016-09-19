module Main exposing (..)

-- import Navigation as Nav exposing (Location)

import Html exposing (Html, div, h1, text, small)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import Http exposing (get, Error)
import Info.Decoder as Info exposing (decodeInfo)
import Info.Model as Info exposing (Model)
import Pages.Login.View as Login exposing (view)
import ProviderList.Decoder as ProviderList exposing (decodeProviderList)
import ProviderList.Model as ProviderList exposing (Model, initModel)
import String exposing (dropRight, endsWith)
import Task exposing (perform)


main : Program Flags
main =
    Html.App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Info Info.Model
    | InfoFailed String
    | ProviderList ProviderList.Model
    | Debug String
    | ProviderListFailed String


type Page
    = Login
    | User
    | Unknown


type alias Flags =
    { url : String }


type alias Model =
    { serverVersion : String
    , url : String
    , redirectPath : String
    , restVersion : String
    , activePage : Page
    , providerList : ProviderList.Model
    , text : String
    , loggedIn : Bool
    , displayName : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProviderList providerlist ->
            ( { model | providerList = providerlist }, Cmd.none )

        ProviderListFailed reason ->
            ( { model | text = reason }, Cmd.none )

        Info info ->
            let
                ( nextPage, nextCmd ) =
                    case info.loggedIn of
                        True ->
                            ( User, Cmd.none )

                        False ->
                            ( Login, retrieveProviderList model.url model.restVersion )
            in
                ( { model
                    | serverVersion = info.version
                    , redirectPath = info.redirectPath
                    , loggedIn = info.loggedIn
                    , displayName = info.displayName
                    , activePage = nextPage
                    , text = model.text ++ " " ++ info.displayName
                  }
                , nextCmd
                )

        InfoFailed reason ->
            ( { model | text = model.text ++ "\n" ++ reason }, Cmd.none )

        Debug data ->
            ( { model | text = data }, Cmd.none )


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
                ]
            , small [] [ text model.serverVersion ]
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

        User ->
            text "you are logged in :)"

        Unknown ->
            text "what ever ..."


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        baseUrl =
            case String.endsWith "/" flags.url of
                True ->
                    String.dropRight 1 flags.url

                False ->
                    flags.url

        restVersion =
            "v2"
    in
        ( { serverVersion = "unknown"
          , restVersion = restVersion
          , url = baseUrl
          , redirectPath = "unknown"
          , providerList = ProviderList.initModel
          , activePage = Login
          , text = baseUrl
          , loggedIn = False
          , displayName = "unknown"
          }
        , retrieveInfo baseUrl restVersion
        )


retrieveInfo : String -> String -> Cmd Msg
retrieveInfo baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/info/"

        fail error =
            case error of
                Http.Timeout ->
                    InfoFailed "Timeout"

                Http.NetworkError ->
                    InfoFailed "Network error"

                Http.UnexpectedPayload load ->
                    InfoFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    InfoFailed ("bad response: " ++ body)

        success providerlist =
            Info providerlist
    in
        Http.get Info.decodeInfo apiUrl
            |> Task.perform fail success


retrieveProviderList : String -> String -> Cmd Msg
retrieveProviderList baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/oidcp/"

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
        Http.get ProviderList.decodeProviderList apiUrl
            |> Task.perform fail success



-- retrieveServiceList : String -> String -> Cmd Msg
-- retrieveServiceList baseUrl restVersion =
--     let
--         apiUrl =
--             baseUrl ++ "/api/" ++ restVersion ++ "/service/"
--         fail error =
--             case error of
--                 Http.Timeout ->
--                     ServiceListFailed "Timeout"
--                 Http.NetworkError ->
--                     ServiceListFailed "Network error"
--                 Http.UnexpectedPayload load ->
--                     ServiceListFailed ("UnexpectedPayload: " ++ load)
--                 Http.BadResponse status body ->
--                     ServiceListFailed ("bad response: " ++ body)
--         success servicelist =
--             ServiceList servicelist
--     in
--         Http.get ServiceList.decodeServiceList apiUrl
--             |> Task.perform fail success
