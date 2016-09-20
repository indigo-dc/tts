module Main exposing (..)

-- import Navigation as Nav exposing (Location)

import CredentialList.Decoder as CredentialList exposing (decodeCredentialList)
import CredentialList.Model as CredentialList exposing (Model, initModel)
import Debug exposing (log)
import Html exposing (Html, div, h1, text, small)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import Http exposing (get, Error)
import Info.Decoder as Info exposing (decodeInfo)
import Info.Model as Info exposing (Model)
import Pages.Login.View as Login exposing (view)
import Pages.User.View as User exposing (view)
import ProviderList.Decoder as ProviderList exposing (decodeProviderList)
import ProviderList.Model as ProviderList exposing (Model, initModel)
import Service.Model as Service exposing (Msg)
import ServiceList.Decoder as ServiceList exposing (decodeServiceList)
import ServiceList.Model as ServiceList exposing (Model, initModel)
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
    | ServiceList ServiceList.Model
    | CredentialList CredentialList.Model
    | ProviderListFailed String
    | ServiceListFailed String
    | CredentialListFailed String
    | LoggedOut
    | LogoutFailed String
    | ServiceMsg Service.Msg


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
    , serviceList : ServiceList.Model
    , credentialList : CredentialList.Model
    , loggedIn : Bool
    , displayName : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Info info ->
            let
                ( nextPage, nextCmd ) =
                    case info.loggedIn of
                        True ->
                            ( User, retrieveServiceList model.url model.restVersion )

                        False ->
                            ( Login, retrieveProviderList model.url model.restVersion )
            in
                ( { model
                    | serverVersion = info.version
                    , redirectPath = info.redirectPath
                    , loggedIn = info.loggedIn
                    , displayName = info.displayName
                    , activePage = nextPage
                  }
                , nextCmd
                )

        LoggedOut ->
            initModel model.url model.restVersion

        ProviderList providerlist ->
            ( { model
                | providerList = providerlist
              }
            , Cmd.none
            )

        ServiceList servicelist ->
            ( { model
                | serviceList = servicelist
              }
            , retrieveCredentialList model.url model.restVersion
            )

        CredentialList credentiallist ->
            ( { model
                | credentialList = credentiallist
              }
            , Cmd.none
            )

        InfoFailed reason ->
            initModel model.url model.restVersion

        ProviderListFailed reason ->
            initModel model.url model.restVersion

        ServiceListFailed reason ->
            initModel model.url model.restVersion

        CredentialListFailed reason ->
            initModel model.url model.restVersion

        LogoutFailed reason ->
            initModel model.url model.restVersion

        ServiceMsg (Service.Logout) ->
            ( model, logout model.url model.restVersion )

        ServiceMsg (Service.Request serviceId) ->
            ( model, request model.url model.restVersion serviceId )


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
                , small [] [ text model.serverVersion ]
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

        User ->
            let
                context =
                    { serviceList = model.serviceList
                    , credentialList = model.credentialList
                    , displayName = model.displayName
                    }
            in
                Html.App.map ServiceMsg <| User.view context

        Unknown ->
            let
                context =
                    { providerList = model.providerList
                    , path = model.redirectPath
                    }
            in
                Login.view context


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
        initModel baseUrl restVersion


initModel : String -> String -> ( Model, Cmd Msg )
initModel baseUrl restVersion =
    ( { serverVersion = "unknown"
      , restVersion = restVersion
      , url = baseUrl
      , redirectPath = "unknown"
      , providerList = ProviderList.initModel
      , serviceList = ServiceList.initModel
      , credentialList = CredentialList.initModel
      , activePage = Login
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


retrieveServiceList : String -> String -> Cmd Msg
retrieveServiceList baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/service/"

        fail error =
            case error of
                Http.Timeout ->
                    ServiceListFailed "Timeout"

                Http.NetworkError ->
                    ServiceListFailed "Network error"

                Http.UnexpectedPayload load ->
                    ServiceListFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    ServiceListFailed ("bad response: " ++ body)

        success servicelist =
            ServiceList (log "serviceList" servicelist)
    in
        Http.get ServiceList.decodeServiceList apiUrl
            |> Task.perform fail success


retrieveCredentialList : String -> String -> Cmd Msg
retrieveCredentialList baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/credential/"

        fail error =
            case error of
                Http.Timeout ->
                    CredentialListFailed "Timeout"

                Http.NetworkError ->
                    CredentialListFailed "Network error"

                Http.UnexpectedPayload load ->
                    CredentialListFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    CredentialListFailed ("bad response: " ++ body)

        success credentiallist =
            CredentialList (log "credentialList" credentiallist)
    in
        Http.get CredentialList.decodeCredentialList apiUrl
            |> Task.perform fail success


logout : String -> String -> Cmd Msg
logout baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/logout/"

        fail error =
            case error of
                Http.Timeout ->
                    LogoutFailed "Timeout"

                Http.NetworkError ->
                    LogoutFailed "Network error"

                Http.UnexpectedPayload load ->
                    LogoutFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    LogoutFailed ("bad response: " ++ body)

        success data =
            LoggedOut
    in
        Http.getString apiUrl
            |> Task.perform fail success
logout : String -> String -> String -> Cmd Msg
logout baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/logout/"

        fail error =
            case error of
                Http.Timeout ->
                    LogoutFailed "Timeout"

                Http.NetworkError ->
                    LogoutFailed "Network error"

                Http.UnexpectedPayload load ->
                    LogoutFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    LogoutFailed ("bad response: " ++ body)

        success data =
            LoggedOut
    in
        Http.getString apiUrl
            |> Task.perform fail success
