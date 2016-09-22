module Main exposing (..)

-- import Json.Decode as Decoder

import AccessToken.Decoder exposing (decodeAccessToken)
import AccessToken.Model as AccessToken exposing (Model)
import CredentialList.Decoder as CredentialList exposing (decodeCredentialList)
import CredentialList.Model as CredentialList exposing (Model, initModel)
import Html exposing (Html, div, h1, text, small)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import Http exposing (get, post, send, defaultSettings, Error)
import Info.Decoder as Info exposing (decodeInfo)
import Messages exposing (Msg)
import Pages.Login.View as Login exposing (view)
import Pages.User.View as User exposing (view)
import ProviderList.Decoder as ProviderList exposing (decodeProviderList)
import ProviderList.Model as ProviderList exposing (Model, initModel)
import Secret.Decoder exposing (decodeSecret)
import Secret.Model as Secret exposing (Model)
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
    , credential : Maybe Secret.Model
    , accessToken : AccessToken.Model
    , loggedIn : Bool
    , displayName : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages.Info info ->
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

        Messages.LoggedOut ->
            initModel model.url model.restVersion

        Messages.ProviderList providerlist ->
            ( { model
                | providerList = providerlist
              }
            , Cmd.none
            )

        Messages.ServiceList servicelist ->
            ( { model
                | serviceList = servicelist
              }
            , retrieveCredentialList model.url model.restVersion
            )

        Messages.CredentialList credentiallist ->
            ( { model
                | credentialList = credentiallist
              }
            , Cmd.none
            )

        Messages.Credential credential ->
            ( { model
                | credential = Just credential
              }
            , retrieveCredentialList model.url model.restVersion
            )

        Messages.InfoFailed reason ->
            initModel model.url model.restVersion

        Messages.ProviderListFailed reason ->
            ( model, Cmd.none )

        Messages.ServiceListFailed reason ->
            ( model, logout model.url model.restVersion )

        Messages.RequestFailed reason ->
            ( model, Cmd.none )

        Messages.RevokeFailed reason ->
            ( model, Cmd.none )

        Messages.CredentialListFailed reason ->
            ( model, Cmd.none )

        Messages.LogoutFailed reason ->
            initModel model.url model.restVersion

        Messages.Logout ->
            ( model, logout model.url model.restVersion )

        Messages.Request serviceId ->
            ( model, request model.url model.restVersion serviceId )

        Messages.AdvancedRequest serviceId ->
            ( model, Cmd.none )

        Messages.Revoke credId ->
            ( model, revoke model.url model.restVersion credId )

        Messages.RetrieveAccessToken ->
            ( model, retrieveAccessToken model.url model.restVersion )

        Messages.Requested credential ->
            ( { model
                | credential = Just credential
              }
            , retrieveServiceList model.url model.restVersion
            )

        Messages.AccessTokenFailed error ->
            ( model, Cmd.none )

        Messages.AccessToken token ->
            ( { model | accessToken = token }, Cmd.none )

        Messages.HideAccessToken ->
            ( { model | accessToken = AccessToken.initModel }, Cmd.none )

        Messages.HideSecret ->
            ( { model | credential = Nothing }, Cmd.none )

        Messages.Revoked ->
            ( model, retrieveServiceList model.url model.restVersion )


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
                    , accessToken = model.accessToken
                    , secret = model.credential
                    }
            in
                User.view context

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
        withoutHash =
            case String.endsWith "#" flags.url of
                True ->
                    String.dropRight 1 flags.url

                False ->
                    flags.url

        baseUrl =
            case String.endsWith "/" withoutHash of
                True ->
                    String.dropRight 1 withoutHash

                False ->
                    withoutHash

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
      , accessToken = AccessToken.initModel
      , credential = Nothing
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
                    Messages.InfoFailed "Timeout"

                Http.NetworkError ->
                    Messages.InfoFailed "Network error"

                Http.UnexpectedPayload load ->
                    Messages.InfoFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    Messages.InfoFailed ("bad response: " ++ body)

        success providerlist =
            Messages.Info providerlist
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
                    Messages.ProviderListFailed "Timeout"

                Http.NetworkError ->
                    Messages.ProviderListFailed "Network error"

                Http.UnexpectedPayload load ->
                    Messages.ProviderListFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    Messages.ProviderListFailed ("bad response: " ++ body)

        success providerlist =
            Messages.ProviderList providerlist
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
                    Messages.ServiceListFailed "Timeout"

                Http.NetworkError ->
                    Messages.ServiceListFailed "Network error"

                Http.UnexpectedPayload load ->
                    Messages.ServiceListFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    Messages.ServiceListFailed ("bad response: " ++ body)

        success servicelist =
            Messages.ServiceList servicelist
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
                    Messages.CredentialListFailed "Timeout"

                Http.NetworkError ->
                    Messages.CredentialListFailed "Network error"

                Http.UnexpectedPayload load ->
                    Messages.CredentialListFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    Messages.CredentialListFailed ("bad response: " ++ body)

        success credentiallist =
            Messages.CredentialList credentiallist
    in
        Http.get CredentialList.decodeCredentialList apiUrl
            |> Task.perform fail success


retrieveAccessToken : String -> String -> Cmd Msg
retrieveAccessToken baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/access_token/"

        fail error =
            case error of
                Http.Timeout ->
                    Messages.AccessTokenFailed "Timeout"

                Http.NetworkError ->
                    Messages.AccessTokenFailed "Network error"

                Http.UnexpectedPayload load ->
                    Messages.AccessTokenFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    Messages.AccessTokenFailed ("bad response: " ++ body)

        success accessToken =
            Messages.AccessToken accessToken
    in
        Http.get decodeAccessToken apiUrl
            |> Task.perform fail success


logout : String -> String -> Cmd Msg
logout baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/logout/"

        fail error =
            case error of
                Http.Timeout ->
                    Messages.LogoutFailed "Timeout"

                Http.NetworkError ->
                    Messages.LogoutFailed "Network error"

                Http.UnexpectedPayload load ->
                    Messages.LogoutFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    Messages.LogoutFailed ("bad response: " ++ body)

        success data =
            Messages.LoggedOut
    in
        Http.getString apiUrl
            |> Task.perform fail success


request : String -> String -> String -> Cmd Msg
request baseUrl restVersion serviceId =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/credential/"

        data =
            Http.string ("{\"service_id\":\"" ++ serviceId ++ "\"}")

        request =
            { verb = "POST"
            , headers = [ ( "Content-Type", "application/json" ) ]
            , url = apiUrl
            , body = data
            }

        fail error =
            case error of
                Http.Timeout ->
                    Messages.RequestFailed "Timeout"

                Http.NetworkError ->
                    Messages.RequestFailed "Network error"

                Http.UnexpectedPayload load ->
                    Messages.RequestFailed ("UnexpectedPayload: " ++ load)

                Http.BadResponse status body ->
                    Messages.RequestFailed ("bad response: " ++ body)

        success credential =
            Messages.Requested credential
    in
        Http.send Http.defaultSettings request
            |> Http.fromJson decodeSecret
            |> Task.perform fail success


revoke : String -> String -> String -> Cmd Msg
revoke baseUrl restVersion credId =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/credential/" ++ credId

        request =
            { verb = "DELETE"
            , headers = []
            , url = apiUrl
            , body = Http.empty
            }

        fail error =
            case error of
                Http.RawTimeout ->
                    Messages.RevokeFailed "Timeout"

                Http.RawNetworkError ->
                    Messages.RevokeFailed "Network error"

        success data =
            Messages.Revoked
    in
        Http.send Http.defaultSettings request
            |> Task.perform fail success
