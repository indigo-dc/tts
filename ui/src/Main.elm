module Main exposing (..)

-- import Json.Decode as Decoder

import AccessToken.Decoder exposing (decodeAccessToken)
import AccessToken.Model as AccessToken exposing (Model)
import CredentialList.Decoder as CredentialList exposing (decodeCredentialList)
import CredentialList.Model as CredentialList exposing (Model, initModel)
import Debug exposing (log)
import Dict exposing (Dict, empty, insert)
import Html exposing (Html, a, div, h1, text, small, strong)
import Html.Attributes exposing (class, hidden, style, href)
import Http exposing (get, post, send, Error)
import Info.Decoder as Info exposing (decodeInfo)
import Json.Decode exposing (decodeString)
import Json.Encode as Json exposing (encode)
import Messages exposing (Msg)
import Pages.Login.View as Login exposing (view)
import Pages.User.View as User exposing (view)
import ProviderList.Decoder as ProviderList exposing (decodeProviderList)
import ProviderList.Model as ProviderList exposing (Model, initModel)
import Secret.Decoder exposing (decodeSecret)
import Secret.Model as Secret exposing (Model, empty_credential)
import Service.Model as Service exposing (Model)
import ServiceList.Decoder as ServiceList exposing (decodeServiceList)
import ServiceList.Model as ServiceList exposing (Model, initModel)
import String exposing (dropRight, endsWith)


main : Program Flags Model Msg
main =
    Html.programWithFlags
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
    , error : String
    , displayName : String
    , current_service : Maybe Service.Model
    , current_param : Maybe (Dict String Json.Value)
    , request_progressing : Bool
    , progressing_title : Maybe String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages.Info (Ok info) ->
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
                    , error = info.error
                    , displayName = info.displayName
                    , activePage = nextPage
                  }
                , nextCmd
                )

        Messages.Info (Err info) ->
            let
                res =
                    Debug.log "info:" info
            in
                ( model, Cmd.none )

        Messages.ProviderList (Ok providerlist) ->
            ( { model
                | providerList = providerlist
              }
            , Cmd.none
            )

        Messages.ProviderList (Err info) ->
            ( model, Cmd.none )

        Messages.ServiceList (Ok servicelist) ->
            ( { model
                | serviceList = servicelist
              }
            , retrieveCredentialList model.url model.restVersion
            )

        Messages.ServiceList (Err info) ->
            ( model, Cmd.none )

        Messages.CredentialList (Ok credentiallist) ->
            ( { model
                | credentialList = credentiallist
              }
            , Cmd.none
            )

        Messages.CredentialList (Err info) ->
            ( model, Cmd.none )

        Messages.Logout ->
            ( model, logout model.url model.restVersion )

        Messages.LoggedOut _ ->
            initModel model.url model.restVersion

        Messages.Request serviceId ->
            ( { model
                | current_service = Nothing
                , current_param = Nothing
                , request_progressing = True
                , progressing_title = Just "Requesting Credential ..."
              }
            , request model.url model.restVersion serviceId model.current_param
            )

        Messages.Requested (Ok credential) ->
            ( { model
                | credential = Just credential
                , request_progressing = False
                , progressing_title = Nothing
              }
            , retrieveServiceList model.url model.restVersion
            )

        Messages.Requested (Err (Http.BadStatus info)) ->
            let
                result =
                    case info.status.code of
                        401 ->
                            initModel model.url model.restVersion

                        _ ->
                            let
                                err_code =
                                    toString info.status.code

                                err_msg =
                                    "bad response [" ++ err_code ++ "]: " ++ info.body

                                error_cred =
                                    case decodeString decodeSecret info.body of
                                        Ok cred ->
                                            Just cred

                                        Err _ ->
                                            Just
                                                { result = "error"
                                                , credential = Secret.empty_credential
                                                , error = err_msg
                                                }
                            in
                                ( { model
                                    | credential = error_cred
                                    , request_progressing = False
                                    , progressing_title = Nothing
                                  }
                                , retrieveServiceList model.url model.restVersion
                                )
            in
                result

        Messages.Requested (Err info) ->
            let
                bla =
                    Debug.log "reques failed" info

                err_msg =
                    "an internal error occured, see the console for more details"

                error_cred =
                    Just
                        { result = "error"
                        , credential = Secret.empty_credential
                        , error = err_msg
                        }
            in
                ( { model
                    | credential = error_cred
                    , request_progressing = False
                    , progressing_title = Nothing
                  }
                , retrieveServiceList model.url model.restVersion
                )

        Messages.Revoke credId ->
            ( { model
                | request_progressing = True
                , progressing_title = Just "Revoking Credential ..."
              }
            , revoke model.url model.restVersion credId
            )

        Messages.Revoked (Ok _) ->
            ( { model
                | request_progressing = False
                , progressing_title = Nothing
              }
            , retrieveServiceList model.url model.restVersion
            )

        Messages.Revoked (Err (Http.BadStatus info)) ->
            let
                result =
                    case info.status.code of
                        401 ->
                            initModel model.url model.restVersion

                        _ ->
                            let
                                err_code =
                                    toString info.status.code

                                err_msg =
                                    "bad response [" ++ err_code ++ "]: " ++ info.body

                                error_cred =
                                    case decodeString decodeSecret info.body of
                                        Ok cred ->
                                            Just cred

                                        Err _ ->
                                            Just
                                                { result = "error"
                                                , credential = Secret.empty_credential
                                                , error = err_msg
                                                }
                            in
                                ( { model
                                    | credential = error_cred
                                    , request_progressing = False
                                    , progressing_title = Nothing
                                  }
                                , retrieveServiceList model.url model.restVersion
                                )
            in
                result

        Messages.Revoked (Err info) ->
            let
                bla =
                    Debug.log "revoke failed" info

                err_msg =
                    "an internal error occured, see the console for more details"

                error_cred =
                    Just
                        { result = "error"
                        , credential = Secret.empty_credential
                        , error = err_msg
                        }
            in
                ( { model
                    | credential = error_cred
                    , request_progressing = False
                    , progressing_title = Nothing
                  }
                , retrieveServiceList model.url model.restVersion
                )

        Messages.AdvancedRequest service ->
            ( { model
                | current_service = Just service
              }
            , Cmd.none
            )

        Messages.AdvancedChange name value ->
            let
                jsonValue =
                    Json.string value

                newParams =
                    case model.current_param of
                        Just params ->
                            Dict.insert name jsonValue params

                        Nothing ->
                            Dict.insert name jsonValue Dict.empty
            in
                ( { model
                    | current_param = Just newParams
                  }
                , Cmd.none
                )

        Messages.AdvancedCancel ->
            ( { model
                | current_service = Nothing
                , current_param = Nothing
              }
            , Cmd.none
            )

        Messages.RetrieveAccessToken ->
            ( model, retrieveAccessToken model.url model.restVersion )

        Messages.AccessToken (Ok token) ->
            ( { model | accessToken = token }, Cmd.none )

        Messages.AccessToken (Err info) ->
            ( model, Cmd.none )

        Messages.HideAccessToken ->
            ( { model | accessToken = AccessToken.initModel }, Cmd.none )

        Messages.HideSecret ->
            ( { model | credential = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        hideError =
            case model.error of
                "" ->
                    True

                _ ->
                    False
    in
        div []
            [ div [ class "container" ]
                [ h1 []
                    [ text "INDIGO Token Translation Service" ]
                , div [ class "alert alert-danger", hidden hideError ]
                    [ strong [] [ text "Login Error!" ]
                    , text " "
                    , text model.error
                    ]
                , mainContent model
                ]
            , div [ class "footer" ]
                [ div [ style [ ( "text-align", "center" ) ] ]
                    [ small [ style [ ( "color", "#808080" ) ] ]
                        [ text "This work was partially funded within the "
                        , a [ href "https://www.indigo-datacloud.eu" ]
                            [ text "INDIGO DataCloud Project" ]
                        , text " (European Union’s Horizon 2020 research and innovation programme under grant agreement RIA 653549)"
                        ]
                    ]
                , div
                    [ style
                        [ ( "position", "absolute" )
                        , ( "top", "35px" )
                        , ( "right", "5px" )
                        ]
                    ]
                    [ small [ style [ ( "color", "#b3b3b3" ) ] ]
                        [ text model.serverVersion ]
                    ]
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
                    , secret_progressing = model.request_progressing
                    , progressing_title = model.progressing_title
                    , service = model.current_service
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
      , error = ""
      , displayName = "unknown"
      , current_service = Nothing
      , current_param = Nothing
      , request_progressing = False
      , progressing_title = Nothing
      }
    , retrieveInfo baseUrl restVersion
    )


retrieveInfo : String -> String -> Cmd Msg
retrieveInfo baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/info/"

        getInfo =
            Http.get apiUrl Info.decodeInfo
    in
        Http.send Messages.Info getInfo


retrieveProviderList : String -> String -> Cmd Msg
retrieveProviderList baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/oidcp/"

        getProviderList =
            Http.get apiUrl ProviderList.decodeProviderList
    in
        Http.send Messages.ProviderList getProviderList


retrieveServiceList : String -> String -> Cmd Msg
retrieveServiceList baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/service/"

        getServiceList =
            Http.get apiUrl ServiceList.decodeServiceList
    in
        Http.send Messages.ServiceList getServiceList


retrieveCredentialList : String -> String -> Cmd Msg
retrieveCredentialList baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/credential/"

        getCredentialList =
            Http.get apiUrl CredentialList.decodeCredentialList
    in
        Http.send Messages.CredentialList getCredentialList


retrieveAccessToken : String -> String -> Cmd Msg
retrieveAccessToken baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/access_token/"

        getAccessToken =
            Http.get apiUrl decodeAccessToken
    in
        Http.send Messages.AccessToken getAccessToken


logout : String -> String -> Cmd Msg
logout baseUrl restVersion =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/logout/"

        logout =
            Http.getString apiUrl
    in
        Http.send Messages.LoggedOut logout


request : String -> String -> String -> Maybe (Dict String Json.Value) -> Cmd Msg
request baseUrl restVersion serviceId dict =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/credential/"

        dataList =
            case dict of
                Nothing ->
                    Json.object
                        [ ( "service_id", Json.string serviceId ) ]

                Just paramsDict ->
                    Json.object
                        [ ( "service_id", Json.string serviceId )
                        , ( "params", Json.object (Dict.toList paramsDict) )
                        ]

        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = apiUrl
                , body = Http.jsonBody dataList
                , expect = Http.expectJson decodeSecret
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send Messages.Requested request


revoke : String -> String -> String -> Cmd Msg
revoke baseUrl restVersion credId =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/credential/" ++ credId

        revoke =
            Http.request
                { method = "DELETE"
                , headers = []
                , url = apiUrl
                , body = Http.emptyBody
                , expect = Http.expectJson decodeSecret
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send Messages.Revoked revoke
