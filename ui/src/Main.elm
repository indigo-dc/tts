module Main exposing (..)

-- import Json.Decode as Decoder

import AccessToken.Decoder exposing (decodeAccessToken)
import AccessToken.Model as AccessToken exposing (Model)
import CredentialList.Decoder as CredentialList exposing (decodeCredentialList)
import CredentialList.Model as CredentialList exposing (Model, initModel)
import Dict exposing (Dict, empty, insert)
import Html exposing (Html, a, div, h1, text, small, strong)
import Html.Attributes exposing (class, hidden, style, href)
import Http exposing (get, post, send, Error)
import Info.Decoder as Info exposing (decodeInfo)
import Json.Decode exposing (decodeString)
import Json.Encode as Json exposing (encode)
import Messages exposing (Msg)
import Navigation exposing (load)
import Pages.Login.View as Login exposing (view)
import Pages.Rsp.View as Rsp exposing (view)
import Pages.User.View as User exposing (view)
import ProviderList.Decoder as ProviderList exposing (decodeProviderList)
import ProviderList.Model as ProviderList exposing (Model, initModel)
import Secret.Decoder exposing (decodeSecret)
import Secret.Model as Secret exposing (Model, empty_credential)
import Service.Model as Service exposing (Model)
import ServiceList.Decoder as ServiceList exposing (decodeServiceList)
import ServiceList.Model as ServiceList exposing (Model, initModel, update_services)
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
    | Rsp
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
    , issuer_id : String
    , loggedIn : Bool
    , error : String
    , displayName : String
    , current_service : Maybe Service.Model
    , current_param : Maybe (Dict String Json.Value)
    , non_empty_params : List String
    , current_serviceid : Maybe String
    , request_progressing : Bool
    , user_docs_enabled : Bool
    , code_docs_enabled : Bool
    , progressing_title : Maybe String
    , rsp_redir_success : Maybe String
    , rsp_redir_error : Maybe String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Messages.Info (Ok info) ->
            let
                ( nextPage, nextCmd ) =
                    case ( info.loggedIn, info.rsp_success_redir ) of
                        ( True, Nothing ) ->
                            ( User, retrieveServiceList model.url model.restVersion info.issuer_id )

                        ( True, _ ) ->
                            ( Rsp, retrieveCredentialList model.url model.restVersion info.issuer_id )

                        ( False, _ ) ->
                            ( Login, retrieveProviderList model.url model.restVersion )

                ( curService, curServiceParam ) =
                    case info.service_request of
                        Nothing ->
                            ( Nothing, Nothing )

                        Just request ->
                            ( Just request.service_id, Just request.params )
            in
                ( { model
                    | serverVersion = info.version
                    , redirectPath = info.redirectPath
                    , loggedIn = info.loggedIn
                    , error = info.error
                    , displayName = info.displayName
                    , activePage = nextPage
                    , issuer_id = info.issuer_id
                    , user_docs_enabled = info.doc_user_enabled
                    , code_docs_enabled = info.doc_code_enabled
                    , current_serviceid = curService
                    , current_param = curServiceParam
                    , rsp_redir_success = info.rsp_success_redir
                    , rsp_redir_error = info.rsp_error_redir
                  }
                , nextCmd
                )

        Messages.Info (Err info) ->
                ( model, Cmd.none )

        Messages.ProviderList (Ok providerlist) ->
            let
                maybeProvider = List.head providerlist.provider

                cmd = case (List.length providerlist.provider, maybeProvider) of
                    (1, Just provider)  ->
                        Navigation.load (model.redirectPath ++ "?provider=" ++ provider.id)
                    (_, _) ->
                        Cmd.none
            in
                ( { model
                      | providerList = ProviderList.sort providerlist
                  }, cmd )

        Messages.ProviderList (Err info) ->
            ( model, Cmd.none )

        Messages.ServiceList (Ok servicelist) ->
            let
                newServiceList =
                    ServiceList.update_services servicelist
            in
                ( { model
                    | serviceList = newServiceList
                  }
                , retrieveCredentialList model.url model.restVersion model.issuer_id
                )

        Messages.ServiceList (Err info) ->
            ( model, Cmd.none )

        Messages.CredentialList (Ok credentiallist) ->
            let
                ( isProgressing, progTitle, nextCmd ) =
                    case model.current_serviceid of
                        Nothing ->
                            ( False, Nothing, Cmd.none )

                        Just id ->
                            ( True
                            , Just "Requesting Credential ..."
                            , request model.url model.restVersion model.issuer_id id model.current_param
                            )
            in
                ( { model
                    | credentialList = credentiallist
                    , request_progressing = isProgressing
                    , progressing_title = progTitle
                    , current_serviceid = Nothing
                    , current_param = Nothing
                    , non_empty_params = []
                  }
                , nextCmd
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
                , non_empty_params = []
                , request_progressing = True
                , progressing_title = Just "Requesting Credential ..."
              }
            , request model.url model.restVersion model.issuer_id serviceId model.current_param
            )

        Messages.Requested (Ok credential) ->
            ( { model
                | credential = Just credential
                , request_progressing = False
                , progressing_title = Nothing
              }
            , retrieveServiceList model.url model.restVersion model.issuer_id
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
                                            Just (Secret.error_credential err_msg)
                            in
                                ( { model
                                    | credential = error_cred
                                    , request_progressing = False
                                    , progressing_title = Nothing
                                  }
                                , retrieveServiceList model.url model.restVersion model.issuer_id
                                )
            in
                result

        Messages.Requested (Err info) ->
            let
                err_msg =
                    "an internal error occured, see the console for more details"

                error_cred =
                    Just (Secret.error_credential err_msg)
            in
                ( { model
                    | credential = error_cred
                    , request_progressing = False
                    , progressing_title = Nothing
                  }
                , retrieveServiceList model.url model.restVersion model.issuer_id
                )

        Messages.Revoke credId ->
            ( { model
                | request_progressing = True
                , progressing_title = Just "Removing Credential ..."
              }
            , revoke model.url model.restVersion model.issuer_id credId
            )

        Messages.Revoked (Ok _) ->
            ( { model
                | request_progressing = False
                , progressing_title = Nothing
              }
            , retrieveServiceList model.url model.restVersion model.issuer_id
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
                                            Just (Secret.error_credential err_msg)
                            in
                                ( { model
                                    | credential = error_cred
                                    , request_progressing = False
                                    , progressing_title = Nothing
                                  }
                                , retrieveServiceList model.url model.restVersion model.issuer_id
                                )
            in
                result

        Messages.Revoked (Err info) ->
            let
                err_msg =
                    "an internal error occured, see the console for more details"

                error_cred =
                    Just (Secret.error_credential err_msg)
            in
                ( { model
                    | credential = error_cred
                    , request_progressing = False
                    , progressing_title = Nothing
                  }
                , retrieveServiceList model.url model.restVersion model.issuer_id
                )

        Messages.AdvancedRequest service ->
            ( { model
                | current_service = Just service
              }
            , Cmd.none
            )

        Messages.AdvancedChange key value ->
            let
                jsonValue =
                    Json.string value

                filter a =
                    a /= key

                oldNonEmpty =
                    List.filter filter model.non_empty_params

                nonEmptyParams =
                    case (String.length value) > 0  of
                        True ->
                           List.append [key] oldNonEmpty

                        False ->
                            oldNonEmpty

                oldParams =
                    case model.current_param of
                        Just params ->
                            Dict.remove key params

                        Nothing ->
                            Dict.empty

                newParams =
                    case (String.length value) > 0 of
                        True ->
                            Dict.insert key jsonValue oldParams

                        False ->
                            oldParams

            in
                ( { model
                    | current_param = Just newParams
                    , non_empty_params = nonEmptyParams
                  }
                , Cmd.none
                )

        Messages.AdvancedSet pos ->
            let
                newService =
                    case model.current_service of
                        Nothing ->
                            Nothing

                        Just service ->
                            Just (Service.setPos pos service)
            in
                ( { model
                    | current_service = newService
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
            initModel model.url model.restVersion

        Messages.HideAccessToken ->
            ( { model | accessToken = AccessToken.initModel }, Cmd.none )

        Messages.HideSecret ->
            ( { model | credential = Nothing }, Cmd.none )

        Messages.LoadUrl url ->
            ( { model | credential = Nothing }, Navigation.load url )


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

        user_docu =
            case model.user_docs_enabled of
                True ->
                    [ small [ style [ ( "color", "#808080" ), ( "margin-left", "20px" ) ] ]
                        [ a [ href "docs/user/index.html" ] [ text "User Documentation" ]
                        ]
                    ]

                False ->
                    []
        code_docu =
            case model.code_docs_enabled of
                True ->
                    [ small [ style [ ( "color", "#808080" ), ( "margin-left", "20px" ) ] ]
                        [ a [ href "docs/code/index.html" ] [ text "Code Documentation" ]
                        ]
                    ]

                False ->
                    []
    in
        div []
            [ div [ class "container" ]
                [ h1 []
                    [ text "WaTTS  "
                    , small [] [ text "- The INDIGO Token Translation Service" ]
                    ]
                , div [ class "alert alert-danger", hidden hideError ]
                    [ strong [] [ text "Login Error!" ]
                    , text " "
                    , text model.error
                    ]
                , mainContent model
                ]
            , div [ class "footer" ]
                [ div [ style [ ( "text-align", "center" ) ] ]
                    ([ small [ style [ ( "color", "#808080" ) ] ]
                        [ text "This work was partially funded by the "
                        , a [ href "https://www.indigo-datacloud.eu" ]
                            [ text "INDIGO DataCloud Project" ]
                        , text " under grant agreement RIA 653549"
                        ]
                     , small [ style [ ( "color", "#808080" ), ( "margin-left", "20px" ) ] ]
                        [ a [ href "privacystatement.html" ] [ text "Privacy Statement" ]
                        ]
                     ]
                        ++ user_docu
                        ++ code_docu
                        ++ [ small [ style [ ( "color", "#808080" ), ( "margin-left", "20px" ) ] ]
                                [ a [ href "https://github.com/indigo-dc/tts" ] [ text "GitHub" ]
                                ]
                           ]
                    )
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
                    , non_empty_fields = model.non_empty_params
                    }
            in
                User.view context

        Rsp ->
            let
                context =
                    { secret = model.credential
                    , progressing_title = model.progressing_title
                    , success_url = model.rsp_redir_success
                    , error_url = model.rsp_redir_error
                    }
            in
                Rsp.view context

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
      , issuer_id = "unknown"
      , current_service = Nothing
      , current_serviceid = Nothing
      , current_param = Nothing
      , non_empty_params = []
      , request_progressing = False
      , user_docs_enabled = False
      , code_docs_enabled = False
      , progressing_title = Nothing
      , rsp_redir_success = Nothing
      , rsp_redir_error = Nothing
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


retrieveServiceList : String -> String -> String -> Cmd Msg
retrieveServiceList baseUrl restVersion issuer_id =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/" ++ issuer_id ++ "/service/"

        getServiceList =
            Http.get apiUrl ServiceList.decodeServiceList
    in
        Http.send Messages.ServiceList getServiceList


retrieveCredentialList : String -> String -> String -> Cmd Msg
retrieveCredentialList baseUrl restVersion issuer_id =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/" ++ issuer_id ++ "/credential/"

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


request : String -> String -> String -> String -> Maybe (Dict String Json.Value) -> Cmd Msg
request baseUrl restVersion issuer_id serviceId dict =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/" ++ issuer_id ++ "/credential/"

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


revoke : String -> String -> String -> String -> Cmd Msg
revoke baseUrl restVersion issuer_id credId =
    let
        apiUrl =
            baseUrl ++ "/api/" ++ restVersion ++ "/" ++ issuer_id ++ "/credential/" ++ credId

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
