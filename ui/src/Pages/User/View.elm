module Pages.User.View exposing (..)

import AccessToken.Model as AccessToken exposing (Model)
import AccessToken.View as AccessToken exposing (view)
import CredentialList.Model as CredentialList exposing (Model)
import CredentialList.View as CredentialList exposing (view)
import Html exposing (Html, div, text, form, select, option, button, span, br, input)
import Html.Attributes exposing (disabled, action, class, method, id, name, value, type')
import Html.Events exposing (onClick)
import Messages exposing (Msg)
import ServiceList.Model as ServiceList exposing (Model)
import ServiceList.View as ServiceList exposing (view)


type alias ViewContext =
    { displayName : String
    , serviceList : ServiceList.Model
    , credentialList : CredentialList.Model
    , accessToken : AccessToken.Model
    }


view : ViewContext -> Html Msg
view context =
    div []
        [ text ("Hello " ++ context.displayName ++ ", welcome!")
        , br [] []
        , button
            [ id "ac_token"
            , type' "button"
            , class "btn btn-default"
            , onClick Messages.RetrieveAccessToken
            ]
            [ text "show access token" ]
        , AccessToken.view context.accessToken
        , br [] []
        , br [] []
        , ServiceList.view context.serviceList
        , CredentialList.view context.credentialList
        , button [ class "btn btn-primary", onClick Messages.Logout ] [ text "Logout" ]
        ]
