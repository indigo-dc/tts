module Credential.View exposing (..)

import Credential.Model as Credential exposing (Model)
import Html exposing (Html, text, button, option, tr, td, form, input)
import Html.Attributes exposing (class, method, value, disabled, name, action)
import Html.Events exposing (onClick)
import Messages exposing (Msg)


view : Credential.Model -> Html Msg
view credential =
    tr []
        [ td [] [ text credential.interface ]
        , td [] [ text credential.ctime ]
        , td [] [ text credential.serviceId ]
        , td []
            [ button
                [ class "btn btn-default", onClick (Messages.Revoke credential.id) ]
                [ text "Remove" ]
            ]
        ]
