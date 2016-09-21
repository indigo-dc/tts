module Credential.View exposing (..)

import Credential.Model as Credential exposing (Model)
import Html exposing (Html, text, button, option, tr, td, form, input)
import Html.Attributes exposing (class, method, value, disabled, name, type', action)


view : Credential.Model -> Html msg
view service =
    tr []
        [ td [] [ text service.interface ]
        , td [] [ text service.ctime ]
        , td [] [ text service.serviceId ]
        , td []
            [ form [ method "post", action "/user" ]
                [ input [ type' "hidden", name "service_id", value service.id ] []
                , input [ type' "hidden", name "action", value "revoke" ] []
                , button
                    [ type' "submit", class "btn btn-default" ]
                    [ text "Revoke" ]
                ]
            ]
        ]
