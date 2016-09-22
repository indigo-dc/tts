module Service.View exposing (..)

import Html exposing (Html, text, button, option, tr, td, form, input, div)
import Html.Attributes exposing (class, method, value, disabled, name, type', action)
import Html.Events exposing (onClick)
import Messages exposing (Msg)
import Service.Model as Service exposing (Model, onlyAdvanced)


view : Service.Model -> Html Msg
view service =
    let
        credText =
            (toString service.credCount) ++ " / " ++ (toString service.credLimit)

        serviceDisabled =
            service.limitReached || (not service.enabled)

        requestDisabled =
            serviceDisabled || Service.onlyAdvanced service

        advancedDisabled =
            serviceDisabled || ((List.length service.parameter) == 0)
    in
        tr []
            [ td [] [ text service.id ]
            , td [] [ text service.type' ]
            , td [] [ text service.host ]
            , td [] [ text service.description ]
            , td [] [ text credText ]
            , td []
                [ div [ class "btn-group" ]
                    [ button
                        [ class "btn btn-default"
                        , disabled requestDisabled
                        , onClick (Messages.Request service.id)
                        ]
                        [ text "Request" ]
                    , button
                        [ class "btn btn-default"
                        , disabled advancedDisabled
                        , onClick (Messages.AdvancedRequest service.id)
                        ]
                        [ text "Advanced" ]
                    ]
                ]
            ]
