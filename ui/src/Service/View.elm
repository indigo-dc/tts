module Service.View exposing (..)

import Dialog as Dialog exposing (view)
import Html exposing (Html, h4, p, text, table, tbody, button, option, tr, td, form, input, div, textarea)
import Html.Attributes exposing (class, method, value, disabled, name, type', action, placeholder)
import Html.Events exposing (onClick, onInput)
import Messages exposing (Msg)
import Service.Model as Service exposing (Model, Param, onlyAdvanced)


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
                        , onClick (Messages.AdvancedRequest service)
                        ]
                        [ text "Advanced" ]
                    ]
                ]
            ]


advancedView : Maybe Service.Model -> Html Msg
advancedView srvc =
    let
        config =
            case srvc of
                Nothing ->
                    Nothing

                Just service ->
                    Just
                        { closeMessage = Just Messages.AdvancedCancel
                        , containerClass = Nothing
                        , dialogSize = Just Dialog.Large
                        , header =
                            Just
                                (h4 [ class "modal-title" ] [ text "Parameter" ])
                        , body =
                            Just
                                (div []
                                    [ p [] [ text "please fill in at least all mandatory parameter" ]
                                    , table [ class "table table-striped" ]
                                        [ tbody []
                                            (List.map viewParam service.parameter)
                                        ]
                                    ]
                                )
                        , footer =
                            Just
                                (div [ class "btn-group" ]
                                    [ button
                                        [ class "btn btn-primary"
                                        , onClick (Messages.Request service.id)
                                        ]
                                        [ text "Submit" ]
                                    , button
                                        [ class "btn btn-default"
                                        , onClick Messages.AdvancedCancel
                                        ]
                                        [ text "Cancel" ]
                                    ]
                                )
                        }
    in
        Dialog.view config


viewParam : Service.Param -> Html Msg
viewParam param =
    let
        name =
            if param.mandatory then
                param.name ++ " (mandatory)"
            else
                param.name ++ " (optional)"

        column =
            td []
                [ if param.type' == "text" then
                    input [ type' "text", class "form-control" ] []
                  else if param.type' == "textarea" then
                    textarea [ class "form-control", placeholder param.description, onInput (Messages.AdvancedChange param.name) ] []
                  else
                    input [ type' "text", placeholder param.description, class "form-control" ] []
                ]
    in
        tr []
            [ td [] [ text name ], column ]
