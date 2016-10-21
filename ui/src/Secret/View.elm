module Secret.View exposing (..)

import Dialog
import Html exposing (Html, h4, div, p, small, text, input, button, tr, td, form, table, textarea, tbody)
import Html.Attributes exposing (class, value, type', readonly, rows, cols)
import Html.Events exposing (onClick)
import Messages exposing (Msg)
import Secret.Model as Secret exposing (Model, Entry)


view : Maybe Secret.Model -> Html Msg
view scrt =
    let
        config =
            case scrt of
                Nothing ->
                    Nothing

                Just secret ->
                    let
                        cred =
                            secret.credential
                    in
                        Just
                            { closeMessage = Just Messages.HideSecret
                            , containerClass = Nothing
                            , dialogSize = Just Dialog.Large
                            , header =
                                Just
                                    (h4 [ class "modal-title" ]
                                        [ text "Your Credential"
                                        , small [] [ text ("  id: " ++ cred.id) ]
                                        ]
                                    )
                            , body =
                                Just
                                    (div []
                                        [ p [] [ text "This information will be gone once you perform any action, the credential is also not stored on the server" ]
                                        , table [ class "table table-striped" ]
                                            [ tbody []
                                                (List.map viewEntry cred.entries)
                                            ]
                                        ]
                                    )
                            , footer =
                                Just
                                    (button
                                        [ type' "button"
                                        , class "btn btn-default"
                                        , onClick Messages.HideSecret
                                        ]
                                        [ text "Close" ]
                                    )
                            }
    in
        Dialog.view config


viewEntry : Secret.Entry -> Html Msg
viewEntry entry =
    let
        column =
            td []
                [ if entry.type' == "text" then
                    input [ type' "text", value entry.value, class "form-control", readonly True ] []
                  else if entry.type' == "textfile" then
                    textarea [ class "form-control", readonly True, rows entry.rows, cols entry.cols ]
                        [ text entry.value
                        ]
                  else if entry.type' == "textarea" then
                    textarea [ class "form-control", readonly True ] [ text entry.value ]
                  else
                    input [ type' "text", value entry.value, class "form-control", readonly True ] []
                ]
    in
        tr []
            [ td [] [ text entry.name ], column ]
