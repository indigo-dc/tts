module Secret.View exposing (..)

import Dialog
import Html exposing (Html, h4, div, p, small, text, input, button, tr, td, form, table, textarea, tbody)
import Html.Attributes exposing (class, value, type', readonly, rows, cols, style, hidden)
import Html.Events exposing (onClick)
import Messages exposing (Msg)
import Secret.Model as Secret exposing (Model, Entry)


view : Bool -> Maybe Secret.Model -> Html Msg
view progressing scrt =
    let
        config =
            case scrt of
                Nothing ->
                    case progressing of
                        False ->
                            Nothing

                        True ->
                            Just
                                { closeMessage = Nothing
                                , containerClass = Nothing
                                , dialogSize = Just Dialog.Large
                                , header =
                                    Just (h4 [ class "modal-title" ] [ text "Requesting Credential" ])
                                , body =
                                    Just
                                        (div [ class "cssload-container" ]
                                            [ div [ class "cssload-lt" ] []
                                            , div [ class "cssload-rt" ] []
                                            , div [ class "cssload-lb" ] []
                                            , div [ class "cssload-rb" ] []
                                            ]
                                        )
                                , footer = Nothing
                                }

                Just secret ->
                    let
                        isCredential =
                            (secret.result == "ok")

                        cred =
                            secret.credential

                        error =
                            secret.error

                        title =
                            case isCredential of
                                True ->
                                    "Your Credential"

                                False ->
                                    "An Error Occured"

                        footer =
                            case isCredential of
                                True ->
                                    "  id: " ++ cred.id

                                False ->
                                    ""
                    in
                        Just
                            { closeMessage = Just Messages.HideSecret
                            , containerClass = Nothing
                            , dialogSize = Just Dialog.Large
                            , header =
                                Just (h4 [ class "modal-title" ] [ text title ])
                            , body =
                                Just
                                    (div []
                                        [ div [ hidden (not isCredential) ]
                                            [ p [] [ text "This information will be gone once you perform any action, the credential is also not stored on the server" ]
                                            , table [ class "table table-striped" ]
                                                [ tbody []
                                                    (List.map viewEntry cred.entries)
                                                ]
                                            ]
                                        , div [ hidden isCredential ]
                                            [ p [] [ text error ] ]
                                        ]
                                    )
                            , footer =
                                Just
                                    (div []
                                        [ div [ style [ ( "float", "left" ), ( "color", "#737373" ) ] ]
                                            [ small [] [ text footer ] ]
                                        , div [ style [ ( "float", "right" ) ] ]
                                            [ button
                                                [ type' "button"
                                                , class "btn btn-default"
                                                , onClick Messages.HideSecret
                                                ]
                                                [ text "Close" ]
                                            ]
                                        ]
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
