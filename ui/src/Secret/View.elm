module Secret.View exposing (..)

import Dialog
import Html exposing (Html, span, a, br, h4, div, p, small, text, input, button, tr, td, form, table, textarea, tbody)
import Html.Attributes exposing (title, href, downloadAs, class, value, type_, readonly, rows, cols, style, hidden, action, method, name)
import Html.Events exposing (onClick)
import Http exposing (encodeUri)
import Messages exposing (Msg)
import Secret.Model as Secret exposing (Model, Entry)


view : Maybe String -> Maybe Secret.Model -> Html Msg
view progress_title scrt =
    let
        config =
            case ( scrt, progress_title ) of
                ( Nothing, Nothing ) ->
                    Nothing

                ( Nothing, Just title ) ->
                    Just
                        { closeMessage = Nothing
                        , containerClass = Nothing
                        , dialogSize = Dialog.Large
                        , header =
                            Just (h4 [ class "modal-title" ] [ text title ])
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

                ( Just secret, _ ) ->
                    let
                        isCredential =
                            (secret.result == "ok")

                        isOidcRedirect =
                            (secret.result == "oidc_login")

                        isError =
                            (not isCredential) && (not isOidcRedirect)

                        cred =
                            secret.credential

                        error =
                            secret.error

                        provider =
                            secret.oidc_login.provider

                        oidc_url =
                            secret.oidc_login.url

                        title =
                            case ( isCredential, isOidcRedirect ) of
                                ( True, _ ) ->
                                    "Your Credential"

                                ( _, True ) ->
                                    "A login is required"

                                _ ->
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
                            , dialogSize = Dialog.Large
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
                                        , div [ hidden (not isOidcRedirect) ]
                                            [ p [] [ text secret.oidc_login.msg ]
                                            ]
                                        , div [ hidden (not isError) ]
                                            [ p [] [ text error ]
                                            ]
                                        ]
                                    )
                            , footer =
                                Just
                                    (div []
                                        [ div [ style [ ( "float", "left" ), ( "color", "#737373" ) ] ]
                                            [ small [] [ text footer ] ]
                                        , div [ style [ ( "float", "right" ) ], hidden isOidcRedirect ]
                                            [ button
                                                [ type_ "button"
                                                , class "btn btn-default"
                                                , onClick Messages.HideSecret
                                                ]
                                                [ text "Close" ]
                                            ]
                                        , div
                                            [ style [ ( "float", "right" ) ]
                                            , hidden (not isOidcRedirect)
                                            ]
                                            [ form [ method "post", action oidc_url ]
                                                [ input
                                                    [ type_ "hidden"
                                                    , name "provider"
                                                    , value provider
                                                    ]
                                                    []
                                                , button
                                                    [ type_ "submit"
                                                    , class "btn btn-primary"
                                                    ]
                                                    [ text "Okay" ]
                                                , button
                                                    [ type_ "button"
                                                    , class "btn btn-default"
                                                    , onClick Messages.HideSecret
                                                    ]
                                                    [ text "Cancel" ]
                                                ]
                                            ]
                                        ]
                                    )
                            }
    in
        Dialog.view config


viewEntry : Secret.Entry -> Html Msg
viewEntry entry =
    let
        name_col =
            case entry.saveas of
                Nothing ->
                    text entry.name

                Just filename ->
                    div []
                        [ text entry.name
                        , a
                            [ class "btn btn-default pull-right"
                            , downloadAs (filename)
                            , href ("data:text/plain;charset=utf8," ++ (encodeUri entry.value))
                            , title "Download as File"
                            ]
                            [ span [ class "glyphicon glyphicon glyphicon-download-alt" ] [] ]
                        ]

        value_col =
            if entry.type_ == "text" then
                input [ type_ "text", value entry.value, class "form-control", readonly True ] []
            else if entry.type_ == "textfile" then
                textarea [ class "form-control", readonly True, rows entry.rows, cols entry.cols ]
                    [ text entry.value ]
            else if entry.type_ == "textarea" then
                textarea [ class "form-control", readonly True ] [ text entry.value ]
            else
                input [ type_ "text", value entry.value, class "form-control", readonly True ] []
    in
        tr []
            [ td [] [ name_col ], td [] [ value_col ] ]
