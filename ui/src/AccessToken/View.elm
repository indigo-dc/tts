module AccessToken.View exposing (..)

import AccessToken.Model as AccessToken exposing (Model)
import Dialog
import Html exposing (Html, text, textarea, table, tbody, tr, td)
import Html.Attributes exposing (class, rows, readonly)
import Messages exposing (Msg)


view : AccessToken.Model -> Html Msg
view at =
    let
        config =
            if at.token == "" then
                Nothing
            else
                Just
                    { closeMessage = Just Messages.HideAccessToken
                    , containerClass = Nothing
                    , dialogSize = Dialog.Large
                    , header = Just (text "Information about You")
                    , body =
                        Just
                            (table [ class "table table-striped" ]
                                [ tbody []
                                    [ tr []
                                        [ td [] [ text "Issuer" ]
                                        , td [] [ text at.issuer ]
                                        ]
                                    , tr []
                                        [ td [] [ text "Subject" ]
                                        , td [] [ text at.subject ]
                                        ]
                                    , tr []
                                        [ td [] [ text "Access Token" ]
                                        , td []
                                            [ textarea
                                                [ class "form-control"
                                                , rows 10
                                                , readonly True
                                                ]
                                                [ text at.token ]
                                            ]
                                        ]
                                    , tr []
                                        [ td [] [ text "WaTTS Issuer Id" ]
                                        , td [] [ text at.issuer_id ]
                                        ]
                                    ]
                                ]
                            )
                    , footer = Nothing
                    }
    in
        Dialog.view (config)
