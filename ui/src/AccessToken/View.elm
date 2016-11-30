module AccessToken.View exposing (..)

import AccessToken.Model as AccessToken exposing (Model)
import Dialog
import Html exposing (Html, text, textarea)
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
                    , header = Just (text "Your Access Token")
                    , body =
                        Just
                            (textarea
                                [ class "form-control"
                                , rows 10
                                , readonly True
                                ]
                                [ text at.token ]
                            )
                    , footer = Nothing
                    }
    in
        Dialog.view (config)
