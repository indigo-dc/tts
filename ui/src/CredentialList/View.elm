module CredentialList.View exposing (..)

import Credential.View as Credential exposing (view)
import CredentialList.Model as CredentialList exposing (Model)
import Html exposing (Html, div, h2, text, form, select, option, button, span, tr, th, table, thead, tbody, p)
import Html.Attributes exposing (class, method, value, disabled, name, type', action)


view : CredentialList.Model -> Html msg
view model =
    let
        append service list =
            [ Credential.view service ] ++ list

        tableContent =
            (List.foldr (append) [] model.credentialList)

        empty =
            (==) 0 (List.length model.credentialList)
    in
        if empty then
            div [] []
        else
            div [ class "panel panel-default" ]
                [ div [ class "panel-heading" ] [ h2 [] [ text "Credentials" ] ]
                , div [ class "panel-body" ] [ p [] [ text "The credentials that have been created for you or on your behalf" ] ]
                , table [ class "table table-striped" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Interface" ]
                            , th [] [ text "Creation Time" ]
                            , th [] [ text "Service Id" ]
                            , th [] [ text "Action" ]
                            ]
                        ]
                    , tbody [] tableContent
                    ]
                ]
