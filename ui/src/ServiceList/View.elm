module ServiceList.View exposing (..)

import Html exposing (Html, div, h2, text, form, select, option, button, span, tr, th, table, thead, tbody)
import Html.Attributes exposing (class, method, value, disabled, name, type', action)
import Service.Model as Service exposing (Msg)
import Service.View as Service exposing (view)
import ServiceList.Model as ServiceList exposing (Model)


view : ServiceList.Model -> Html Service.Msg
view model =
    let
        append service list =
            [ Service.view service ] ++ list

        tableContent =
            (List.foldr (append) [] model.serviceList)

        empty =
            (==) 0 (List.length model.serviceList)
    in
        if empty then
            text "Sorry, no services available"
        else
            div [ class "panel panel-default" ]
                [ div [ class "panel-heading" ] [ h2 [] [ text "Services" ] ]
                , table [ class "table table-striped" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Id" ]
                            , th [] [ text "Type" ]
                            , th [] [ text "Host" ]
                            , th [] [ text "Description" ]
                            , th [] [ text "#Credentials" ]
                            , th [] [ text "Action" ]
                            ]
                        ]
                    , tbody [] tableContent
                    ]
                ]
