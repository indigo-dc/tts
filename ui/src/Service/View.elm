module Service.View exposing (..)

import Dialog as Dialog exposing (view)
import Html exposing (Html, small, a, li, ul, h4, br, p, text, table, tbody, button, option, tr, td, form, input, div, textarea)
import Html.Attributes exposing (class, title, method, value, disabled, name, type_, action, placeholder)
import Html.Events exposing (onClick, onInput)
import Messages exposing (Msg)
import Service.Model as Service exposing (Model, Set, Param, hasBasic, hasAdvanced)


view : Service.Model -> Html Msg
view service =
    let
        credText =
            (toString service.credCount) ++ " / " ++ (toString service.credLimit)

        serviceDisabled =
            service.limitReached || (not service.enabled) || (not service.authorized)

        requestDisabled =
            serviceDisabled || not (Service.hasBasic service)

        advancedDisabled =
            serviceDisabled || not (Service.hasAdvanced service)

        rowattrs =
            if service.authorized then
                [ disabled serviceDisabled
                , title "this service is disabled"
                ]
            else
                [ disabled serviceDisabled
                , title service.tooltip
                ]
    in
        tr rowattrs
            [ td [] [ text service.id ]
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


advancedPagination : Service.Model -> Html Msg
advancedPagination service =
    let
        pos =
            service.current_set + 1

        setCount =
            List.length service.non_empty_sets

        numbers =
            List.range 1 setCount

        numberToEntry num list =
            let
                attr =
                    if num == pos then
                        [ class "active" ]
                    else
                        []
            in
                list
                    ++ [ li attr
                            [ a [ onClick (Messages.AdvancedSet (num - 1)) ]
                                [ text (toString num) ]
                            ]
                       ]

        entries =
            List.foldl numberToEntry [] numbers

        attrLeft =
            if pos == 1 then
                [ class "disabled" ]
            else
                []

        left =
            [ li attrLeft [ a [ onClick (Messages.AdvancedSet (pos - 2)) ] [ text "«" ] ] ]

        attrRight =
            if pos == setCount then
                [ class "disabled" ]
            else
                []

        right =
            [ li attrRight [ a [ onClick (Messages.AdvancedSet pos) ] [ text "»" ] ] ]

        pagination =
            if setCount < 2 then
                div [] []
            else
                ul [ class "pagination" ] (left ++ entries ++ right)
    in
        pagination


advancedHeader : Service.Model -> Html Msg
advancedHeader service =
    let
        setCount =
            List.length service.non_empty_sets

        headList =
            if setCount >= 2 then
                [ text "Parameter"
                , br [] []
                , small []
                    [ text "this service has multiple parameter sets, ensure you use the right one"
                    ]
                ]
            else
                [ text "Parameter" ]

        headPagination =
            advancedPagination service
    in
        div []
            [ h4 [ class "modal-title" ] headList
            , headPagination
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
                        , dialogSize = Dialog.Large
                        , header =
                            Just (advancedHeader service)
                        , body =
                            Just
                                (viewParamSet service)
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


viewParamSet : Service.Model -> Html Msg
viewParamSet model =
    let
        sets =
            model.non_empty_sets

        pos =
            model.current_set

        setFilter a ( num, entry ) =
            case num == pos of
                True ->
                    ( num + 1, Just a )

                False ->
                    ( num + 1, entry )

        set =
            case List.foldl setFilter ( 0, Nothing ) sets of
                ( _, Nothing ) ->
                    []

                ( _, Just s ) ->
                    s
    in
        div []
            [ p [] [ text "please fill in at least all mandatory parameter" ]
            , table [ class "table table-striped" ]
                [ tbody []
                    (List.map viewParam set)
                ]
            ]


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
                [ if param.type_ == "text" then
                    input [ type_ "text", class "form-control" ] []
                  else if param.type_ == "textarea" then
                    textarea [ class "form-control", placeholder param.description, onInput (Messages.AdvancedChange param.name) ] []
                  else
                    input [ type_ "text", placeholder param.description, class "form-control" ] []
                ]
    in
        tr []
            [ td [] [ text name ], column ]
