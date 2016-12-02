module Service.View exposing (..)

import Dialog as Dialog exposing (view)
import Html exposing (Html, small, a, li, ul, h4, br, p, text, table, tbody, button, option, tr, td, form, input, div, textarea)
import Html.Attributes exposing (class, method, value, disabled, name, type_, action, placeholder)
import Html.Events exposing (onClick, onInput)
import Messages exposing (Msg)
import Service.Model as Service exposing (Model, Set, Param, hasBasic, hasAdvanced, nonEmptySets)


view : Service.Model -> Html Msg
view service =
    let
        credText =
            (toString service.credCount) ++ " / " ++ (toString service.credLimit)

        serviceDisabled =
            service.limitReached || (not service.enabled)

        requestDisabled =
            serviceDisabled || not (Service.hasBasic service)

        advancedDisabled =
            serviceDisabled || not (Service.hasAdvanced service)
    in
        tr []
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


advancedPagination : Int -> Service.Model -> Html Msg
advancedPagination pos service =
    let
        setCount =
            List.length (nonEmptySets service.parameter_sets)

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
                list ++ [ li attr [ a [] [ text (toString num) ] ] ]

        entries =
            List.foldl numberToEntry [] numbers

        attrLeft =
            if pos == 1 then
                [ class "disabled" ]
            else
                []

        left =
            [ li attrLeft [ a [] [ text "«" ] ] ]

        attrRight =
            if pos == setCount then
                [ class "disabled" ]
            else
                []

        right =
            [ li attrRight [ a [] [ text "»" ] ] ]

        pagination =
            if setCount == 1 then
                div [] []
            else
                ul [ class "pagination" ] (left ++ entries ++ right)
    in
        pagination


advancedHeader : Int -> Service.Model -> Html Msg
advancedHeader pos service =
    let
        setCount =
            List.length (nonEmptySets service.parameter_sets)

        headList =
            if setCount == 1 then
                [ text "Parameter" ]
            else
                [ text "Parameter"
                , br [] []
                , small []
                    [ text "this service has multiple parameter sets, ensure you use the right one"
                    ]
                ]

        headPagination =
            advancedPagination pos service
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
                            Just (advancedHeader 1 service)
                        , body =
                            Just
                                (viewParamSet 0 service.parameter_sets)
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


viewParamSet : Int -> List Service.Set -> Html Msg
viewParamSet pos allSets =
    let
        sets =
            Service.nonEmptySets allSets

        set =
            case List.head sets of
                Nothing ->
                    []

                Just s ->
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
