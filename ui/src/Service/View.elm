module Service.View exposing (..)

import Dialog as Dialog exposing (view)
import Html exposing (Html, small, a, li, ul, h4, br, p, text, table, tbody, button, option, tr, td, form, input, div, textarea, span)
import Html.Attributes exposing (class, title, method, value, disabled, name, type_, action, placeholder, style, hidden)
import Html.Events exposing (onClick, onInput)
import Messages exposing (Msg)
import Service.Model as Service exposing (Model, Set, Param, hasBasic, hasAdvanced)


view : Service.Model -> Html Msg
view service =
    let
        credText =
            case service.credLimit >= 0 of
                True ->
                    (toString service.credCount) ++ " / " ++ (toString service.credLimit)

                False ->
                    (toString service.credCount)

        serviceDisabled =
            service.limitReached || (not service.enabled) || (not service.authorized)

        requestDisabled =
            serviceDisabled || not (Service.hasBasic service)

        advancedDisabled =
            serviceDisabled || not (Service.hasAdvanced service)

        serviceTitle =
            if service.authorized && (not service.enabled) then
                [ title "Sorry, this service is disabled" ]
            else if service.authorized && service.limitReached then
                [ title "The credential limit for this service is reached" ]
            else if (not service.authorized) then
                [ title service.tooltip ]
            else
                []

        warning =
            case service.pass_access_token of
                True ->
                    [ span
                        [ class "glyphicon glyphicon glyphicon-warning-sign"
                        , title "this service receives your access token"
                        ]
                        []
                    ]

                False ->
                    []

        icons =
            warning

        rowattrs =
            [ disabled serviceDisabled ] ++ serviceTitle

        color =
            if serviceDisabled then
                [ ( "color", "#777" ) ]
            else
                []
    in
        tr rowattrs
            [ td [ style color ] [ text service.id ]
            , td [ style color ] [ text service.description ]
            , td [ style color ] [ text credText ]
            , td [ style color ] icons
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


advancedView : Maybe Service.Model -> List String -> Html Msg
advancedView srvc nonEmptyFields=
    let
        config =
            case srvc of
                Nothing ->
                    Nothing

                Just service ->
                    let
                        (valid, dialogBody) =
                                viewParamSet service nonEmptyFields
                    in
                    Just
                        { closeMessage = Just Messages.AdvancedCancel
                        , containerClass = Nothing
                        , dialogSize = Dialog.Large
                        , header =
                            Just (advancedHeader service)
                        , body =
                            Just dialogBody
                        , footer =
                            Just
                                (div [ class "btn-group" ]
                                    [ button
                                        [ class "btn btn-primary"
                                        , disabled (not valid)
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


viewParamSet : Service.Model -> List String -> (Bool, Html Msg)
viewParamSet model nonEmptyFields =
    let
        sets =
            model.non_empty_sets

        pos =
            model.current_set

        set =
            case List.head (List.drop pos sets) of
                Nothing ->
                    []
                Just s ->
                    s
        doViewParam set (valid, htmlList) =
            let
                (v, html) = viewParam set nonEmptyFields
            in
                (valid && v, List.append htmlList [html])

        (isValid, tableBody) =
                    List.foldl doViewParam (True, []) set
    in
        (isValid, div []
             [ p [] [ text "please fill in at least all mandatory parameter" ]
             , table [ class "table table-striped" ]
                 [ tbody []
                       tableBody
                 ]
             ])


viewParam : Service.Param -> List String -> (Bool, Html Msg)
viewParam param nonEmptyFields=
    let
        (name, isValid)  =
            if param.mandatory then
                (param.name ++ " (mandatory)"
                ,List.member param.key nonEmptyFields)

            else
                (param.name ++ " (optional)", True)

        classText = "form-control"

        column =
            td []
                [ if param.type_ == "text" then
                    input [ type_ "text", class classText ] []
                  else if param.type_ == "textarea" then
                    textarea [ class "form-control"
                             , placeholder param.description
                             , onInput (Messages.AdvancedChange param.key) ] []
                  else
                    input [ type_ "text"
                          , placeholder param.description
                          , class "form-control"
                          , onInput (Messages.AdvancedChange param.key) ] []
                ]
    in
        (isValid,
             tr []
             [ td [] [ text name ], column ])
