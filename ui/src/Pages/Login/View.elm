module Pages.Login.View exposing (..)

import Html exposing (Html, div, text, form, select, option, button, span)
import Html.Attributes exposing (hidden, disabled, action, class, method, id, name, value, type_)
import ProviderList.Model as ProviderList exposing (Model)
import ProviderList.View as ProviderList exposing (view)


type alias ViewContext =
    { providerList : ProviderList.Model
    , path : String
    }


view : ViewContext -> Html msg
view context =
    let
        showLogin =
            (List.length context.providerList.provider) > 1

        autoLogin =
            (List.length context.providerList.provider) == 1

        loginImpossible =
            (not showLogin) && (not autoLogin)
    in
        div []
            [ div [ hidden (not showLogin) ]
                [ text "Please select your OpenId Connect Provider"
                , div [ class "form-group" ]
                    [ form [ method "post", action context.path ]
                        [ div [ class "input-group" ]
                            [ select [ id "provider", name "provider", class "form-control" ]
                                (ProviderList.view context.providerList)
                            , span [ class "input-group-btn" ]
                                [ button
                                    [ type_ "submit"
                                    , class "btn btn-primary"
                                    , disabled (not (ProviderList.hasReadyEntries context.providerList))
                                    ]
                                    [ text "Login"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ hidden (not autoLogin) ]
                [ text "You are getting redirected for login, this might take some time" ]
            , div [ hidden (not loginImpossible) ]
                [ text "Login is disabled as no OpenId Connect provider is configured" ]
            ]
