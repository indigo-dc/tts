module Pages.Login.View exposing (..)

import Html exposing (Html, div, text, form, select, option, button, span)
import Html.Attributes exposing (disabled, action, class, method, id, name, value, type')
import ProviderList.Model as ProviderList exposing (Model)
import ProviderList.View as ProviderList exposing (view)


type alias ViewContext =
    { providerList : ProviderList.Model
    , path : String
    }


view : ViewContext -> Html msg
view context =
    div []
        [ text "Please select your OpenId Connect Provider"
        , div [ class "form-group" ]
            [ form [ method "post", action context.path ]
                [ div [ class "input-group" ]
                    [ select [ id "provider", name "provider", class "form-control" ]
                        (ProviderList.view context.providerList)
                    , span [ class "input-group-btn" ]
                        [ button
                            [ type' "submit"
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
