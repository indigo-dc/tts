module Pages.Login.View exposing (..)

import Html exposing (Html, div, text, form, select, option, button, span)
import Html.Attributes exposing (action, class, method, id, name, value, type')
import Provider.Model as Provider exposing (Model)


type alias ViewContext =
    { provider : List Provider.Model
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
                        [ option [ value "id1" ] [ text "description one" ]
                        , option [ value "id2" ] [ text "description two" ]
                        ]
                    , span [ class "input-group-btn" ]
                        [ button [ type' "submit", class "btn btn-primary" ]
                            [ text "Login"
                            ]
                        ]
                    ]
                ]
            ]
        ]
