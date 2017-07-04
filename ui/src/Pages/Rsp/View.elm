module Pages.Rsp.View exposing (..)

import Html exposing (Html, div)
import Messages exposing (Msg)
import Secret.Model as Secret exposing (Model)
import Secret.View as Secret exposing (view_and_redirect)


type alias ViewContext =
    { secret : Maybe Secret.Model
    , progressing_title : Maybe String
    , success_url : Maybe String
    , error_url : Maybe String
    }


view : ViewContext -> Html Msg
view context =
    div []
        [ Secret.view_and_redirect context.success_url context.error_url context.progressing_title context.secret
        ]
