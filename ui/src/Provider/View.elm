module Provider.View exposing (..)

import Html exposing (Html, text, option)
import Html.Attributes exposing (class, value, disabled)
import Provider.Model as Provider exposing (Model)


view : Provider.Model -> Html msg
view provider =
    option attributes provider [ text provider.description ]


attributes provider =
    if provider.ready then
        [ value provider.id ]
    else
        [ disabled ]
