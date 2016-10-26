module ProviderList.View exposing (..)

import Html exposing (Html, div, text, form, select, option, button, span)
import Provider.View as Provider exposing (view)
import ProviderList.Model as ProviderList exposing (Model)


view : ProviderList.Model -> List (Html msg)
view model =
    let
        append provider list =
            [ Provider.view provider ] ++ list
    in
        List.foldr (append) [] model.provider
