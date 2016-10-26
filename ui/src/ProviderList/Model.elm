module ProviderList.Model exposing (..)

import Provider.Model as Provider exposing (Model, isReady)


type alias Model =
    { provider : List Provider.Model }


initModel : Model
initModel =
    { provider = []
    }


hasReadyEntries : Model -> Bool
hasReadyEntries model =
    let
        oneReady provider bool =
            case Provider.isReady provider of
                True ->
                    True

                False ->
                    bool
    in
        List.foldl (oneReady) False model.provider



-- initModel =
--     { provider =
--         [ { id = "123", description = "Google", ready = True }
--         , { id = "789", description = "not reachable", ready = False }
--         , { id = "456", description = "Iam", ready = True }
--         ]
--     }
