module Service.Model exposing (..)


type alias Model =
    { id : String
    , description : String
    , enabled : Bool
    , host : String
    , port' : String
    , type' : String
    , credCount : Int
    , credLimit : Int
    , limitReached : Bool
    }



-- isEnabled : Model -> Bool
-- isEnabled model =
--     let
--         notLimit =
--             not model.limitReached
--         enabled =
--             model.enabled
--     in
--         enabled (&&) notLimit
