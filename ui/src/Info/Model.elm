module Info.Model exposing (..)

import Dict exposing (Dict, empty, insert)
import Json.Encode as Json exposing (Value)


type alias Model =
    { version : String
    , redirectPath : String
    , loggedIn : Bool
    , displayName : String
    , error : String
    , issuer_id : String
    , docs_enabled : Bool
    , service_request : Maybe ServiceRequest
    , rsp_success_redir : Maybe String
    , rsp_error_redir : Maybe String
    }


type alias ServiceRequest =
    { service_id : String
    , params : Dict String Json.Value
    }
