module Secret.Model exposing (..)


type alias Model =
    { result : String
    , credential : Credential
    , oidc_login : OidcLogin
    , error : String
    }


type alias Credential =
    { id : String
    , entries : List Entry
    }


type alias OidcLogin =
    { provider : String
    , url : String
    , msg : String
    }


error_credential : String -> Model
error_credential err_msg =
    { result = "error"
    , credential = empty_credential
    , oidc_login = empty_login
    , error = err_msg
    }


empty_credential : Credential
empty_credential =
    { id = "", entries = [] }


empty_login : OidcLogin
empty_login =
    { provider = "", url = "", msg = "" }


type alias Entry =
    { name : String
    , type_ : String
    , value : String
    , rows : Int
    , cols : Int
    , saveas : Maybe String
    }
