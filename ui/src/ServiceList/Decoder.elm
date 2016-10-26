module ServiceList.Decoder exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Service.Decoder as Service exposing (decodeService)
import ServiceList.Model as ServiceList exposing (Model)


decodeServiceList : Json.Decode.Decoder ServiceList.Model
decodeServiceList =
    Json.Decode.Pipeline.decode ServiceList.Model
        |> Json.Decode.Pipeline.required "service_list" (Json.Decode.list Service.decodeService)
