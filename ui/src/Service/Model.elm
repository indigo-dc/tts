module Service.Model exposing (..)


type alias Model =
    { id : String
    , description : String
    , enabled : Bool
    , credCount : Int
    , credLimit : Int
    , limitReached : Bool
    , parameter_sets : List Set
    }


type alias Set =
    List Param


type alias Param =
    { name : String
    , description : String
    , type' : String
    , mandatory : Bool
    }


hasBasic : Model -> Bool
hasBasic service =
    basicAllowed service.parameter_sets


basicAllowed : List Set -> Bool
basicAllowed sets =
    let
        noMandatory param current =
            case param.mandatory of
                True ->
                    False

                False ->
                    current

        hasNoMandatoryFields set current =
            case List.foldl noMandatory True set of
                True ->
                    True

                False ->
                    current
    in
        List.foldl hasNoMandatoryFields False sets


hasAdvanced : Model -> Bool
hasAdvanced service =
    let
        hasNonEmptyList set current =
            case List.isEmpty set of
                True ->
                    current

                False ->
                    True
    in
        List.foldl hasNonEmptyList False service.parameter_sets


nonEmptySets : List Set -> List Set
nonEmptySets sets =
    let
        nonEmpty set =
            not (List.isEmpty set)
    in
        List.filter nonEmpty sets
