----------------------------------------------------------------------
--
-- SimpleXmlToJson.elm
-- Convert simple XML to JSON, ignoring attributes.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Xml.SimpleXmlToJson exposing ( TagSpec, Required(..)
                                    , decodeXml, stringToJson, xmlToJson
                                    , tagDecoder
                                    )

{-|

Simplify the output of `Xml.xmlToJson`, removing attributes.

Provide a decoder to ease turning that into an Elm record.

# Types
@docs TagSpec, Required

# Decoders
@docs tagDecoder

# Functions
@docs decodeXml, stringToJson, xmlToJson

-}

import Xml
import Xml.Decode as XD
import Json.Encode as JE exposing ( Value )
import Json.Decode as JD exposing ( Decoder )
import List.Extra as LE

{-| Decode an XML string containing a single tag into an Elm value.
-}
decodeXml : String -> String -> Decoder value -> List TagSpec -> Result String value
decodeXml xml tag decoder tagSpecs =
    case stringToJson xml of
        Err msg ->
            Err msg
        Ok value ->
            case JD.decodeValue (JD.list JD.value) value
            of
                Ok list ->
                    case list of
                        [a] ->
                            JD.decodeValue
                                (JD.field tag <| tagDecoder decoder tagSpecs)
                                a
                        _ ->
                            Err "Xml did not contain a single tag."
                Err msg ->
                    Err msg                

{-| Decode an XML string into a simplified `Json.Encode.Value`.
-}
stringToJson : String -> Result String Value
stringToJson string =
    case XD.decode string of
        Ok val ->
            Ok <| xmlToJson val
        Err msg ->
            Err msg

{-|
Convert the `Xml.Value` returned by `Xml.Decode.decode` to a `Json.Encode.Value`,
removing all the attributes.
-}
xmlToJson : Xml.Value -> Value
xmlToJson xml =
    let value = Xml.xmlToJson xml
    in
        decodeXmlValue value

removeLeadingNullDecoder : Decoder Value
removeLeadingNullDecoder =
    JD.list JD.value
        |> JD.andThen (\list ->
                           case list of
                               a :: rest ->
                                   if a == JE.null then
                                       JD.succeed (JE.list rest)
                                   else
                                       JD.succeed (JE.list list)
                               _ ->
                                   JD.succeed <| JE.list list
                            )

removeLeadingNull : Value -> Value
removeLeadingNull value =
    case JD.decodeValue removeLeadingNullDecoder value of
        Ok val ->
            val
        Err _ ->
            value

decodeXmlValue : Value -> Value
decodeXmlValue value =
    case JD.decodeValue xmlValueDecoder value of
        Ok val ->
            removeLeadingNull val
        Err s ->
            value

listToNull : List Value -> Decoder Value
listToNull list =
    case list of
        [] ->
            JD.succeed JE.null
        _ ->
            JD.fail "Not an empty list"

-- Simplify the parsed XML by replacing the objects containing a "value"
xmlValueDecoder : Decoder Value
xmlValueDecoder =
    JD.oneOf
        [ JD.map decodeXmlValue <| JD.field "value" JD.value
        , JD.list JD.value |> JD.andThen listToNull
        , JD.map JE.list (JD.list <| JD.lazy (\_ -> xmlValueDecoder))
        , JD.map JE.object
            <| JD.keyValuePairs (JD.lazy (\_ -> xmlValueDecoder))
        , JD.value
        ]

{-| For `tagDecoder`. How to handle a tag.

Required tags error if not there.
RequiredIgnore tags must be in the XML, but are not returned.
Optional tags will become null if not in the XML.
Multiple tags become a list.
-}
type Required
    = Required
    | RequiredIgnore
    | Optional
    | Multiple

{-| A description of one tag to decode: (<tag name>, Required, <tag decoder>)
-}
type alias TagSpec =
    (String, Required)

{-| Decode the contents of an XML tag with subtags.

Each TagSpec pulls one or more matching tags from the list.

Unspecified tags in the parsed `Value` are skipped.

You end up with a single JSON object, with the tags as keys,
to which you can apply a standard JSON decoder.
-}
tagDecoder : Decoder value -> List TagSpec -> Decoder value
tagDecoder decoder tagSpecs =
    tagValueDecoder tagSpecs
        |> JD.andThen
           (\value ->
                case JD.decodeValue decoder value of
                    Ok value ->
                        JD.succeed value
                    Err msg ->
                        JD.fail msg
           )

tagValueDecoder : List TagSpec -> Decoder Value
tagValueDecoder tagSpecs =
    JD.list JD.value |> JD.andThen (doTagDecode tagSpecs)

oneTagDecoder : String -> Decoder Value
oneTagDecoder tag =
    JD.field tag JD.value

doTagDecode : List TagSpec -> List Value -> Decoder Value
doTagDecode tagSpecs values =
    let loop : List TagSpec -> List Value -> List (String, Value) -> Decoder Value
        loop = (\dcdrs vals res ->
                    case dcdrs of
                        [] ->
                            JD.succeed <| JE.object res
                        (tag, req) :: dcdrsTail ->
                            case req of
                                Multiple ->
                                    case decodeMultiple tag vals of
                                        Ok (valsTail, value) ->
                                            loop dcdrsTail valsTail
                                                <| (tag, value) :: res
                                        Err msg ->
                                            JD.fail msg
                                _ ->
                                    case vals of
                                        [] ->
                                            hangingVals dcdrs res
                                        val :: valsTail ->
                                            case decodeOne tag req val valsTail of
                                                Ok (vtail, oneRes) ->
                                                    loop dcdrsTail vtail
                                                        <| case req of
                                                               RequiredIgnore ->
                                                                   res
                                                               _ ->
                                                                   (tag, oneRes) :: res
                                                Err msg ->
                                                    JD.fail msg
               )

    in
        loop tagSpecs values []
                                    
-- Here when we've successfully parsed all the elements of the list.
-- If there are any tag specs left, make sure they're all
-- Optional or Multiple, and append nulls or empty lists.
-- If any Required specs are left, error.
hangingVals : List TagSpec -> List (String, Value) -> Decoder Value
hangingVals dcdrs res =
    let hloop = (\dt r ->
                     case dt of
                         [] ->
                             JD.succeed <| JE.object r
                         (tag, req) :: dtt ->
                             case req of
                                 Optional ->
                                     hloop dtt <| (tag, JE.null) :: r
                                 Multiple ->
                                     hloop dtt <| (tag, JE.list []) :: r
                                 _ ->
                                     JD.fail <| "Tag not found: " ++ tag
                )
    in
        hloop dcdrs res

-- Decode a single tag from the list.
-- Skip elements with other tags until you find one with the passed `tag`.
-- The `Required` arg is guaranteed NOT to be `Multiple`.
decodeOne : String -> Required -> Value -> List Value -> Result String (List Value, Value)
decodeOne tag req val valsTail =
    let decoder = oneTagDecoder tag
        d1loop =
            (\v vt ->
                 case JD.decodeValue decoder v of
                     Ok v ->
                         case req of
                             RequiredIgnore ->
                                 Ok (valsTail, JE.null)
                             _ ->
                                 Ok (valsTail, v)
                     Err msg ->
                         case vt of
                             [] ->
                                 case req of
                                     Optional ->
                                         Ok (valsTail, JE.null)
                                     _ ->
                                         Err <| "Required tag not found: " ++ tag
                             vv :: vvt ->
                                 d1loop vv vvt
            )
    in
        d1loop val valsTail

decodeMultiple : String -> List Value -> Result String (List Value, Value)
decodeMultiple tag vals =
    Err "decodeMultiple not yet implemented."
