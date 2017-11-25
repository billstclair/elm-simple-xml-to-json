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

module Xml.SimpleXmlToJson exposing ( TagDecoder, Required(..)
                                    , xmlToJson, tagDecoder
                                    )

{-|

Simplify the output of `Xml.xmlToJson`, removing attributes.

Provide a decoder to ease turning that into an Elm record.

# Types
@docs TagDecoder, Required

# Functions
@docs xmlToJson

# Decoders
@docs tagDecoder

-}

import Xml
import Json.Encode as JE exposing ( Value )
import Json.Decode as JD exposing ( Decoder )
import List.Extra as LE

{-|
Convert the `Xml.Value` returned by `Xml.Decode.decode` to a `Json.Encode.Value`,
removing all the attributes.
-}
xmlToJson : Xml.Value -> Value
xmlToJson xml =
    let value = Xml.xmlToJson xml
    in
        decodeXmlValue value

decodeXmlValue : Value -> Value
decodeXmlValue value =
    case JD.decodeValue xmlValueDecoder value of
        Ok val ->
            val
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
type alias TagDecoder =
    (String, Required, Decoder Value)

{-| Decode the contents of an XML tag with subtags.

Each TagDecoder pulls one or more matching tags from the list.

Unspecified tags in the parsed `Value` are skipped.
-}
tagDecoder : List TagDecoder -> Decoder Value
tagDecoder subtagDecoders =
    JD.list JD.value |> JD.andThen (doTagDecode subtagDecoders)

oneTagDecoder : String -> Decoder Value
oneTagDecoder tag =
    JD.field tag JD.value

doTagDecode : List TagDecoder -> List Value -> Decoder Value
doTagDecode decoders values =
    let loop : List TagDecoder -> List Value -> List Value -> Decoder Value
        loop = (\dcdrs vals res ->
                    case dcdrs of
                        [] ->
                            JD.succeed <| JE.list (List.reverse res)
                        (tag, req, tagDcdr) :: dcdrsTail ->
                            case req of
                                Multiple ->
                                    case decodeMultiple tag tagDcdr vals of
                                        Ok (valsTail, resPrefix) ->
                                            loop dcdrsTail valsTail
                                                <| List.append resPrefix res
                                        Err msg ->
                                            JD.fail msg
                                _ ->
                                    case vals of
                                        [] ->
                                            let r = JE.list <| List.reverse res
                                            in
                                                case dcdrs of
                                                    [] ->
                                                        JD.succeed r
                                                    _ ->
                                                        hangingVals dcdrs r
                                        val :: valsTail ->
                                            case decodeOne tag req tagDcdr val valsTail of
                                                Ok (vtail, oneRes) ->
                                                    loop dcdrsTail vtail
                                                        <| case req of
                                                               RequiredIgnore ->
                                                                   res
                                                               _ ->
                                                                   oneRes :: res
                                                Err msg ->
                                                    JD.fail msg
               )

        decodeOne : String -> Required -> Decoder Value -> Value -> List Value -> Result String (List Value, Value)
        decodeOne = (\tag req dcdr val valsTail ->
                         case JD.decodeValue (oneTagDecoder tag) val of
                             Ok v ->
                                 case req of
                                     RequiredIgnore ->
                                       Ok (valsTail, JE.null)
                                     _ ->
                                         case JD.decodeValue dcdr v of
                                             Ok v ->
                                                 Ok (valsTail, v)
                                             Err msg ->
                                                 case req of
                                                     Required ->
                                                         Err msg
                                                     _ ->
                                                         decodeOptional
                                                             tag dcdr valsTail
                             Err msg ->
                                 Err msg
                    )

        decodeOptional : String -> Decoder Value -> List Value -> Result String (List Value, Value)
        decodeOptional = (\tag dcdr vals ->
                              Err "decodeOptional not yet implemented."
                         )

        decodeMultiple : String -> Decoder Value -> List Value -> Result String (List Value, List Value)
        decodeMultiple = (\tag dcdr vals ->
                              Err "decodeMultiple not yet implemented."
                         )

        hangingVals : List TagDecoder -> Value -> Decoder Value
        hangingVals = (\dcdrs res ->
                           JD.fail "hangingVals not yet implemented."
                      )
    in
        loop decoders values []
                                    
