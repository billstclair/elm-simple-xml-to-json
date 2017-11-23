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

module Xml.SimpleXmlToJson exposing ( xmlToJson )

{-|

Simplify the output of `Xml.xmlToJson`, removing attributes.

@docs xmlToJson

-}

import Xml
import Json.Encode as JE
import Json.Decode as JD

{-|
Convert the `Xml.Value` returned by `Xml.Decode.decode` to a `Json.Encode.Value`,
removing all the attributes.
-}
xmlToJson : Xml.Value -> JE.Value
xmlToJson xml =
    let value = Xml.xmlToJson xml
    in
        decodeXmlValue value

decodeXmlValue : JE.Value -> JE.Value
decodeXmlValue value =
    case JD.decodeValue xmlValueDecoder value of
        Ok val ->
            val
        Err s ->
            value

-- Simplify the parsed XML by replacing the objects containing a "value"
xmlValueDecoder : JD.Decoder JE.Value
xmlValueDecoder =
    JD.oneOf
        [ JD.map decodeXmlValue <| JD.field "value" JD.value
        , JD.map JE.list (JD.list <| JD.lazy (\_ -> xmlValueDecoder))
        , JD.map JE.object
            <| JD.keyValuePairs (JD.lazy (\_ -> xmlValueDecoder))
        , JD.value
        ]
