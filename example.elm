----------------------------------------------------------------------
--
-- example.elm
-- Example of using billstclair/elm-simple-xml-to-json
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Main exposing (..)

import Xml.SimpleXmlToJson exposing ( TagSpec, Required(..)
                                    , xmlToJson, decodeXml
                                    )

import Xml
import Xml.Decode as XD

import Json.Decode as JD
import Json.Encode as JE

import Html exposing ( Html, Attribute
                     , div, p, pre, text, input
                     )
import Html.Attributes exposing ( type_, checked )
import Html.Events exposing ( onCheck )

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

type alias Model =
    { xml : String
    , isComplex : Bool
    }

type Msg
    = SetXml String
    | SetIsComplex Bool

simpleXml =
    """
<?xml version="1.0" encoding="UTF-8"?>
<person>
  <name>noah</name>
  <age max="100">50</age>
</person>
    """

-- Will get more complicated
complexXml =
    """
<person>
  <name>irving</name>
  <age max="100">20</age>
  <sex>yes</sex>
  <favoriteColor>blue</favoriteColor>
</person>
    """

init : (Model, Cmd Msg)
init =
    ( { xml = simpleXml
      , isComplex = False
      }
    , Cmd.none
    )

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : JD.Decoder Person
personDecoder =
    JD.map2 Person
        (JD.field "name" JD.string)
        (JD.field "age" JD.int)

personTagSpecs : List TagSpec
personTagSpecs =
    [ ("name", Required)
    , ("age", Required)
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetXml xml ->
            ( { model | xml = xml }
            , Cmd.none
            )
        SetIsComplex isComplex ->
            ( { model
                  | isComplex = isComplex
                  , xml = if isComplex then complexXml else simpleXml
              }
            , Cmd.none
            )

b : List (Html msg) -> Html msg
b body =
    Html.b [] body

br : Html msg
br =
    Html.br [][]

view : Model -> Html Msg
view model =
    let xml = model.xml
        xval = XD.decode xml
        xs = case xval of
                 Ok v -> toString v
                 Err msg -> msg
        val = case xval of
                  Ok v -> JE.encode 1 <| Xml.xmlToJson v
                  Err msg -> msg
        simpleVal = case xval of
                        Ok v -> JE.encode 1 <| xmlToJson v
                        Err msg -> msg
        -- This simple call will suffice for most of your XML parsing.
        decodedSimpleValue = decodeXml xml "person" personDecoder personTagSpecs
        decodedString = case decodedSimpleValue of
                            Err msg -> "Error:" ++ msg
                            Ok s -> toString s
    in
        div []
            [ p []
                  [ input [ type_ "checkbox"
                          , checked model.isComplex
                          , onCheck SetIsComplex
                          ]
                        []
                  , text " complex"
                  ]
            , b [ text "XML:" ]
            , pre []
                [ text xml ]
            , b [ text "Decoded:" ]
            , pre []
                [ text decodedString ]
            , b [ text "Xml.SimpleXmlToJson.xmlToJson:" ]
            , pre []
                [ text simpleVal ]
            , b [ text "Xml.xmlToJson:" ]
            , pre []
                [ text val ]
            , b [ text "Parsed XML:" ]
            , pre []
                [ text xs ]
            ]
