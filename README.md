The [billstclair/elm-simple-xml-to-json](http://package.elm-lang.org/packages/billstclair/elm-simple-xml-to-json/latest) package is an extension of the [eeue56/elm-xml](http://package.elm-lang.org/packages/eeue56/elm-xml/latest) package that simplifies the output of `Xml.xmlToJson`, removing all attributes, and replacing the lists containing attributes and `value` with just the value. It greatly simplifies writing standard `Json.Decode` decoders for simple XML.

There's an example in `example.elm`, which you can run with elm reactor:

    cd .../SimpleXmlToJson
    elm reactor
    
Then aim your web browser at [localhost:8000/example.elm](http://localhost:8000/example.elm). Its output is below. Note how much simpler the first JSON is than the second. If the XML you need to bring in to Elm has no important attributes, just tags with values, it will be much easier to create a `Json.Decode.Decoder` for it.

**`XML:`**

    <?xml version="1.0" encoding="UTF-8"?>
    <person>
      <name>noah</name>
      <age max="100">50</age>
    </person>
    <person>
      <name>josh</name>
      <age max="100">57</age>
    </person>
        
**`Decoded:`**

    { name = "noah", age = 50 }

Given these definitions:

    import Json.Decoder as JD exposing ( Decoder )
    import Xml.SimpleXmlToJson exposing ( decodeXml )

    type alias Person =
        { name : String
        , age : Int
        }

    personDecoder : Decoder Person
    personDecoder =
        JD.map2 Person
            (JD.field "name" JD.string)
            (JD.field "age" JD.int)

    personTagSpecs : List TagSpec
    personTagSpecs =
        [ ("name", Required)
        , ("age", Required)
        ]
       
    xml =
        """
    <?xml version="1.0" encoding="UTF-8"?>
    <person>
      <name>noah</name>
      <age max="100">50</age>
    </person>
        """

Here's the code to turn that XML string into a `Person` object:

    decodeXml xml "person" personDecoder personTagSpecs

If your XML has more than one top-level tag, you can use the other exported functions, which the code for `decodeXml` should help you understand, to handle it.
