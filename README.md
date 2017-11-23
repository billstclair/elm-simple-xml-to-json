The [billstclair/elm-simple-xml-to-json](http://package.elm-lang.org/packages/billstclair/elm-simple-xml-to-json/latest) package is an extension of the [eeue56/elm-xml](http://package.elm-lang.org/packages/eeue56/elm-xml/latest) package that simplifies the output of `Xml.xmlToJson`, removing all attributes, and replacing the lists containing attributes and `value` with just the value. It greatly simplifies writing standard `Json.Decode` decoders for simple XML.

The `Xml.SimpleXmlToJson` module has a single exported function, `xmlToJson`. There's an example of using it in `example.elm`, which you can run with elm reactor:

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
        
**`Xml.SimpleXmlToJson.xmlToJson:`**

    [
     null,
     {
      "person": [
       {
        "name": "noah"
       },
       {
        "age": 50
       }
      ]
     },
     {
      "person": [
       {
        "name": "josh"
       },
       {
        "age": 57
       }
      ]
     }
    ]

**`Xml.xmlToJson:`**

    [
     null,
     {
      "person": {
       "value": [
        {
         "name": {
          "value": "noah"
         }
        },
        {
         "age": {
          "max": 100,
          "value": 50
         }
        }
       ]
      }
     },
     {
      "person": {
       "value": [
        {
         "name": {
          "value": "josh"
         }
        },
        {
         "age": {
          "max": 100,
          "value": 57
         }
        }
       ]
      }
     }
    ]

**`Decoded:`**

    [Nothing,Just { name = "noah", age = 50 },Just { name = "josh", age = 57 }]
