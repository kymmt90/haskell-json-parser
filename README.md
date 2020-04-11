# json-parser

```
$ stack ghci
*Main Lib> parseTest parseJson "{ \"key\" : \"value\" , \"key2\" : 123, \"key3\" : false, \"key4\" : null , \"key5\" : [ 1 , 2 , 3 ] , \"key6\" : { \"nested\" : null } }"
JObject [("key",JString "value"),("key2",JNumber 123.0),("key3",JBool False),("key4",JNull),("key5",JArray [JNumber 1.0,JNumber 2.0,JNumber 3.0]),("key6",JObject [("nested",JNull)])]
```
