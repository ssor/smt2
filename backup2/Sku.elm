module Sku exposing
    ( Sku
    , emptySku
    , skuColumnNames
    , skuDataToArray
    , skuDecoder
    , skuEncoder
    , skuToArray
    )

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as Encode exposing (Value)
import TableData exposing (TableTitle)


type alias Sku =
    { code : String
    , name : String
    , attr : String
    }


emptySku : Sku
emptySku =
    Sku "" "" ""


skuDecoder : Decoder Sku
skuDecoder =
    Decode.succeed Sku
        |> required "code" string
        |> required "name" string
        |> required "attr" string


skuEncoder : Sku -> Value
skuEncoder sku =
    Encode.object
        [ ( "code", Encode.string sku.code )
        , ( "name", Encode.string sku.name )
        , ( "attr", Encode.string sku.attr )
        ]


skuDataToArray : List Sku -> List (List String)
skuDataToArray skus =
    skus
        |> List.map (\sku -> [ sku.code, sku.name, sku.attr ])


skuToArray : Sku -> List String
skuToArray sku =
    [ sku.code, sku.name, sku.attr ]


skuColumnNames : List TableTitle
skuColumnNames =
    [ "商品编码", "名称", "属性" ]
        |> List.map (\name -> TableTitle name)

