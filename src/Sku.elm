module Sku exposing
    ( Sku
    , decodeSkuListFromString
    , emptySku
    , skuDecoder
    , skuEncoder
    , skuFromStringArray
    )

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as Encode exposing (Value)


type alias Sku =
    { code : String
    , name : String
    , attr : String
    }


emptySku : Sku
emptySku =
    Sku "" "" ""


skuFromStringArray : List String -> Maybe Sku
skuFromStringArray list =
    if List.length list < 3 then
        Nothing

    else
        let
            code =
                list |> List.head |> Maybe.withDefault "" |> String.trim

            name =
                list |> List.drop 1 |> List.head |> Maybe.withDefault ""

            attr =
                list |> List.drop 2 |> List.head |> Maybe.withDefault ""
        in
        if String.length code <= 0 then
            Nothing

        else
            Just (Sku code name attr)


decodeSkuListFromString : String -> List Sku
decodeSkuListFromString s =
    case Decode.decodeString (list skuDecoder) s of
        Err _ ->
            []

        Ok result ->
            result


skuDecoder : Decoder Sku
skuDecoder =
    Decode.succeed Sku
        |> required "id" string
        |> required "name" string
        |> required "attr" string


skuEncoder : Sku -> Value
skuEncoder sku =
    Encode.object
        [ ( "id", Encode.string sku.code )
        , ( "name", Encode.string sku.name )
        , ( "attr", Encode.string sku.attr )
        ]
