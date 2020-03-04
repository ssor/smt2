module Order exposing
    ( Item
    , Order
    , StatusOrder(..)
    , decodeOrderId
    , fromBoolToFormatString
    , orderDecoder
    , orderEncoder
    , ordersEncoder
    , toSyncList
    )

import Json.Decode as Decode
    exposing
        ( Decoder
        , bool
        , int
        , list
        , string
        )
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as Encode exposing (..)
import Sku exposing (..)


type StatusOrder
    = Sync Order
    | NotSync Order


type alias Order =
    { id : String
    , items : List Item
    }


fromBoolToFormatString : Bool -> String
fromBoolToFormatString b =
    if b == True then
        "是"

    else
        "否"


type alias Item =
    { sku : Sku
    , count : Int
    }


itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "sku" Sku.skuDecoder
        |> required "count" Decode.int


itemEncoder : Item -> Encode.Value
itemEncoder item =
    Encode.object
        [ ( "sku", skuEncoder item.sku )
        , ( "count", Encode.int item.count )
        ]


fromNotSyncToSyncOrder : String -> StatusOrder -> StatusOrder
fromNotSyncToSyncOrder id order =
    case order of
        Sync _ ->
            order

        NotSync o ->
            if o.id == id then
                Sync o

            else
                order


toSyncList : String -> List StatusOrder -> List StatusOrder
toSyncList id all =
    List.map (fromNotSyncToSyncOrder id) all


decodeOrderId : String -> String
decodeOrderId all =
    let
        id =
            Decode.decodeString (Decode.field "id" Decode.string) all
    in
    case id of
        Err _ ->
            ""

        Ok data ->
            data


orderDecoder : Decoder Order
orderDecoder =
    Decode.succeed Order
        |> required "id" Decode.string
        |> required "items" (Decode.list itemDecoder)


orderEncoder : Order -> Encode.Value
orderEncoder order =
    Encode.object
        [ ( "id", Encode.string order.id )
        , ( "items", Encode.list itemEncoder order.items )
        ]


ordersEncoder : List Order -> Encode.Value
ordersEncoder orders =
    Encode.list orderEncoder orders
