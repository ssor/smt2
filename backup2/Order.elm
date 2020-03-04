module Order exposing
    ( Item
    , Order
    , StatusOrder(..)
    , decodeOrderId
    , fromBoolToFormatString
    , orderColumnNames
    , orderDataToArray
    , orderDecoder
    , orderEncoder
    , ordersDataToArray
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
import TableData exposing (TableTitle)


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


orderColumnNames : List TableTitle
orderColumnNames =
    [ "订单编码", "商品编码", "名称", "属性", "数量", "已保存" ]
        |> List.map (\order -> TableTitle order)


ordersDataToArray : List StatusOrder -> List (List String)
ordersDataToArray orders =
    orders
        |> List.map
            (\order ->
                orderDataToArray order
            )


orderDataToArray : StatusOrder -> List String
orderDataToArray statusOrder =
    let
        toArray =
            \order ->
                [ order.id, order.items |> List.length |> String.fromInt ]
    in
    case statusOrder of
        Sync order ->
            List.append (toArray order) [ "Yes" ]

        NotSync order ->
            List.append (toArray order) [ "No" ]
