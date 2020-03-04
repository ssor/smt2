module OrderPrepare exposing
    ( OrderPrepare
    , emptyOrderPrepare
    , saveItemToOrderPrepare
    )

import Order exposing (..)
import Sku exposing (..)
import Time exposing (..)


type alias OrderPrepare =
    { productId : String
    , count : Int
    , sku : Maybe Sku
    , itemsForNewOrder : List Order.Item
    }


emptyOrderPrepare : OrderPrepare
emptyOrderPrepare =
    OrderPrepare "" 1 Nothing []


saveItemToOrderPrepare : OrderPrepare -> OrderPrepare
saveItemToOrderPrepare op =
    case op.sku of
        Nothing ->
            op

        Just sku ->
            let
                emptyOp =
                    emptyOrderPrepare

                newItem =
                    Order.Item sku op.count

                items =
                    newItem :: op.itemsForNewOrder
            in
            { op
                | itemsForNewOrder = items
            }
