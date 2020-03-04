module OrderPrepare exposing
    ( OrderPrepare
    ,  defaultGiftList
       -- , emptyOrderPrepare

    , initOrderPrepare
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
    , currentGift : Maybe String
    , giftList : List (Maybe String)
    , giftCount : Int
    }


initOrderPrepare : OrderPrepare
initOrderPrepare =
    let
        gift =
            List.head defaultGiftList
    in
    OrderPrepare "" 1 Nothing [] gift [] 0


defaultGiftList : List String
defaultGiftList =
    [ "纤维布"
    , "加长螺丝"
    , "铜箔纸包装"
    ]



-- emptyOrderPrepare : OrderPrepare
-- emptyOrderPrepare =
--     OrderPrepare "" 1 Nothing [] Nothing [] 0


saveItemToOrderPrepare : OrderPrepare -> OrderPrepare
saveItemToOrderPrepare op =
    case op.sku of
        Nothing ->
            op

        Just sku ->
            let
                emptyOp =
                    initOrderPrepare

                newItem =
                    Order.Item sku op.count

                items =
                    op.itemsForNewOrder ++ [ newItem ]
            in
            { emptyOp
                | itemsForNewOrder = items
                , giftList = op.giftList
            }
