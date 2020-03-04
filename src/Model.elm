module Model exposing
    ( Model
    , Msg(..)
    , Page(..)
    )

import File exposing (File)
import File.Select as Select
import Order exposing (..)
import OrderPrepare exposing (..)
import RemoteData exposing (WebData)
import Sku exposing (..)
import Time exposing (..)
import Http exposing (..)

type alias Model =
    { orderList : List Order.Order
    , skuList : List Sku
    , page : Page
    , newOrderPrepare : OrderPrepare
    , giftList : List String
    , time : Maybe Time.Posix

    -- , csv : Maybe String
    }


type Page
    = PageSkuList
    | PageAddOrder
    | PageHome


type Msg
    = SkuDataReceived (WebData (List Sku))
    | ToShowSkuListPage
    | ToAddOrderPage
    | AddNewItemToOrder
    | DeleteOrderItem String
    | AddGiftToOrder
    | SelectGift String
    | DeleteGift String
    | PrintOrder
    | ResetOrder
    | SearchProductById String
    | ProductCountInOrderChanged String
    | Tick Time.Posix
    | CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | PatchPostSkuResponse (Result Http.Error String)
