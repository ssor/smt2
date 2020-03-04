module Msg exposing (..)

import Page.AddOrder as PageAddOrder exposing (..)
import Page.Home as PageHome exposing (..)
import Page.Sku as PageSkuList exposing (..)


type Msg
    = ToShowHome PageHome.Msg
    | ToShowSkuListPage PageSkuList.Msg
    | ToAddOrderPage PageAddOrder.Msg
