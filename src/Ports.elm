port module Ports exposing (printOrder, storeSkuList)

import TableData exposing (..)


port storeSkuList : String -> Cmd msg


port printOrder : String -> Cmd msg
