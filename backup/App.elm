port module App exposing (main)

import Browser exposing (..)
import Debug exposing (..)
import Delay exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
    exposing
        ( checked
        , class
        , height
        , href
        , id
        , max
        , min
        , src
        , style
        , type_
        , value
        , width
        )
import Html.Events exposing (onCheck, onClick, onInput)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Order exposing (..)
import RemoteData exposing (WebData)
import Sku exposing (..)
import TableData exposing (TableData)
import Task exposing (..)
import Time exposing (Posix, every)


type alias Model =
    { orderList : List Order.StatusOrder
    , skuList : List Sku
    , page : Page
    , newOrderPrepare : OrderPrepare
    , time : Maybe Time.Posix
    }


type alias OrderPrepare =
    { orderId : Maybe String
    , productId : String
    , count : Int
    , sku : Maybe Sku
    , itemsForNewOrder : List Order.Item
    , xianwei : Bool
    , tongbozhi : Bool
    , luosi : Bool
    }


type OtherCheck
    = Xianweibu
    | Tongbozhi
    | Luosi


emptyOrderPrepare : OrderPrepare
emptyOrderPrepare =
    OrderPrepare Nothing "" 1 Nothing [] False False False


saveItemToOrderPrepare : OrderPrepare -> Time.Posix -> OrderPrepare
saveItemToOrderPrepare op posix =
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
                , orderId = Just (posix |> Time.posixToMillis |> String.fromInt)
            }


type Page
    = PageOrderList
    | PageSkuList
    | PageAddOrder
    | PageMain


type Msg
    = ToShowOrderListPage
    | OrderDataReceived (WebData (List Order.Order))
    | SkuDataReceived (WebData (List Sku))
    | ToShowSkuListPage
    | ToAddOrderPage
    | AddNewItemToOrder
    | CreateOrder
    | DeleteOrder String
    | SearchProductById String
    | OrderIdChanged String
    | ProductCountInOrderChanged String
    | SyncOrderResponse (Result Http.Error String)
    | OrderDeleted (Result Http.Error String)
    | OpinionChanged OtherCheck Bool
    | Tick Time.Posix


view : Model -> Html Msg
view model =
    topLayout model


topLayout : Model -> Html Msg
topLayout model =
    div
        []
        [ navView model
        , columnsView model
        ]


navView : Model -> Html Msg
navView model =
    nav
        [ class "navbar is-spaced has-shadow" ]
        [ div
            [ class "navbar-brand" ]
            [ a
                [ class "navbar-item"
                , href "https://bulma.io"
                ]
                [ img
                    [ src "https://bulma.io/images/bulma-logo.png"
                    , width 112
                    , height 28
                    ]
                    []
                ]
            ]
        ]


columnsView : Model -> Html Msg
columnsView model =
    div [ class "columns", style "padding-top" "20px" ]
        [ menuView model
        , mainContentView model
        , div [ class "column is-1" ] []
        ]


mainContentView : Model -> Html Msg
mainContentView model =
    case model.page of
        PageMain ->
            div
                [ class "column", style "padding-left" "solid 1px rgba(100,100,100,0.2)" ]
                []

        PageOrderList ->
            div
                [ class "column", style "padding-left" "solid 1px rgba(100,100,100,0.2)" ]
                [ h1
                    [ class "title is-4" ]
                    [ text "配货单列表" ]
                , orderListTableView model
                ]

        PageSkuList ->
            div
                [ class "column", style "padding-left" "solid 1px rgba(100,100,100,0.2)" ]
                [ h1
                    [ class "title is-4" ]
                    [ text "产品列表" ]
                , skuListTableView model
                ]

        PageAddOrder ->
            div
                [ class "column", style "padding-left" "solid 1px rgba(100,100,100,0.2)" ]
                [ h1
                    [ class "title is-4" ]
                    [ text "新配货单" ]
                , inputControlsForAddOrder model
                ]


skuListTableView : Model -> Html Msg
skuListTableView model =
    table
        [ id "data-list", class "table", style "width" "100%" ]
        [ tr []
            [ th [] [ text "编码" ]
            , th [] [ text "名称" ]
            , th [] [ text "属性" ]
            ]
        , tbody [] (List.map skuItemTableBody model.skuList)
        ]


skuItemTableBody : Sku -> Html Msg
skuItemTableBody sku =
    tr []
        [ td [] [ text sku.code ]
        , td [] [ text sku.name ]
        , td [] [ text sku.attr ]
        ]


orderListTableView : Model -> Html Msg
orderListTableView model =
    table [ class "table", style "width" "100%" ]
        [ thead []
            [ tr []
                [ th []
                    [ abbr [] [ text "编码" ]
                    ]
                , th []
                    [ abbr
                        []
                        [ text "产品数量" ]
                    ]
                , th []
                    [ abbr
                        []
                        [ text "纤维布" ]
                    ]
                , th []
                    [ abbr
                        []
                        [ text "铜箔纸包装" ]
                    ]
                , th []
                    [ abbr
                        []
                        [ text "加长螺丝" ]
                    ]
                , th []
                    [ abbr
                        []
                        [ text "同步状态" ]
                    ]
                ]
            ]
        , tbody [] (List.map orderItemTableBody model.orderList)
        ]


orderItemTableBody : Order.StatusOrder -> Html Msg
orderItemTableBody statusOrder =
    case statusOrder of
        Order.Sync order ->
            tr []
                [ td [] [ text order.id ]
                , td [] [ text (order.items |> List.length |> String.fromInt) ]
                , td [] [ text (Order.fromBoolToFormatString order.xianwei) ]
                , td [] [ text (Order.fromBoolToFormatString order.tongbozhi) ]
                , td [] [ text (Order.fromBoolToFormatString order.luosi) ]
                , td [] [ text "已同步" ]
                ]

        Order.NotSync order ->
            tr []
                [ td [] [ text order.id ]
                , td [] [ text (order.items |> List.length |> String.fromInt) ]
                , td [] [ text (Order.fromBoolToFormatString order.xianwei) ]
                , td [] [ text (Order.fromBoolToFormatString order.tongbozhi) ]
                , td [] [ text (Order.fromBoolToFormatString order.luosi) ]
                , td [] [ text "未同步" ]
                ]


inputControlsForAddOrder : Model -> Html Msg
inputControlsForAddOrder model =
    let
        orderId =
            Maybe.withDefault "01" model.newOrderPrepare.orderId

        _ =
            Debug.log "fill control for order: " orderId

        _ =
            Debug.log "fill control with items: " model.skuList
    in
    div
        [ class "container is-fullhd" ]
        [ div
            [ class "notification" ]
            [ inputOrderId orderId
            , br [] []
            , inputForOrderItems model.newOrderPrepare
            ]
        ]


inputForOrderItems : OrderPrepare -> Html Msg
inputForOrderItems prepare =
    case prepare.sku of
        Nothing ->
            div []
                [ inputProductId prepare.productId
                , inputProductName ""
                , inputProductAttr ""
                , inputProductCountInOrder prepare.count
                , inputCheckOptions prepare
                , hr [ style "border-top" "1px solid #bbb" ] []
                , br [] []
                , orderItemsTable prepare
                ]

        Just sku ->
            div []
                [ inputProductId prepare.productId
                , inputProductName sku.name
                , inputProductAttr sku.attr
                , inputProductCountInOrder prepare.count
                , inputCheckOptions prepare
                , hr [ style "border-top" "1px solid #bbb" ] []
                , br [] []
                , orderItemsTable prepare
                ]


orderItemsTable : OrderPrepare -> Html Msg
orderItemsTable prepare =
    table [ class "table", style "width" "100%" ]
        [ thead []
            [ tr []
                [ th []
                    [ abbr [] [ text "商品编码" ]
                    ]
                , th []
                    [ abbr
                        []
                        [ text "名称" ]
                    ]
                , th []
                    [ abbr
                        []
                        [ text "属性" ]
                    ]
                , th []
                    [ abbr
                        []
                        [ text "数量" ]
                    ]
                ]
            ]
        , tbody [] (List.map inputForOrderItem prepare.itemsForNewOrder)
        ]


inputForOrderItem : Order.Item -> Html Msg
inputForOrderItem item =
    tr []
        [ td [] [ text item.sku.code ]
        , td [] [ text item.sku.name ]
        , td [] [ text item.sku.attr ]
        , td [] [ text (String.fromInt item.count) ]
        ]


inputCheckOptions : OrderPrepare -> Html Msg
inputCheckOptions prepare =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "赠品" ]
            ]
        , div [ class "field-body" ]
            [ div
                [ class "field is-grouped" ]
                [ div [ class "control" ]
                    [ label [ class "checkbox" ]
                        [ input
                            [ type_ "checkbox"
                            , checked prepare.xianwei
                            , onCheck (OpinionChanged Xianweibu)
                            ]
                            []
                        , text "纤维布"
                        ]
                    ]
                , div [ class "control" ]
                    [ label [ class "checkbox" ]
                        [ input
                            [ type_ "checkbox"
                            , checked prepare.tongbozhi
                            , onCheck (OpinionChanged Tongbozhi)
                            ]
                            []
                        , text "铜箔纸包装"
                        ]
                    ]
                , div [ class "control" ]
                    [ label [ class "checkbox" ]
                        [ input
                            [ type_ "checkbox"
                            , checked prepare.luosi
                            , onCheck (OpinionChanged Luosi)
                            ]
                            []
                        , text "加长螺丝"
                        ]
                    ]
                ]
            ]
        ]


inputProductName : String -> Html Msg
inputProductName name =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-small" ]
            [ label [ class "label" ] [ text "详细信息" ]
            ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ label [ class "label" ] [ text name ]
                ]
            ]
        ]


inputProductCountInOrder : Int -> Html Msg
inputProductCountInOrder count =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "产品数量" ]
            ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , value (String.fromInt count)
                        , onInput ProductCountInOrderChanged
                        ]
                        []
                    ]
                ]
            ]
        ]



-- buttonCreateOrder : Html Msg
-- buttonCreateOrder =
--     div [ class "field is-horizontal" ]
--         [ div [ class "field-label is-normal" ] []
--         , div [ class "field-body" ]
--             [ div [ class "field" ]
--                 [ div [ class "control" ]
--                     [ button
--                         [ class "button is-primary"
--                         , onClick CreateOrder
--                         , style "width" "100%"
--                         ]
--                         [ text "创建" ]
--                     ]
--                 ]
--             ]
--         ]


inputProductAttr : String -> Html Msg
inputProductAttr attr =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-small" ]
            [ label [ class "label" ] [ text "" ]
            ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ label [ class "label" ] [ text attr ]
                ]
            ]
        ]


inputProductId : String -> Html Msg
inputProductId code =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ] [ text "商品编码" ]
            ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ input [ class "input", type_ "text", onInput SearchProductById, value code ]
                        []
                    ]
                ]
            , div [ class "field" ]
                [ div [ class "control" ]
                    [ button
                        [ class "button is-info"
                        , onClick AddNewItemToOrder
                        ]
                        [ text "添加" ]
                    ]
                ]
            ]
        ]


inputOrderId : String -> Html Msg
inputOrderId v =
    div [ class "field is-horizontal" ]
        [ div [ class "field-label is-normal" ]
            [ label [ class "label" ]
                [ text "订单编码" ]
            ]
        , div [ class "field-body" ]
            [ div [ class "field" ]
                [ div
                    [ class "control" ]
                    [ input
                        [ class "input", type_ "text", onInput OrderIdChanged, value v ]
                        []
                    ]
                ]
            , div [ class "field" ]
                [ div [ class "control" ]
                    [ button
                        [ class "button is-primary"
                        , onClick CreateOrder
                        , style "width" "100%"
                        ]
                        [ text "创建" ]
                    ]
                ]
            ]
        ]


menuView : Model -> Html Msg
menuView model =
    div [ class "column is-2" ]
        [ aside
            [ class "menu bd-side", style "padding-left" "20px" ]
            [ p [ class "menu-label" ] [ text "配货单管理" ]
            , ul [ class "menu-list" ]
                [ li []
                    [ a [ onClick ToAddOrderPage ] [ text "添加配货单" ]
                    ]
                , li []
                    [ a [ onClick ToShowOrderListPage ] [ text "配货单列表" ]
                    ]
                ]
            , p [ class "menu-label" ] [ text "产品管理" ]
            , ul [ class "menu-list" ]
                [ li []
                    [ a [ onClick ToShowSkuListPage ] [ text "产品列表" ] ]
                ]
            ]
        ]


syncOrder : Order.StatusOrder -> Cmd Msg
syncOrder statusOrder =
    case statusOrder of
        Sync _ ->
            Cmd.none

        Order.NotSync order ->
            Http.post
                { url = "http://localhost:5019/order-list"
                , body =
                    order
                        |> Order.orderEncoder
                        |> Http.jsonBody
                , expect = Http.expectString SyncOrderResponse
                }


setupDeleteOrder : String -> Cmd Msg
setupDeleteOrder id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:5019/order-list/" ++ id
        , body = Http.emptyBody
        , expect = Http.expectString OrderDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


fetchOrders : Cmd Msg
fetchOrders =
    Http.get
        { url = "http://localhost:5019/order-list"
        , expect =
            Decode.list Order.orderDecoder
                |> Http.expectJson (RemoteData.fromResult >> OrderDataReceived)
        }


fetchSkuList : Cmd Msg
fetchSkuList =
    Http.get
        { url = "http://localhost:5019/sku-list"
        , expect =
            Decode.list skuDecoder
                |> Http.expectJson (RemoteData.fromResult >> SkuDataReceived)
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.batch [ fetchOrders, fetchSkuList ] )


initialModel : Model
initialModel =
    { orderList = []
    , skuList = []
    , newOrderPrepare = emptyOrderPrepare
    , page = PageMain
    , time = Nothing
    }


message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            updateTick posix model

        ToAddOrderPage ->
            toAddOrderPage model

        AddNewItemToOrder ->
            addNewItemToOrder model

        CreateOrder ->
            createOrder model

        DeleteOrder id ->
            deleteOrder id model

        OrderDeleted _ ->
            ( model, Cmd.none )

        ToShowOrderListPage ->
            toShowOrderListPage model

        ToShowSkuListPage ->
            ( { model | page = PageSkuList }, fetchSkuList )

        SkuDataReceived response ->
            skuDataReceived model response

        OrderDataReceived response ->
            orderDataReceived model response

        SearchProductById value ->
            searchProductById model value

        OrderIdChanged value ->
            orderIdChanged model value

        ProductCountInOrderChanged value ->
            productCountInOrderChanged model value

        OpinionChanged check value ->
            opinionChanged check value model

        SyncOrderResponse response ->
            syncOrderResponse response model


deleteOrder : String -> Model -> ( Model, Cmd Msg )
deleteOrder id model =
    ( model, setupDeleteOrder id )


opinionChanged : OtherCheck -> Bool -> Model -> ( Model, Cmd Msg )
opinionChanged check value model =
    let
        prepare =
            model.newOrderPrepare
    in
    case check of
        Xianweibu ->
            ( { model | newOrderPrepare = { prepare | xianwei = value } }, Cmd.none )

        Tongbozhi ->
            ( { model | newOrderPrepare = { prepare | tongbozhi = value } }, Cmd.none )

        Luosi ->
            ( { model | newOrderPrepare = { prepare | luosi = value } }, Cmd.none )


updateTick : Time.Posix -> Model -> ( Model, Cmd Msg )
updateTick posix model =
    case model.newOrderPrepare.orderId of
        Nothing ->
            let
                newId =
                    posix |> Time.posixToMillis |> String.fromInt

                prepare =
                    model.newOrderPrepare
            in
            ( { model | time = Just posix, newOrderPrepare = { prepare | orderId = Just newId } }, Cmd.none )

        Just _ ->
            ( model, Cmd.none )


syncOrderResponse : Result Http.Error String -> Model -> ( Model, Cmd Msg )
syncOrderResponse response model =
    case response of
        Err err ->
            let
                _ =
                    Debug.log "sync order failed" err
            in
            ( model, Cmd.none )

        Ok value ->
            let
                _ =
                    Debug.log "sync order success" value

                orderId =
                    Order.decodeOrderId value

                orderList =
                    Order.toSyncList orderId model.orderList
            in
            ( { model | orderList = orderList }, Cmd.none )


toShowOrderListPage : Model -> ( Model, Cmd Msg )
toShowOrderListPage model =
    let
        m =
            { model | page = PageOrderList }

        _ =
            Debug.log "order list: " model.orderList
    in
    ( m, Cmd.none )


orderDataReceived : Model -> WebData (List Order.Order) -> ( Model, Cmd Msg )
orderDataReceived model response =
    let
        m =
            case response of
                RemoteData.Success data ->
                    { model | orderList = List.map (\order -> Order.Sync order) data }

                _ ->
                    model
    in
    ( m, sendData m )


skuDataReceived : Model -> WebData (List Sku) -> ( Model, Cmd Msg )
skuDataReceived model response =
    let
        m =
            case response of
                RemoteData.Success data ->
                    { model | skuList = data }

                _ ->
                    model
    in
    ( m, sendData m )


toAddOrderPage : Model -> ( Model, Cmd Msg )
toAddOrderPage model =
    let
        _ =
            Debug.log "orders: " model.orderList
    in
    ( { model | page = PageAddOrder, newOrderPrepare = emptyOrderPrepare }, Cmd.none )


searchProductById : Model -> String -> ( Model, Cmd Msg )
searchProductById model value =
    let
        skuAfterFilter =
            List.filter (\sku -> sku.code == value) model.skuList

        prepared =
            model.newOrderPrepare

        _ =
            Debug.log "product id changed" value

        _ =
            Debug.log "filter: " (List.head skuAfterFilter)
    in
    ( { model
        | newOrderPrepare =
            { prepared
                | productId = value
                , sku = List.head skuAfterFilter
            }
      }
    , Cmd.none
    )


orderIdChanged : Model -> String -> ( Model, Cmd Msg )
orderIdChanged model value =
    let
        prepared =
            model.newOrderPrepare
    in
    ( { model | newOrderPrepare = { prepared | orderId = Just value } }, Cmd.none )


productCountInOrderChanged : Model -> String -> ( Model, Cmd Msg )
productCountInOrderChanged model value =
    let
        count =
            Maybe.withDefault 1 (String.toInt value)

        prepared =
            model.newOrderPrepare
    in
    ( { model | newOrderPrepare = { prepared | count = count } }, Cmd.none )


addNewItemToOrder : Model -> ( Model, Cmd Msg )
addNewItemToOrder model =
    let
        prepare =
            model.time
                |> Maybe.withDefault (Time.millisToPosix 10000000)
                |> saveItemToOrderPrepare model.newOrderPrepare
    in
    ( { model | newOrderPrepare = prepare }, Cmd.none )


createOrder : Model -> ( Model, Cmd Msg )
createOrder model =
    if List.length model.newOrderPrepare.itemsForNewOrder <= 0 then
        ( model, Cmd.none )

    else
        case model.newOrderPrepare.orderId of
            Nothing ->
                ( model, Cmd.none )

            Just id ->
                let
                    prepared =
                        model.newOrderPrepare

                    newOrder =
                        Order.NotSync
                            (Order.Order
                                id
                                prepared.itemsForNewOrder
                                prepared.xianwei
                                prepared.tongbozhi
                                prepared.luosi
                            )

                    _ =
                        Debug.log "create new order: " newOrder
                in
                ( { model
                    | orderList = newOrder :: model.orderList
                    , newOrderPrepare = emptyOrderPrepare
                  }
                , syncOrder newOrder
                )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


sendData : Model -> Cmd msg
sendData model =
    case model.page of
        PageMain ->
            setupDataTable (TableData [] [] "")

        PageAddOrder ->
            setupDataTable (TableData [] [] "")

        PageOrderList ->
            setupDataTable (TableData orderColumnNames (ordersDataToArray model.orderList) "order")

        PageSkuList ->
            setupDataTable (TableData skuColumnNames (skuDataToArray model.skuList) "sku")


port setupDataTable : TableData -> Cmd msg
