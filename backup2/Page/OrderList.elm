module Page.OrderList exposing
    ( Model
    , Msg
    , orderListTableView
    )

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
import Http exposing (..)
import Order exposing (..)
import RemoteData exposing (WebData)


type alias Model =
    { orderList : List Order.StatusOrder
    }


type Msg
    = ShowOrderList
    | SyncOrderResponse (Result Http.Error String)
    | OrderDeleted (Result Http.Error String)
    | OrderDataReceived (WebData (List Order.Order))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OrderDeleted _ ->
            ( model, Cmd.none )

        ShowOrderList ->
            toShowOrderListPage model


toShowOrderListPage : Model -> ( Model, Cmd Msg )
toShowOrderListPage model =
    let
        m =
            { model | page = PageOrderList }

        _ =
            Debug.log "order list: " model.orderList
    in
    ( m, Cmd.none )


deleteOrder : String -> Model -> ( Model, Cmd Msg )
deleteOrder id model =
    ( model, setupDeleteOrder id )


orderListTableView : List Order.StatusOrder -> Html Msg
orderListTableView orderList =
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
        , tbody [] (List.map orderItemTableBody orderList)
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


fetchOrders : Cmd Msg
fetchOrders =
    Http.get
        { url = "http://localhost:5019/order-list"
        , expect =
            Decode.list Order.orderDecoder
                |> Http.expectJson (RemoteData.fromResult >> OrderDataReceived)
        }
