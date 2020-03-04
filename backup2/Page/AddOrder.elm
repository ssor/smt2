module Page.AddOrder exposing
    ( Model
    , Msg
    , inputControlsForAddOrder
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
import Html.Events exposing (onCheck, onClick, onInput)
import Order exposing (..)
import OrderPrepare exposing (..)
import Sku exposing (..)


type alias Model =
    { newOrderPrepare : OrderPrepare
    , skuList : List Sku
    }


type Msg
    = ShowEmptyOrder
    | AddNewItemToOrder
    | CreateOrder
    | SearchProductById String
    | OrderIdChanged String
    | ProductCountInOrderChanged String
    | ToShowSkuListPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Tick posix ->
        --     updateTick posix model
        ShowEmptyOrder ->
            toAddOrderPage model

        AddNewItemToOrder ->
            addNewItemToOrder model

        CreateOrder ->
            createOrder model

        SearchProductById value ->
            searchProductById model value

        OrderIdChanged value ->
            orderIdChanged model value

        ProductCountInOrderChanged value ->
            productCountInOrderChanged model value

        ToShowSkuListPage ->
            ( model, Cmd.none )


orderIdChanged : Model -> String -> ( Model, Cmd Msg )
orderIdChanged model value =
    let
        prepared =
            model.newOrderPrepare
    in
    ( { model | newOrderPrepare = prepared }, Cmd.none )


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
    ( model, Cmd.none )


createOrder : Model -> ( Model, Cmd Msg )
createOrder model =
    if List.length model.newOrderPrepare.itemsForNewOrder <= 0 then
        ( model, Cmd.none )

    else
        let
            prepared =
                model.newOrderPrepare

            _ =
                Debug.log "create new order: " prepared
        in
        ( { model
            | newOrderPrepare = emptyOrderPrepare
          }
        , Cmd.none
        )


toAddOrderPage : Model -> ( Model, Cmd Msg )
toAddOrderPage model =
    ( { model | newOrderPrepare = emptyOrderPrepare }, Cmd.none )


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


topLayout : Model -> Html Msg
topLayout model =
    div
        []
        [ navView model
        , columnsView model
        ]


menuView : Model -> Html Msg
menuView model =
    div [ class "column is-2" ]
        [ aside
            [ class "menu bd-side", style "padding-left" "20px" ]
            [ p [ class "menu-label" ] [ text "配货单管理" ]
            , ul [ class "menu-list" ]
                [ li []
                    [ a [ onClick ShowEmptyOrder ] [ text "添加配货单" ]
                    ]
                ]
            , p [ class "menu-label" ] [ text "产品管理" ]
            , ul [ class "menu-list" ]
                [ li []
                    [ a [ onClick ToShowSkuListPage ] [ text "产品列表" ] ]
                ]
            ]
        ]


navView : Model -> Html Msg
navView model =
    nav
        [ class "navbar is-spaced has-shadow" ]
        [ div
            [ class "navbar-brand" ]
            [ a
                [ class "navbar-item"
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
    div
        [ class "column", style "padding-left" "solid 1px rgba(100,100,100,0.2)" ]
        [ h1
            [ class "title is-4" ]
            [ text "新配货单" ]
        , inputControlsForAddOrder model.newOrderPrepare model.skuList
        ]


inputControlsForAddOrder : OrderPrepare -> List Sku -> Html Msg
inputControlsForAddOrder newOrderPrepare skuList =
    let
        _ =
            Debug.log "fill control with items: " skuList
    in
    div
        [ class "container is-fullhd" ]
        [ div
            [ class "notification" ]
            [ inputForOrderItems newOrderPrepare
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
                , hr [ style "border-top" "1px solid #bbb" ] []
                , br [] []
                , orderItemsTable prepare
                ]


orderItemsTable : OrderPrepare -> Html msg
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


inputForOrderItem : Order.Item -> Html msg
inputForOrderItem item =
    tr []
        [ td [] [ text item.sku.code ]
        , td [] [ text item.sku.name ]
        , td [] [ text item.sku.attr ]
        , td [] [ text (String.fromInt item.count) ]
        ]


inputProductName : String -> Html msg
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


inputProductAttr : String -> Html msg
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
