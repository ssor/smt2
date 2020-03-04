module Page.NewOrder exposing
    ( addGiftToOrder
    , addNewItemToOrder
    , deleteGift
    , deleteOrderItem
    , handlePrintOrder
    , inputControlsForAddOrder
    , productCountInOrderChanged
    , resetOrder
    , searchProductById
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
import Model exposing (..)
import Order exposing (..)
import OrderPrepare exposing (..)
import Ports exposing (..)
import Sku exposing (..)
import TableData exposing (..)



-- Update


resetOrder : Model -> ( Model, Cmd Msg )
resetOrder model =
    ( { model | newOrderPrepare = initOrderPrepare }, Cmd.none )


handlePrintOrder : Model -> ( Model, Cmd Msg )
handlePrintOrder model =
    let
        tableHead =
            """
        <table>
                    <thead>
                        <tr>
                            <th class="product-code">商家编码</th>
                            <th class="product-name">产品名称</th>
                            <th class="product-attr">属性</th>
                            <th class="product-count">数量</th>
                            <th class="product-measure">单位</th>
                        </tr>
                    </thead>
        """

        tdg s styleOption =
            case styleOption of
                Nothing ->
                    "<td>" ++ s ++ "</td>"

                Just st ->
                    "<td " ++ st ++ ">" ++ s ++ "</td>"

        trGenerator item =
            "<tr>"
                ++ tdg item.sku.code Nothing
                ++ tdg item.sku.name Nothing
                ++ tdg item.sku.attr Nothing
                ++ tdg (String.fromInt item.count) (Just """ class="count" """)
                ++ tdg item.sku.measure Nothing
                ++ "</tr>"

        tableBody =
            model.newOrderPrepare.itemsForNewOrder
                |> List.map trGenerator
                |> String.join " "

        spans =
            """
        </table>
                <div>
                    <span class="xianweibu">纤维布</span>
                    <span class="tongbozhi">铜箔纸包装</span>
                    <span class="jiachangluosi">加长螺丝</span>
                    <span class="peihuoyuan">配货员</span>
                    <span class="shenheyuan">审核员</span>
                </div>
        """
    in
    ( model, printOrder (tableHead ++ tableBody ++ spans) )


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


productCountInOrderChanged : Model -> String -> ( Model, Cmd Msg )
productCountInOrderChanged model value =
    let
        count =
            Maybe.withDefault 1 (String.toInt value)

        prepared =
            model.newOrderPrepare
    in
    ( { model | newOrderPrepare = { prepared | count = count } }, Cmd.none )


addGiftToOrder : Model -> ( Model, Cmd Msg )
addGiftToOrder model =
    let
        prepare =
            model.newOrderPrepare

        newGiftList =
            if List.member prepare.currentGift prepare.giftList == False then
                prepare.currentGift :: prepare.giftList

            else
                prepare.giftList

        _ =
            Debug.log "current gift" prepare.currentGift

        _ =
            Debug.log "new gift list: " newGiftList
    in
    ( { model | newOrderPrepare = { prepare | giftList = newGiftList } }, Cmd.none )


addNewItemToOrder : Model -> ( Model, Cmd Msg )
addNewItemToOrder model =
    let
        prepare =
            saveItemToOrderPrepare model.newOrderPrepare
    in
    ( { model | newOrderPrepare = prepare }, Cmd.none )



-- View


inputControlsForAddOrder : Model -> Html Msg
inputControlsForAddOrder model =
    div []
        [ inputForOrderItems model
        , br [] []
        , orderItemsTable model.newOrderPrepare
        ]


inputForOrderItems : Model -> Html Msg
inputForOrderItems model =
    let
        prepare =
            model.newOrderPrepare
    in
    case prepare.sku of
        Nothing ->
            div []
                [ inputProductEmpty 1
                , inputGift model.giftList
                ]

        Just sku ->
            div []
                [ inputProduct prepare.productId sku.name sku.attr prepare.count
                , inputGift model.giftList
                ]


inputGift : List String -> Html Msg
inputGift values =
    fieldset []
        [ legend [] [ text "赠品" ]
        , label [] [ text "列表" ]
        , select [ onInput SelectGift, style "width" "203px" ]
            (values |> List.map toOption)
        , button
            [ class "primary small"
            , style "min-width" "80px"
            , onClick AddGiftToOrder
            ]
            [ text "添加" ]
        ]


toOption : String -> Html Msg
toOption v =
    option [ value v ] [ text v ]


inputProductEmpty : Int -> Html Msg
inputProductEmpty count =
    div []
        [ fieldset []
            [ legend [] [ text "SKU信息" ]
            , label [] [ text "编码" ]
            , input [ class "input", type_ "text", onInput SearchProductById ] []
            , label [] [ text "数量" ]
            , input
                [ class "input"
                , type_ "number"
                , value (String.fromInt count)
                , onInput ProductCountInOrderChanged
                ]
                []
            , button
                [ class "primary small"
                , style "min-width" "80px"
                , onClick AddNewItemToOrder
                ]
                [ text "添加" ]
            ]
        ]


inputProduct : String -> String -> String -> Int -> Html Msg
inputProduct id name attr count =
    div [ class "pure-form" ]
        [ fieldset []
            [ legend [] [ text "SKU信息" ]
            , label [] [ text "编码" ]
            , input [ class "input", type_ "text", onInput SearchProductById, value id ] []
            , label [] [ text "数量" ]
            , input
                [ class "input"
                , type_ "number"
                , value (String.fromInt count)
                , onInput ProductCountInOrderChanged
                ]
                []
            , button
                [ class "primary small"
                , style "min-width" "80px"
                , onClick AddNewItemToOrder
                ]
                [ text "添加" ]
            , br [] []
            , div [ style "margin" "5px 0px 12px 50px" ]
                [ span
                    [ style "font-size" "small"
                    , style "color" "blue"
                    ]
                    [ text (name ++ "-" ++ attr) ]
                ]
            ]
        ]


orderItemsTable : OrderPrepare -> Html Msg
orderItemsTable prepare =
    div []
        [ div []
            [ h4 [] [ text "配货单列表" ]
            ]
        , div []
            [ button
                [ class "tertiary small"
                , style "width" "80px"
                , onClick PrintOrder
                ]
                [ text "打印" ]
            , button
                [ class "secondary small"
                , style "width" "80px"
                , onClick ResetOrder
                ]
                [ text "清空" ]
            ]
        , div [ id "mytable" ]
            [ table [ class "table" ]
                [ --caption [] [ text "SKU列表" ]
                  thead []
                    [ tr []
                        [ th [ style "font-size" "14px" ]
                            [ text "商品编码" ]
                        , th [ style "font-size" "14px" ]
                            [ text "名称" ]
                        , th [ style "font-size" "14px" ]
                            [ text "属性" ]
                        , th [ style "font-size" "14px" ]
                            [ text "数量" ]
                        , th []
                            [ text "" ]
                        ]
                    ]
                , orderItemsTableBody prepare
                ]
            ]
        ]


orderItemsTableBody : OrderPrepare -> Html Msg
orderItemsTableBody prepare =
    let
        skuList =
            List.map inputForOrderItem prepare.itemsForNewOrder

        giftList =
            List.map giftItem prepare.giftList

        list =
            skuList ++ giftList
    in
    tbody [] list


inputForOrderItem : Order.Item -> Html Msg
inputForOrderItem item =
    tr []
        [ td [ style "font-size" "13px" ] [ text item.sku.code ]
        , td [ style "font-size" "13px" ] [ text item.sku.name ]
        , td [ style "font-size" "13px" ] [ text item.sku.attr ]
        , td [ style "font-size" "13px" ] [ text (String.fromInt item.count) ]
        , td
            [ onClick (DeleteOrderItem item.sku.code)
            , style "font-size" "13px"
            , style "text-decoration" "underline"
            ]
            [ text "删除" ]
        ]



-- giftTable : OrderPrepare -> Html Msg
-- giftTable prepare =
--     table [ class "table", style "width" "100%" ]
--         [ --caption [] [ text "赠品列表" ]
--           thead []
--             [ tr []
--                 [ th []
--                     [ abbr
--                         []
--                         [ text "名称" ]
--                     ]
--                 , th []
--                     [ abbr
--                         []
--                         [ text "属性" ]
--                     ]
--                 , th []
--                     [ abbr
--                         []
--                         [ text "管理" ]
--                     ]
--                 ]
--             ]
--         , tbody [] (List.map giftItem prepare.giftList)
--         ]


giftItem : Maybe String -> Html Msg
giftItem item =
    case item of
        Nothing ->
            tr [] []

        Just value ->
            tr []
                [ td [] []
                , td [ style "font-size" "13px" ] [ text value ]
                , td [ style "font-size" "13px" ] [ text "赠品" ]
                , td [] []
                , td
                    [ onClick (DeleteGift value)
                    , style "font-size" "13px"
                    , style "text-decoration" "underline"
                    ]
                    [ text "删除" ]
                ]


deleteGift : String -> Model -> ( Model, Cmd Msg )
deleteGift name model =
    let
        prepare =
            model.newOrderPrepare

        currentGiftList =
            prepare.giftList

        filter gift =
            case gift of
                Nothing ->
                    False

                Just value ->
                    if value == name then
                        False

                    else
                        True

        newGiftList =
            List.filter filter currentGiftList
    in
    ( { model | newOrderPrepare = { prepare | giftList = newGiftList } }, Cmd.none )


deleteOrderItem : String -> Model -> ( Model, Cmd Msg )
deleteOrderItem code model =
    let
        prepare =
            model.newOrderPrepare

        currentItems =
            prepare.itemsForNewOrder

        filter item =
            if item.sku.code == code then
                False

            else
                True

        newItemList =
            List.filter filter currentItems
    in
    ( { model | newOrderPrepare = { prepare | itemsForNewOrder = newItemList } }, Cmd.none )



-- inputProductAttr : String -> Html Msg
-- inputProductAttr attr =
--     div [ class "field is-horizontal" ]
--         [ div [ class "field-label is-small" ]
--             [ label [ class "label" ] [ text "" ]
--             ]
--         , div [ class "field-body" ]
--             [ div [ class "field" ]
--                 [ label [ class "label" ] [ text attr ]
--                 ]
--             ]
--         ]
-- inputProductName : String -> Html Msg
-- inputProductName name =
--     div [ class "field is-horizontal" ]
--         [ div [ class "field-label is-small" ]
--             [ label [ class "label" ] [ text "详细信息" ]
--             ]
--         , div [ class "field-body" ]
--             [ div [ class "field" ]
--                 [ label [ class "label" ] [ text name ]
--                 ]
--             ]
--         ]
-- inputProductCountInOrder : Int -> Html Msg
-- inputProductCountInOrder count =
--     div [ class "field is-horizontal" ]
--         [ div [ class "field-label is-normal" ]
--             [ label [ class "label" ] [ text "产品数量" ]
--             ]
--         , div [ class "field-body" ]
--             [ div [ class "field" ]
--                 [ div [ class "control" ]
--                     [ input
--                         [ class "input"
--                         , type_ "number"
--                         , value (String.fromInt count)
--                         , onInput ProductCountInOrderChanged
--                         ]
--                         []
--                     ]
--                 ]
--             ]
--         ]
-- inputProductId : String -> Html Msg
-- inputProductId code =
--     div [ class "field is-horizontal" ]
--         [ div [ class "field-label is-normal" ]
--             [ label [ class "label" ] [ text "商品编码" ]
--             ]
--         , div [ class "field-body" ]
--             [ div [ class "field" ]
--                 [ div [ class "control" ]
--                     [ input [ class "input", type_ "text", onInput SearchProductById, value code ]
--                         []
--                     ]
--                 ]
--             , div [ class "field" ]
--                 [ div [ class "control" ]
--                     [ button
--                         [ class "button is-info"
--                         , onClick AddNewItemToOrder
--                         ]
--                         [ text "添加" ]
--                     ]
--                 ]
--             ]
--         ]
-- inputCheckOptions : OrderPrepare -> Html Msg
-- inputCheckOptions prepare =
--     div [ class "field is-horizontal" ]
--         [ div [ class "field-label is-normal" ]
--             [ label [ class "label" ] [ text "赠品" ]
--             ]
--         , div [ class "field-body" ]
--             [ div
--                 [ class "field is-grouped" ]
--                 [ div [ class "control" ]
--                     [ label [ class "checkbox" ]
--                         [ input
--                             [ type_ "checkbox"
--                             , checked prepare.xianwei
--                             , onCheck (OpinionChanged Xianweibu)
--                             ]
--                             []
--                         , text "纤维布"
--                         ]
--                     ]
--                 , div [ class "control" ]
--                     [ label [ class "checkbox" ]
--                         [ input
--                             [ type_ "checkbox"
--                             , checked prepare.tongbozhi
--                             , onCheck (OpinionChanged Tongbozhi)
--                             ]
--                             []
--                         , text "铜箔纸包装"
--                         ]
--                     ]
--                 , div [ class "control" ]
--                     [ label [ class "checkbox" ]
--                         [ input
--                             [ type_ "checkbox"
--                             , checked prepare.luosi
--                             , onCheck (OpinionChanged Luosi)
--                             ]
--                             []
--                         , text "加长螺丝"
--                         ]
--                     ]
--                 ]
--             ]
--         ]
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
