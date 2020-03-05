module Page.NewOrder exposing
    ( addGiftToOrder
    , addNewItemToOrder
    , deleteGift
    , deleteOrderItem
    , handlePrintOrder
    , inputControlsForAddOrder
    , productCountInOrderChanged
    , resetAutoClearProductsInOrderAfterPrint
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
        , tabindex
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
    if model.autoClearProducts then
        ( { model | newOrderPrepare = initOrderPrepare }, printOrder (tableHead ++ tableBody ++ spans) )

    else
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
    in
    ( { model | newOrderPrepare = { prepare | giftList = newGiftList } }, Cmd.none )


addNewItemToOrder : Model -> ( Model, Cmd Msg )
addNewItemToOrder model =
    let
        prepare =
            saveItemToOrderPrepare model.newOrderPrepare
    in
    ( { model | newOrderPrepare = prepare }, Cmd.none )



-- ( model, Cmd.none )
-- View


inputControlsForAddOrder : Model -> Html Msg
inputControlsForAddOrder model =
    div []
        [ inputForOrderItems model
        , br [] []
        , orderItemsTable model.newOrderPrepare model.autoClearProducts
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
                ]

        Just sku ->
            div []
                [ inputProduct prepare.productId sku.name sku.attr prepare.count
                ]


inputGift : List String -> Html Msg
inputGift values =
    fieldset []
        [ legend [] [ text "赠品" ]
        , label [] [ text "列表" ]
        , select [ onInput SelectGift, style "width" "203px" ]
            (values |> List.map toOption)
        , button
            [ class "ui button"
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
        [ div [ class "ui form" ]
            [ h4 [] [ text "添加SKU" ]
            , div [ class "ui segment" ]
                [ div [ class "two fields" ]
                    [ div [ class "field" ]
                        [ label [] [ text "编码" ]
                        , input
                            [ class "input"
                            , type_ "text"
                            , onInput SearchProductById
                            ]
                            []
                        ]
                    , div [ class "field" ]
                        [ label [] [ text "数量" ]
                        , input
                            [ class "input"
                            , type_ "number"
                            , value (String.fromInt count)
                            , onInput ProductCountInOrderChanged
                            ]
                            []
                        ]
                    ]
                , button
                    [ class "ui button"
                    ]
                    [ text "添加" ]
                ]
            ]

        -- , div [ class "ui divider" ] []
        ]


inputProduct : String -> String -> String -> Int -> Html Msg
inputProduct id name attr count =
    div []
        [ div [ class "ui form" ]
            [ h4 [] [ text "添加SKU" ]
            , div [ class "ui segment" ]
                [ div [ class "two fields" ]
                    [ div [ class "field" ]
                        [ label [] [ text "编码" ]
                        , input [ class "input", type_ "text", onInput SearchProductById, value id ] []
                        ]
                    , div [ class "field" ]
                        [ label [] [ text "数量" ]
                        , input
                            [ class "input"
                            , type_ "number"
                            , value (String.fromInt count)
                            , onInput ProductCountInOrderChanged
                            ]
                            []
                        ]
                    ]
                , div [ class "ui green message" ]
                    [ text (name ++ "-" ++ attr) ]
                , button
                    [ class "ui button"
                    , onClick AddNewItemToOrder

                    --  onClick PrintOrder
                    ]
                    [ text "添加" ]
                ]
            ]
        ]


resetAutoClearProductsInOrderAfterPrint : Bool -> Model -> ( Model, Cmd Msg )
resetAutoClearProductsInOrderAfterPrint value model =
    let
        _ =
            Debug.log "reset auto clear product: " value
    in
    ( { model | autoClearProducts = value }, Cmd.none )


orderItemsTable : OrderPrepare -> Bool -> Html Msg
orderItemsTable prepare autoClearProducts =
    div []
        [ div []
            [ h4 [] [ text "配货单列表" ]
            ]
        , div [ class "ui segment" ]
            [ button
                [ class "ui button"
                , onClick PrintOrder
                ]
                [ text "打印" ]
            , button
                [ class "ui button"
                , onClick ResetOrder
                ]
                [ text "清空" ]
            , input
                [ type_ "checkbox"
                , class "hidden"
                , tabindex 0
                , id "check-auto-clear-products"
                , checked autoClearProducts
                , onCheck ResetAutoClearProductsInOrderAfterPrint
                ]
                []
            , label [] [ text "自动清空" ]
            ]
        , div [ id "mytable" ]
            [ table [ class "ui striped table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "商品编码" ]
                        , th [] [ text "名称" ]
                        , th [] [ text "属性" ]
                        , th [] [ text "数量" ]
                        , th [] [ text "" ]
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
        [ td [] [ text item.sku.code ]
        , td [] [ text item.sku.name ]
        , td [] [ text item.sku.attr ]
        , td [] [ text (String.fromInt item.count) ]
        , td
            [ onClick (DeleteOrderItem item.sku.code)
            , style "text-decoration" "underline"
            ]
            [ text "删除" ]
        ]


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
