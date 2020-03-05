module App exposing (main)

import Browser exposing (..)
import Debug exposing (..)
import Delay exposing (..)
import File exposing (File)
import File.Select as Select
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
import Model exposing (..)
import Order exposing (..)
import OrderPrepare exposing (..)
import Page.NewOrder exposing (..)
import Page.SkuList exposing (..)
import RemoteData exposing (WebData)
import Sku exposing (..)
import Task exposing (..)
import Time exposing (Posix, every)


view : Model -> Html Msg
view model =
    topLayout model


topLayout : Model -> Html Msg
topLayout model =
    div
        [ class "ui grid" ]
        [ columnsView model
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
    div [ class "three column row" ]
        [ div [ class "two wide column" ]
            [ menuView model
            ]
        , div [ class "twelve wide column" ]
            [ mainContentView model
            ]
        , div [ class "two wide column" ] []
        ]


mainContentView : Model -> Html Msg
mainContentView model =
    case model.page of
        PageHome ->
            div
                [ class "main-content" ]
                [ div [ class "main-content-title" ]
                    [ div
                        [ class "ui medium header " ]
                        [ text "版本" ]
                    , div [] [ text model.version ]
                    ]
                ]

        PageSkuList ->
            div
                [ class "main-content" ]
                [ div [ class "main-content-title" ]
                    [ div
                        [ class "ui medium header " ]
                        [ text "产品列表" ]
                    ]
                , skuListTableView model
                ]

        PageAddOrder ->
            div
                [ class "main-content" ]
                [ div [ class "main-content-title" ]
                    [ div
                        [ class "ui medium header " ]
                        [ text "新配货单" ]
                    ]
                , inputControlsForAddOrder model
                ]


menuView : Model -> Html Msg
menuView model =
    div [ class "left-main-menu" ]
        [ div [ class "ui secondary vertical pointing menu" ]
            [ div [ class "item" ]
                [ a [ class "ui logo icon image" ]
                    [ img [ src "logo.png", id "menu-logo" ] []
                    ]
                ]
            , div [ class "ui divider" ] []
            , a [ class "item ui small header", onClick ToAddOrderPage ] [ text "添加配货单" ]
            , a [ class "item ui small header", onClick ToShowSkuListPage ] [ text "产品列表" ]
            ]
        ]


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags, Cmd.batch [] )


initialModel : Decode.Value -> Model
initialModel skuRaw =
    let
        skuList =
            case Decode.decodeValue Decode.string skuRaw of
                Err info ->
                    let
                        _ =
                            Debug.log "parse failed for " info
                    in
                    []

                Ok jsonRawString ->
                    decodeSkuListFromString jsonRawString
    in
    { orderList = []
    , skuList = skuList
    , newOrderPrepare = initOrderPrepare
    , page = PageHome
    , giftList = defaultGiftList
    , time = Nothing
    , autoClearProducts = True
    , version = "2020.3.3.1"
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

        ResetAutoClearProductsInOrderAfterPrint value ->
            resetAutoClearProductsInOrderAfterPrint value model

        DeleteOrderItem code ->
            deleteOrderItem code model

        SelectGift v ->
            let
                prepare =
                    model.newOrderPrepare
            in
            ( { model | newOrderPrepare = { prepare | currentGift = Just v } }, Cmd.none )

        AddGiftToOrder ->
            addGiftToOrder model

        DeleteGift name ->
            deleteGift name model

        PrintOrder ->
            handlePrintOrder model

        ResetOrder ->
            resetOrder model

        ToShowSkuListPage ->
            ( { model | page = PageSkuList }, Cmd.none )

        SkuDataReceived response ->
            skuDataReceived model response

        SearchProductById value ->
            searchProductById model value

        ProductCountInOrderChanged value ->
            productCountInOrderChanged model value

        CsvRequested ->
            ( model
            , Select.file [ "text/csv" ] CsvSelected
            )

        CsvSelected file ->
            ( model
            , Task.perform CsvLoaded (File.toString file)
            )

        CsvLoaded content ->
            parseSkuListInCsv content model

        PatchPostSkuResponse response ->
            patchPostSkuResponse response model


updateTick : Time.Posix -> Model -> ( Model, Cmd Msg )
updateTick posix model =
    ( model, Cmd.none )


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
    ( m, Cmd.none )


toAddOrderPage : Model -> ( Model, Cmd Msg )
toAddOrderPage model =
    let
        _ =
            Debug.log "orders: " model.orderList
    in
    ( { model | page = PageAddOrder, newOrderPrepare = initOrderPrepare }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
