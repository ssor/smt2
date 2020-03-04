module Page.Sku exposing
    ( Model
    , Msg
    , skuListTableView
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
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, requiredAt)
import RemoteData exposing (WebData)
import Sku exposing (..)


type alias Model =
    { skuList : List Sku
    }


type Msg
    = SkuDataReceived (WebData (List Sku))
    | ShowSkuList
    | ToAddOrderPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowSkuList ->
            ( model, fetchSkuList )

        SkuDataReceived response ->
            skuDataReceived model response

        ToAddOrderPage ->
            ( model, Cmd.none )


skuListTableView : List Sku -> Html Msg
skuListTableView skuList =
    table
        [ id "data-list", class "table", style "width" "100%" ]
        [ tr []
            [ th [] [ text "编码" ]
            , th [] [ text "名称" ]
            , th [] [ text "属性" ]
            ]
        , tbody [] (List.map skuItemTableBody skuList)
        ]


skuItemTableBody : Sku -> Html Msg
skuItemTableBody sku =
    tr []
        [ td [] [ text sku.code ]
        , td [] [ text sku.name ]
        , td [] [ text sku.attr ]
        ]


fetchSkuList : Cmd Msg
fetchSkuList =
    Http.get
        { url = "http://localhost:5019/sku-list"
        , expect =
            Decode.list skuDecoder
                |> Http.expectJson (RemoteData.fromResult >> SkuDataReceived)
        }


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


mainContentView : Model -> Html Msg
mainContentView model =
    div
        [ class "column", style "padding-left" "solid 1px rgba(100,100,100,0.2)" ]
        [ h1
            [ class "title is-4" ]
            [ text "产品列表" ]
        , skuListTableView model.skuList
        ]


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
                    [ a [ onClick ToAddOrderPage ] [ text "添加配货单" ]
                    ]
                ]
            , p [ class "menu-label" ] [ text "产品管理" ]
            , ul [ class "menu-list" ]
                [ li []
                    [ a [ onClick ShowSkuList ] [ text "产品列表" ] ]
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
