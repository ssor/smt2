module Page.Home exposing
    ( Model
    , Msg
    , update
    , view
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
import Sku exposing (..)

type alias Model =
    { skuList : List Sku
    }


type Msg
    = ShowHomePage
    | ShowSkuListPage
    | ShowAddOrderPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowHomePage ->
            ( model, Cmd.none )

        ShowSkuListPage ->
            ( model, Cmd.none )

        ShowAddOrderPage ->
            ( model, Cmd.none )


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



-- type Page
--     = HomePage
--     | SkuListPage
--     | AddOrderPage


mainContentView : Model -> Html Msg
mainContentView model =
    div
        [ class "column", style "padding-left" "solid 1px rgba(100,100,100,0.2)" ]
        []


menuView : Model -> Html Msg
menuView model =
    div [ class "column is-2" ]
        [ aside
            [ class "menu bd-side", style "padding-left" "20px" ]
            [ p [ class "menu-label" ] [ text "配货单管理" ]
            , ul [ class "menu-list" ]
                [ li []
                    [ a [ onClick ShowAddOrderPage ] [ text "添加配货单" ]
                    ]
                ]
            , p [ class "menu-label" ] [ text "产品管理" ]
            , ul [ class "menu-list" ]
                [ li []
                    [ a [ onClick ShowSkuListPage ] [ text "产品列表" ] ]
                ]
            ]
        ]
