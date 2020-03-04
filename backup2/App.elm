module App exposing (main)

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
import OrderPrepare exposing (..)
import Page.AddOrder as PageAddOrder exposing (..)
import Page.Home as PageHome exposing (..)
import Page.Sku as PageSkuList exposing (..)
import RemoteData exposing (WebData)
import Sku exposing (..)
import TableData exposing (TableData)
import Task exposing (..)
import Time exposing (Posix, every)



-- type Model
-- = HomePage PageHome.Model
-- | AddOrderPage PageAddOrder.Model
-- | SkuListPage PageSkuList.Model


type alias Model =
    { orderList : List Order.StatusOrder
    , skuList : List Sku
    , page : Page
    , newOrderPrepare : OrderPrepare
    , time : Maybe Time.Posix
    }


type Page
    = PageOrderList
    | PageSkuList
    | PageAddOrder
    | PageMain



-- | Tick Time.Posix
-- | SkuDataReceived (WebData (List Sku))
-- | OrderDataReceived (WebData (List Order.Order))
-- | SyncOrderResponse (Result Http.Error String)
-- | OrderDeleted (Result Http.Error String)
-- | DeleteOrder String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ToShowHome subMsg, HomePage subModel ) ->
            PageHome.update subMsg subModel
                |> updateWith HomePage ToShowHome model

        ( ToShowSkuListPage subMsg, SkuListPage subModel ) ->
            ( model, Cmd.none )

        ( ToAddOrderPage subMsg, AddOrderPage subModel ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


view : Model -> Html Msg
view model =
    case model of
        HomePage subModel ->
            PageHome.view subModel
                |> Html.map ToShowHome

        SkuListPage subModel ->
            div [] []

        AddOrderPage subModel ->
            div [] []


init : () -> ( Model, Cmd Msg )
init _ =
    ( HomePage (PageHome.Model []), Cmd.none )


message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Time.every 1000 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
