module Page.SkuList exposing
    ( fetchSkuListFromServer
    , parseSkuListInCsv
    , patchPostSkuResponse
    , skuListTableView
    )

import Csv exposing (..)
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
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as Encode exposing (list)
import Model exposing (..)
import Ports exposing (..)
import RemoteData exposing (WebData)
import Sku exposing (..)


skuListTableView : Model -> Html Msg
skuListTableView model =
    div [ style "margin-top" "40px" ]
        [ button [ onClick CsvRequested ] [ text "批量导入SKU" ]
        , table
            [ class "striped", style "width" "100%" ]
            [ thead []
                [ tr []
                    [ th [] [ text "编码" ]
                    , th [] [ text "名称" ]
                    , th [] [ text "属性" ]
                    , th [] [ text "单位" ]
                    ]
                ]
            , tbody [] (List.map skuItemTableBody model.skuList)
            ]
        ]


skuItemTableBody : Sku -> Html Msg
skuItemTableBody sku =
    tr []
        [ td [] [ text sku.code ]
        , td [] [ text sku.name ]
        , td [] [ text sku.attr ]
        , td [] [ text sku.measure ]
        ]


parseSkuListInCsv : String -> Model -> ( Model, Cmd Msg )
parseSkuListInCsv content model =
    let
        _ =
            Debug.log "csv content:" content

        result =
            Csv.parse content

        _ =
            Debug.log "parse result: " result

        skuList =
            case result of
                Ok obj ->
                    List.filterMap skuFromStringArray obj.records

                Err _ ->
                    []

        _ =
            Debug.log "convert to sku: " skuList
    in
    ( { model | skuList = skuList }, saveSkuListToLocalStorage skuList )


patchPostSkuResponse : Result Http.Error String -> Model -> ( Model, Cmd Msg )
patchPostSkuResponse result model =
    case result of
        Err info ->
            let
                _ =
                    Debug.log "batch post sku failed: " info
            in
            ( model, Cmd.none )

        Ok _ ->
            ( model, Cmd.none )



-- server version


fetchSkuListFromServer : Cmd Msg
fetchSkuListFromServer =
    Http.get
        { url = "http://localhost:5019/sku-list"
        , expect =
            Decode.list skuDecoder
                |> Http.expectJson (RemoteData.fromResult >> SkuDataReceived)
        }


postSkuListToServer : List Sku -> Cmd Msg
postSkuListToServer skuList =
    Http.post
        { url = "http://localhost:5019/sku-list"
        , body =
            skuList
                |> Encode.list skuEncoder
                |> Http.jsonBody
        , expect = Http.expectString PatchPostSkuResponse
        }


saveSkuListToLocalStorage : List Sku -> Cmd Msg
saveSkuListToLocalStorage skuList =
    Encode.list skuEncoder skuList
        |> Encode.encode 0
        |> Ports.storeSkuList


fetchSkuListFromLocalStorage : Cmd Msg
fetchSkuListFromLocalStorage =
    Http.get
        { url = "http://localhost:5019/sku-list"
        , expect =
            Decode.list skuDecoder
                |> Http.expectJson (RemoteData.fromResult >> SkuDataReceived)
        }
