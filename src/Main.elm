port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, input, label, option, select, text)
import Html.Attributes exposing (class, for, selected, src, step, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Svg exposing (Svg, g, path, rect, svg)
import Svg.Attributes
    exposing
        ( d
        , fill
        , fontFamily
        , fontSize
        , height
        , id
        , stroke
        , strokeLinecap
        , strokeWidth
        , textAnchor
        , transform
        , viewBox
        , width
        , x
        , y
        )
import Url exposing (Url, fromString)
import Url.Builder exposing (relative)
import Url.Parser exposing (query)
import Url.Parser.Query as Query



---- MODEL ----


type alias Model =
    { label : String
    , width : Int
    , height : Int
    , pins : List Pin
    }


type alias Pin =
    { label : String
    , side : Side
    , position : Int
    }


type Side
    = Top
    | Bottom
    | Left
    | Right


defaultModel : Model
defaultModel =
    { label = "Part Label"
    , width = 500
    , height = 300
    , pins =
        [ { label = "VCC", side = Left, position = 100 }
        , { label = "GND", side = Left, position = 200 }
        ]
    }


urlToModel : Url -> Model
urlToModel url =
    let
        query =
            Query.map4 Model
                (Query.string "label" |> Query.map (Maybe.withDefault defaultModel.label))
                (Query.int "width" |> Query.map (Maybe.withDefault defaultModel.width))
                (Query.int "height" |> Query.map (Maybe.withDefault defaultModel.height))
                (Query.map3 (List.map3 Pin)
                    (Query.custom "pinLabel" identity)
                    (Query.custom "pinSide" <| List.filterMap sideFromString)
                    (Query.custom "pinPosition" <| List.filterMap String.toInt)
                )
    in
    Url.Parser.parse (Url.Parser.query query) url |> Maybe.withDefault defaultModel


urlFromModel : Model -> String
urlFromModel model =
    Url.Builder.relative []
        ([ Url.Builder.string "label" model.label
         , Url.Builder.int "width" model.width
         , Url.Builder.int "height" model.height
         ]
            ++ (model.pins
                    |> List.map
                        (\pin ->
                            [ Url.Builder.string "pinLabel" pin.label
                            , Url.Builder.string "pinPosition" <| String.fromInt pin.position
                            , Url.Builder.string "pinSide" <| sideToString pin.side
                            ]
                        )
                    |> List.concat
               )
        )


init : String -> ( Model, Cmd Msg )
init locationHref =
    let
        maybeUrl =
            Url.fromString locationHref

        model =
            case ( maybeUrl, Maybe.map .query maybeUrl ) of
                ( Just url, Just (Just _) ) ->
                    urlToModel url

                _ ->
                    defaultModel
    in
    ( model, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateLabel String
    | UpdateWidth (Maybe Int)
    | UpdateHeight (Maybe Int)
    | AddPin
    | RemovePin
    | UpdatePinLabel Int String
    | UpdatePinSide Int (Maybe Side)
    | UpdatePinPosition Int (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLabel label ->
            ( { model | label = label }, Cmd.none )

        UpdateWidth (Just width) ->
            ( { model | width = width }, Cmd.none )

        UpdateWidth Nothing ->
            ( model, Cmd.none )

        UpdateHeight (Just height) ->
            ( { model | height = height }, Cmd.none )

        UpdateHeight Nothing ->
            ( model, Cmd.none )

        AddPin ->
            let
                pin =
                    case List.reverse model.pins of
                        [] ->
                            { label = "New", side = Top, position = 100 }

                        last :: _ ->
                            { last | label = "New", position = last.position + 100 }

                ( width, height ) =
                    if pin.side == Top || pin.side == Bottom then
                        ( max model.width (pin.position + 100), model.height )

                    else
                        ( model.width, max model.height (pin.position + 100) )
            in
            ( { model | pins = model.pins ++ [ pin ], width = width, height = height }, Cmd.none )

        RemovePin ->
            ( { model | pins = List.Extra.removeAt (List.length model.pins - 1) model.pins }, Cmd.none )

        UpdatePinLabel index label ->
            ( { model
                | pins = List.Extra.updateAt index (\pin -> { pin | label = label }) model.pins
              }
            , Cmd.none
            )

        UpdatePinSide index (Just side) ->
            ( { model
                | pins = List.Extra.updateAt index (\pin -> { pin | side = side }) model.pins
              }
            , Cmd.none
            )

        UpdatePinSide _ Nothing ->
            ( model, Cmd.none )

        UpdatePinPosition index (Just position) ->
            let
                clampPosition pin pos =
                    if pin.side == Top || pin.side == Bottom then
                        clamp 100 (model.width - 100) pos

                    else
                        clamp 100 (model.height - 100) pos
            in
            ( { model
                | pins = List.Extra.updateAt index (\pin -> { pin | position = clampPosition pin position }) model.pins
              }
            , Cmd.none
            )

        UpdatePinPosition _ Nothing ->
            ( model, Cmd.none )


port pushUrl : String -> Cmd msg


updateWithUrl : Msg -> Model -> ( Model, Cmd Msg )
updateWithUrl msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel

        url =
            urlFromModel newModel
    in
    ( newModel
    , Cmd.batch [ pushUrl url, cmds ]
    )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Schematic Generator" ]
        , sectionDiv "Part Properties"
            [ div [ class "inputline" ]
                [ label [ for "label" ] [ text "Label" ]
                , input [ id "label", type_ "text", value model.label, onInput UpdateLabel ] []
                ]
            , div [ class "inputline" ]
                [ label [ for "width" ] [ text "Size" ]
                , div [ class "inputgroup" ]
                    [ milInput UpdateWidth model.width
                    , text "x"
                    , milInput UpdateHeight model.height
                    , text "in"
                    ]
                ]
            ]
        , sectionDiv "Pins" <|
            List.concat
                [ List.indexedMap viewPinInput model.pins
                , [ div [ class "inputline" ]
                        [ input [ type_ "button", value "+", onClick AddPin ] []
                        , input [ type_ "button", value "-", onClick RemovePin ] []
                        ]
                  ]
                ]
        , sectionDiv "Preview" [ viewSvg model ]
        ]


viewPinInput : Int -> Pin -> Html Msg
viewPinInput index pin =
    div [ class "inputline" ]
        [ label [] [ text <| "Pin " ++ String.fromInt (index + 1) ]
        , input
            [ value pin.label
            , onInput (UpdatePinLabel index)
            ]
            []
        , select
            [ value <| sideToString pin.side
            , onInput (sideFromString >> UpdatePinSide index)
            ]
          <|
            List.map
                (\side ->
                    option
                        [ value <| sideToString side, selected <| side == pin.side ]
                        [ text <| sideToString side ]
                )
                [ Top, Bottom, Left, Right ]
        , div [ class "inputgroup" ]
            [ milInput (UpdatePinPosition index) pin.position
            , text "in"
            ]
        ]


milInput : (Maybe Int -> msg) -> Int -> Html msg
milInput milUpdate milValue =
    input
        [ type_ "number"
        , step ".1"
        , value <| String.fromFloat <| milToInch milValue
        , onInput (String.toFloat >> Maybe.map milFromInch >> milUpdate)
        ]
        []


sectionDiv : String -> List (Html Msg) -> Html Msg
sectionDiv title children =
    div [ class "section" ] <| div [ class "title" ] [ text title ] :: children


milFromInch : Float -> Int
milFromInch inch =
    round (inch * 1000)


milToInch : Int -> Float
milToInch mil =
    toFloat mil / 1000


viewSvg : Model -> Svg Msg
viewSvg model =
    let
        viewWidth =
            model.width + getMargin Left model.pins + getMargin Right model.pins

        viewHeight =
            model.height + getMargin Top model.pins + getMargin Bottom model.pins
    in
    svg
        [ width <| (String.fromFloat <| toFloat viewWidth / 1000) ++ "in"
        , height <| (String.fromFloat <| toFloat viewHeight / 1000) ++ "in"
        , viewBox <| "0 0 " ++ String.fromInt viewWidth ++ " " ++ String.fromInt viewHeight
        ]
        [ g
            [ id "schematic"
            , transform <| "translate(" ++ String.fromInt (getMargin Left model.pins) ++ ", " ++ String.fromInt (getMargin Top model.pins) ++ ")"
            , fill "none"
            , stroke "none"
            , strokeWidth "10"
            , strokeLinecap "round"
            , fontFamily "DroidSans"
            ]
            ([ rect
                [ x "0"
                , y "0"
                , width <| String.fromInt model.width
                , height <| String.fromInt model.height
                , stroke "#000"
                ]
                []
             , Svg.text_
                [ x <| String.fromInt (model.width // 2)
                , y <| String.fromInt (model.height // 2 + 20)
                , fontSize "59"
                , fill "#000"
                , textAnchor "middle"
                ]
                [ Svg.text model.label ]
             ]
                ++ List.indexedMap
                    (viewPinSvg ( model.width, model.height ))
                    model.pins
            )
        ]


getMargin : Side -> List Pin -> Int
getMargin side pins =
    if List.any (\pin -> pin.side == side) pins then
        105

    else
        5


viewPinSvg : ( Int, Int ) -> Int -> Pin -> Html Msg
viewPinSvg ( width_, height_ ) index pin =
    let
        ( x_, y_ ) =
            case pin.side of
                Top ->
                    ( pin.position, 0 )

                Bottom ->
                    ( pin.position, height_ )

                Left ->
                    ( 0, pin.position )

                Right ->
                    ( width_, pin.position )
    in
    g
        [ transform <| "translate(" ++ String.fromInt x_ ++ "," ++ String.fromInt y_ ++ ")" ]
        [ path
            [ d <| sideToPath pin.side
            , id <| "connector" ++ String.fromInt index ++ "pin"
            , stroke "#555"
            ]
            []
        , rect
            [ x "-1"
            , y "-1"
            , width "2"
            , height "2"
            , id <| "connector" ++ String.fromInt index ++ "terminal"
            , fill "none"
            , stroke "none"
            ]
            []
        , Svg.text_
            (sideToSvgTextAttributes pin.side 20
                ++ [ y "15"
                   , fontSize "49"
                   , fill "#555"
                   ]
            )
            [ Svg.text pin.label ]
        , Svg.text_
            (sideToSvgTextAttributes pin.side -50
                ++ [ y "-15"
                   , fontSize "35"
                   , fill "#555"
                   , textAnchor "middle"
                   ]
            )
            [ Svg.text <| String.fromInt (index + 1) ]
        ]


sideToSvgTextAttributes : Side -> Float -> List (Svg.Attribute msg)
sideToSvgTextAttributes side offset =
    case side of
        Top ->
            [ transform "rotate(-90)", x <| String.fromFloat -offset, textAnchor "end" ]

        Bottom ->
            [ transform "rotate(-90)", x <| String.fromFloat offset, textAnchor "start" ]

        Left ->
            [ x <| String.fromFloat offset, textAnchor "start" ]

        Right ->
            [ x <| String.fromFloat -offset, textAnchor "end" ]


sideToPath : Side -> String
sideToPath side =
    case side of
        Top ->
            "m0,0 v-100"

        Bottom ->
            "m0,0 v100"

        Left ->
            "m0,0 h-100"

        Right ->
            "m0,0 h100"


sideToString : Side -> String
sideToString side =
    case side of
        Top ->
            "top"

        Bottom ->
            "bottom"

        Left ->
            "left"

        Right ->
            "right"


sideFromString : String -> Maybe Side
sideFromString side =
    case side of
        "top" ->
            Just Top

        "bottom" ->
            Just Bottom

        "left" ->
            Just Left

        "right" ->
            Just Right

        _ ->
            Nothing



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = updateWithUrl
        , subscriptions = always Sub.none
        }
