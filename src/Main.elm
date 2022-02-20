module Main exposing (main)

import Axis
import Browser exposing (Document)
import Browser.Events
import Color exposing (Color)
import Html exposing (a, button, text)
import Html.Events exposing (onClick)
import Interpolation exposing (Interpolator)
import Path exposing (Path)
import Random
import Scale exposing (ContinuousScale)
import Shape exposing (StackResult)
import Statistics
import Time
import Transition exposing (Transition)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, opacity, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


type Msg
    = Tick Int
    | DataRequested Time.Posix
    | DataArrived (List Series)
    | OverlayClicked
    | StackedClicked
    | StreamClicked
    | SeparateClicked


type alias Extent a =
    ( a, a )


type Configuration
    = Overlaid
    | Stacked
    | Stream
    | Separate


type alias Model =
    { config : Configuration
    , series : List Series
    , seriesT : Transition (StackResult String)
    , xExtent : Extent Time.Posix
    , xExtentT : Transition (Extent Time.Posix)
    }


type alias Series =
    { name : String
    , color : Color
    , data : List Datum
    }


type alias Datum =
    { x : Time.Posix
    , y0 : Float
    , y : Float
    , id : Int
    }


init : () -> ( Model, Cmd msg )
init _ =
    let
        noDataSeries =
            [ { name = "Chrysler", data = [], color = Color.green }
            , { name = "Toyota", data = [], color = Color.blue }
            , { name = "Volvo", data = [], color = Color.red }
            ]
    in
    ( { config = Overlaid
      , series = noDataSeries
      , seriesT = Transition.constant (overlaid noDataSeries)
      , xExtent = initXExtent
      , xExtentT = Transition.constant initXExtent
      }
    , Cmd.none
    )


initYExtent : Extent Float
initYExtent =
    ( 0.0, 5.0 )


initXExtent : Extent Time.Posix
initXExtent =
    let
        endMillis =
            1448928001000

        end =
            Time.millisToPosix endMillis

        start =
            endMillis - days 1 |> Time.millisToPosix
    in
    ( start, end )


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : Model -> ContinuousScale Time.Posix
xScale model =
    Scale.time Time.utc ( 0, w - 2 * padding ) (Transition.value model.xExtentT)


yScale : Model -> ContinuousScale Float
yScale model =
    Scale.linear ( h - 2 * padding, 0 ) (Transition.value model.seriesT |> .extent)


xAxis : Model -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount 10 ] (xScale model)


yAxis : Model -> Svg msg
yAxis model =
    Axis.left [ Axis.tickCount 5 ] (yScale model)


transformToLineData : Model -> ( Time.Posix, Float, Float ) -> Maybe ( Float, Float )
transformToLineData model ( x, y0, y1 ) =
    Just ( Scale.convert (xScale model) x, Scale.convert (yScale model) y1 )


tranfromToAreaData : Model -> ( Time.Posix, Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData model ( x, y0, y1 ) =
    Just
        ( ( Scale.convert (xScale model) x, Scale.convert (yScale model) y0 )
        , ( Scale.convert (xScale model) x, Scale.convert (yScale model) y1 )
        )


line : List ( Time.Posix, Float, Float ) -> Model -> Path
line series model =
    List.map (transformToLineData model) series
        |> Shape.line Shape.monotoneInXCurve


area : List ( Time.Posix, Float, Float ) -> Model -> Path
area series model =
    List.map (tranfromToAreaData model) series
        |> Shape.area Shape.monotoneInXCurve


withOpacity : Float -> Color -> Color
withOpacity opacity color =
    let
        c =
            color |> Color.toHsla
    in
    { c | alpha = opacity } |> Color.fromHsla


seriessToCoords : Model -> List { series : Series, coordinates : List ( Time.Posix, Float, Float ) }
seriessToCoords model =
    List.map2
        (\series transitionedData ->
            { series = series, coordinates = List.map2 (\da db -> ( da.x, Tuple.first db, Tuple.second db )) series.data transitionedData }
        )
        model.series
        (Transition.value model.seriesT |> .values)


viewSeries : Model -> List (Svg Msg)
viewSeries model =
    model
        |> seriessToCoords
        |> List.map
            (\s ->
                g []
                    [ Path.element (area s.coordinates model) [ strokeWidth 3, fill <| Paint <| withOpacity 0.5 <| s.series.color ]
                    , Path.element (line s.coordinates model) [ stroke <| Paint <| s.series.color, strokeWidth 3, fill PaintNone ]
                    ]
            )


view : Model -> Document Msg
view model =
    { title = ""
    , body =
        [ svg [ viewBox 0 0 w h ]
            [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
                [ xAxis model ]
            , g [ transform [ Translate (padding - 1) padding ] ]
                [ yAxis model ]
            , g [ transform [ Translate padding padding ], class [ "series" ] ]
                (viewSeries model)
            ]
        , button [ onClick OverlayClicked ] [ text "Overlaid" ]
        , button [ onClick StackedClicked ] [ text "Stacked" ]
        , button [ onClick StreamClicked ] [ text "Stream" ]
        , button [ onClick SeparateClicked ] [ text "Separate" ]
        ]
    }


span : Model -> Int
span model =
    let
        pair : ( Int, Int )
        pair =
            Tuple.mapBoth Time.posixToMillis Time.posixToMillis model.xExtent
    in
    Tuple.second pair - Tuple.first pair


stackOffsetSeparated : List (List ( Float, Float )) -> List (List ( Float, Float ))
stackOffsetSeparated series =
    List.foldl
        (\s1 ( maxSoFar, accum ) ->
            ( maxSoFar + (s1 |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0)
            , List.map (\( _, hi ) -> ( maxSoFar, maxSoFar + hi )) s1 :: accum
            )
        )
        ( 0, [] )
        series
        |> Tuple.second
        |> List.reverse


interpolatePosix : Time.Posix -> Time.Posix -> Interpolator Time.Posix
interpolatePosix from to =
    let
        from_ =
            Time.posixToMillis from

        to_ =
            Time.posixToMillis to
    in
    Interpolation.int from_ to_ |> Interpolation.map Time.millisToPosix


interpolateExtent : Extent Time.Posix -> Extent Time.Posix -> Interpolator (Extent Time.Posix)
interpolateExtent =
    Interpolation.tuple interpolatePosix interpolatePosix


interpolateStackResult : StackResult String -> StackResult String -> Float -> StackResult String
interpolateStackResult a b f =
    let
        extentInterpolator a1 b1 =
            Interpolation.tuple Interpolation.float Interpolation.float a1 b1 f
    in
    { extent = extentInterpolator a.extent b.extent
    , labels = List.sort b.labels
    , values = List.map2 (\sa sb -> List.map2 (\da db -> extentInterpolator da db) sa sb) a.values b.values
    }


days : Float -> Int
days d =
    d * 24 * 60 * 60 * 1000 |> round


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentXExtent : Extent Time.Posix
        currentXExtent =
            Transition.value model.xExtentT
    in
    case msg of
        DataRequested _ ->
            ( model
            , if span model <= days 15 then
                Random.generate DataArrived (randomSeriess model.series (Tuple.first currentXExtent))

              else
                Cmd.none
            )

        DataArrived series ->
            let
                newSeries : List Series
                newSeries =
                    List.map2 (\a b -> { a | data = List.sortBy .id (b.data ++ a.data) }) model.series series

                newXExtent : Extent Time.Posix
                newXExtent =
                    newSeries
                        |> List.map .data
                        |> List.concat
                        |> List.map .x
                        |> Statistics.extentBy Time.posixToMillis
                        |> Maybe.withDefault currentXExtent

                currentStackResult : StackResult String
                currentStackResult =
                    Transition.value model.seriesT

                normalizedCurrentSeries : StackResult String
                normalizedCurrentSeries =
                    { currentStackResult
                        | values =
                            List.map2 (\listOfFloats series_ -> List.repeat (List.length series_.data) ( 0.0, 0.0 ) ++ listOfFloats)
                                currentStackResult.values
                                series
                    }

                _ =
                    Debug.log "newSeries length" (List.head newSeries |> Maybe.map (.data >> List.length) |> Maybe.withDefault -1)

                _ =
                    Debug.log "normalizedCurrentSeries length" (List.head normalizedCurrentSeries.values |> Maybe.map List.length |> Maybe.withDefault -1)
            in
            ( { model
                | series = newSeries
                , seriesT = Transition.for 300 (interpolateStackResult normalizedCurrentSeries (currentStacking newSeries model))
                , xExtent = newXExtent
                , xExtentT = Transition.for 200 (interpolateExtent currentXExtent newXExtent)
              }
            , Cmd.none
            )

        Tick i ->
            ( { model
                | xExtentT = Transition.step i model.xExtentT
                , seriesT = Transition.step i model.seriesT
              }
            , Cmd.none
            )

        OverlayClicked ->
            ( { model
                | config = Overlaid
                , seriesT = Transition.for 500 (interpolateStackResult (Transition.value model.seriesT) (overlaid model.series))
              }
            , Cmd.none
            )

        StackedClicked ->
            ( { model
                | config = Stacked
                , seriesT = Transition.for 500 (interpolateStackResult (Transition.value model.seriesT) (stacked model.series))
              }
            , Cmd.none
            )

        StreamClicked ->
            ( { model
                | config = Stream
                , seriesT = Transition.for 500 (interpolateStackResult (Transition.value model.seriesT) (stream model.series))
              }
            , Cmd.none
            )

        SeparateClicked ->
            ( { model
                | config = Separate
                , seriesT = Transition.for 500 (interpolateStackResult (Transition.value model.seriesT) (separate model.series))
              }
            , Cmd.none
            )


currentStacking : List Series -> Model -> StackResult String
currentStacking series model =
    case model.config of
        Overlaid ->
            overlaid series

        Stacked ->
            stacked series

        Stream ->
            stream series

        Separate ->
            separate series


overlaid : List Series -> StackResult String
overlaid series =
    { values = List.map (\s -> List.map (\d -> ( 0.0, d.y )) s.data) series
    , labels = List.map .name series
    , extent =
        Statistics.extent (List.map .y (List.concatMap .data series))
            |> Maybe.withDefault initYExtent
            |> Tuple.mapFirst
                (\low ->
                    if low > 0 then
                        0

                    else
                        low
                )
    }


stacked : List Series -> StackResult String
stacked series =
    let
        config =
            { data = List.map (\s -> Tuple.pair s.name (List.map .y s.data)) series
            , offset = Shape.stackOffsetNone
            , order = identity
            }
    in
    Shape.stack config


stream : List Series -> StackResult String
stream series =
    let
        config =
            { data = List.map (\s -> Tuple.pair s.name (List.map .y s.data)) series
            , offset = Shape.stackOffsetWiggle
            , order = identity
            }
    in
    Shape.stack config


separate : List Series -> StackResult String
separate series =
    let
        config =
            { data = List.map (\s -> Tuple.pair s.name (List.map .y s.data)) series
            , offset = stackOffsetSeparated
            , order = identity
            }
    in
    Shape.stack config


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


randomSequence : List (Random.Generator a) -> Random.Generator (List a)
randomSequence =
    List.foldr (Random.map2 (::)) (Random.constant [])


randomSeriess : List Series -> Time.Posix -> Random.Generator (List Series)
randomSeriess series startTime =
    Random.int 3 20
        |> Random.andThen (\length -> List.map (\s -> randomSeries length s startTime) series |> randomSequence)


randomSeries : Int -> Series -> Time.Posix -> Random.Generator Series
randomSeries length series startTime =
    Random.andThen (\data -> Random.constant { series | data = data }) (randomData length startTime)


randomData : Int -> Time.Posix -> Random.Generator (List Datum)
randomData length startTime =
    let
        randomDatum : Int -> Time.Posix -> Random.Generator (List Datum) -> Random.Generator (List Datum)
        randomDatum length_ startTime_ list =
            let
                newId : Int
                newId =
                    Time.posixToMillis startTime_ - days 0.25

                newStart : Time.Posix
                newStart =
                    newId |> Time.millisToPosix
            in
            if length_ > 1 then
                Random.map2 (::) (Random.map4 Datum (Random.constant newStart) (Random.constant 0.0) (Random.float 10.0 350.0) (Random.constant newId)) (randomDatum (length_ - 1) newStart list)

            else
                list
    in
    randomDatum length startTime (Random.constant [])


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if Transition.isComplete model.xExtentT && Transition.isComplete model.seriesT then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta (round >> Tick)
        , Time.every 1000 DataRequested
        ]
