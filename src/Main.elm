module Main exposing (main)

import Axis
import Browser exposing (Document)
import Browser.Events
import Color exposing (Color)
import Interpolation exposing (Interpolator)
import Path exposing (Path)
import Random
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import Time
import Transition exposing (Transition)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


type Msg
    = Tick Int
    | DataRequested Time.Posix
    | DataArrived (List Datum)


type alias Extent =
    ( Time.Posix, Time.Posix )


type alias Model =
    { data : List Datum
    , dataT : Transition (List Datum)
    , extent : Extent
    , extentT : Transition Extent
    }


type alias Datum =
    { x : Time.Posix, y : Float, id : Int }


type alias Series =
    { name : String
    , color : Color
    , data : List Datum
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { data = []
      , dataT = Transition.constant []
      , extent = initExtent
      , extentT = Transition.constant initExtent
      }
    , Cmd.none
    )


initExtent : Extent
initExtent =
    let
        endMillis =
            1448928001000

        end =
            Time.millisToPosix endMillis

        start =
            endMillis - days 20 |> Time.millisToPosix
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
    Scale.time Time.utc ( 0, w - 2 * padding ) (Transition.value model.extentT)


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 5 )


xAxis : Model -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount 10 ] (xScale model)


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


transformToLineData : Model -> Datum -> Maybe ( Float, Float )
transformToLineData model { x, y } =
    Just ( Scale.convert (xScale model) x, Scale.convert yScale y )


tranfromToAreaData : Model -> Datum -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData model { x, y } =
    Just
        ( ( Scale.convert (xScale model) x, Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert (xScale model) x, Scale.convert yScale y )
        )


line : Model -> Path
line model =
    List.map (transformToLineData model) (Transition.value model.dataT)
        |> Shape.line Shape.monotoneInXCurve


area : Model -> Path
area model =
    List.map (tranfromToAreaData model) (Transition.value model.dataT)
        |> Shape.area Shape.monotoneInXCurve


view : Model -> Document msg
view model =
    { title = ""
    , body =
        [ svg [ viewBox 0 0 w h ]
            [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
                [ xAxis model ]
            , g [ transform [ Translate (padding - 1) padding ] ]
                [ yAxis ]
            , g [ transform [ Translate padding padding ], class [ "series" ] ]
                [ Path.element (area model) [ strokeWidth 3, fill <| Paint <| Color.rgba 1 0 0 0.54 ]
                , Path.element (line model) [ stroke <| Paint <| Color.rgb 1 0 0, strokeWidth 3, fill PaintNone ]
                ]
            ]
        ]
    }


span : Model -> Int
span model =
    let
        pair : ( Int, Int )
        pair =
            Tuple.mapBoth Time.posixToMillis Time.posixToMillis model.extent
    in
    Tuple.second pair - Tuple.first pair


interpolatePosix : Time.Posix -> Time.Posix -> Interpolator Time.Posix
interpolatePosix from to =
    let
        from_ =
            Time.posixToMillis from

        to_ =
            Time.posixToMillis to
    in
    Interpolation.int from_ to_ |> Interpolation.map Time.millisToPosix


interpolateExtent : Extent -> Extent -> Interpolator Extent
interpolateExtent =
    Interpolation.tuple interpolatePosix interpolatePosix


interpolateData : List Datum -> List Datum -> Interpolator (List Datum)
interpolateData =
    Interpolation.list
        { add = interpolateDatumFromNothing
        , remove = interpolateDatumToNothing
        , change = interpolateDatum
        , id = .id
        , combine = Interpolation.CombineParallel
        }


interpolateDatumFromNothing : Datum -> Interpolator Datum
interpolateDatumFromNothing b =
    let
        yInterpolator =
            Interpolation.float 0.0 b.y
    in
    Interpolation.map3 Datum (always b.x) yInterpolator (always b.id)


interpolateDatumToNothing : Datum -> Interpolator Datum
interpolateDatumToNothing a =
    let
        yInterpolator =
            Interpolation.float a.y 0.0
    in
    Interpolation.map3 Datum (always a.x) yInterpolator (always a.id)


interpolateDatum : Datum -> Datum -> Interpolator Datum
interpolateDatum a b =
    let
        yInterpolator =
            Interpolation.float a.y b.y
    in
    Interpolation.map3 Datum (always a.x) yInterpolator (always a.id)


days : Float -> Int
days d =
    d * 24 * 60 * 60 * 1000 |> round


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentExtent : Extent
        currentExtent =
            Transition.value model.extentT
    in
    case msg of
        DataRequested _ ->
            ( model
            , if span model <= days 20 then
                Random.generate DataArrived (randomSeries (Tuple.first currentExtent))

              else
                Cmd.none
            )

        DataArrived data ->
            let
                newData : List Datum
                newData =
                    data ++ model.data

                newExtent : Extent
                newExtent =
                    newData
                        |> List.map .x
                        |> Statistics.extentBy Time.posixToMillis
                        |> Maybe.withDefault currentExtent
            in
            ( { model
                | data = newData
                , dataT = Transition.for 300 (interpolateData (Transition.value model.dataT) newData)
                , extent = newExtent
                , extentT = Transition.for 200 (interpolateExtent currentExtent newExtent)
              }
            , Cmd.none
            )

        Tick i ->
            ( { model
                | extentT = Transition.step i model.extentT
                , dataT = Transition.step i model.dataT
              }
            , Cmd.none
            )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


randomSeries : Time.Posix -> Random.Generator (List Datum)
randomSeries startTime =
    let
        newStart : Time.Posix
        newStart =
            Time.posixToMillis startTime - days 0.25 |> Time.millisToPosix
    in
    Random.andThen identity <|
        Random.weighted
            ( 10, Random.constant [] )
            [ ( 90
              , Random.map2 (::)
                    (Random.map3 Datum (Random.constant newStart) (Random.float 1.0 4.0) (Random.constant (Time.posixToMillis newStart)))
                    (Random.lazy (\_ -> randomSeries newStart))
              )
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if Transition.isComplete model.extentT then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta (round >> Tick)
        , Time.every 1000 DataRequested
        ]
