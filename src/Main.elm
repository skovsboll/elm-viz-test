module Main exposing (main)

import Axis
import Basics as Math
import Browser exposing (Document)
import Browser.Events
import Color exposing (Color)
import Force
import Html exposing (Html, a, button, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Interpolation exposing (Interpolator)
import Path exposing (Path)
import Random
import SHA1
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
    | RidgeClicked


type alias Extent a =
    ( a, a )


type Configuration
    = Overlaid
    | Stacked
    | Stream
    | Ridge


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



-----------------------------------------
---- Init
-----------------------------------------


init : () -> ( Model, Cmd msg )
init _ =
    let
        spacedColors : List Color
        spacedColors =
            noDataSeries
                |> List.map .color
                |> spaceColors

        recoloredDataSeries : List Series
        recoloredDataSeries =
            List.map2 (\s c -> { s | color = c }) noDataSeries spacedColors
    in
    ( { config = Stream
      , series = recoloredDataSeries
      , seriesT = Transition.constant (overlaid recoloredDataSeries)
      , xExtent = initXExtent
      , xExtentT = Transition.constant initXExtent
      }
    , Cmd.none
    )


noDataSeries : List Series
noDataSeries =
    [ { name = "Chrysler", data = [], color = Color.green }
    , { name = "Toyota", data = [], color = Color.blue }
    , { name = "VW", data = [], color = Color.red }
    , { name = "Hyundai", data = [], color = Color.red }
    , { name = "Honda", data = [], color = Color.red }
    , { name = "Ford", data = [], color = Color.red }
    , { name = "Kia0", data = [], color = Color.red }
    , { name = "Kia2", data = [], color = Color.red }
    , { name = "Kia3", data = [], color = Color.red }
    , { name = "Kia4", data = [], color = Color.red }
    , { name = "Kia5", data = [], color = Color.red }
    , { name = "Kia6", data = [], color = Color.red }
    , { name = "Kia7", data = [], color = Color.red }
    , { name = "Kia8", data = [], color = Color.red }
    , { name = "Kia9", data = [], color = Color.red }
    , { name = "KiaA", data = [], color = Color.red }
    , { name = "KiaB", data = [], color = Color.red }
    , { name = "KiaC", data = [], color = Color.red }
    , { name = "KiaD", data = [], color = Color.red }
    , { name = "KiaE", data = [], color = Color.red }
    , { name = "KiaF", data = [], color = Color.red }
    ]
        |> List.map (\s -> { s | color = nameToColor s.name })


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



-----------------------------------------
---- Dimensions
-----------------------------------------


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


saturationScale : ContinuousScale Float
saturationScale =
    Scale.linear ( 0.15, 0.85 ) ( 0.0, 1.0 )


saturationScaleWithExtent : ( Float, Float ) -> ContinuousScale Float
saturationScaleWithExtent extent =
    Scale.linear (Scale.range saturationScale) extent


lightnessScale : ContinuousScale Float
lightnessScale =
    Scale.linear ( 0.85, 0.35 ) ( 0.0, 1.0 )



-----------------------------------------
---- Colors
-----------------------------------------


nameToColor : String -> Color
nameToColor name =
    let
        fractions : List Float
        fractions =
            SHA1.fromString name
                |> SHA1.toByteValues
                |> List.map toFloat
                |> List.map (\byte -> byte / 256.0)
    in
    case fractions of
        a :: b :: _ ->
            Color.fromHsla
                { hue = a
                , lightness = 0.5
                , saturation = Scale.convert saturationScale b
                , alpha = 1.0
                }

        _ ->
            Color.blue


spaceColors : List Color -> List Color
spaceColors colors =
    let
        numberOfBodies : Math.Int
        numberOfBodies =
            List.length colors

        iterations =
            3

        force : Force.Force Int
        force =
            List.range 0 (numberOfBodies - 1)
                |> Force.manyBodyStrength (toFloat -numberOfBodies / iterations)

        state : Force.State Int
        state =
            Force.simulation [ force ]
                |> Force.iterations iterations

        polarInputColors : List ( Float, Float )
        polarInputColors =
            colors
                |> List.map Color.toHsla
                |> List.map (\c -> fromPolar ( c.saturation, c.hue * 2 * Math.pi ))

        colorEntities : List (Force.Entity Int {})
        colorEntities =
            polarInputColors
                |> List.indexedMap
                    (\i ( x, y ) ->
                        { x = x
                        , y = y
                        , vx = 0.0
                        , vy = 0.0
                        , id = i
                        }
                    )

        simulationResult : List (Force.Entity Int {})
        simulationResult =
            Force.computeSimulation state colorEntities |> List.sortBy .id

        entityToPolar : { a | x : Float, y : Float } -> ( Float, Float )
        entityToPolar { x, y } =
            toPolar ( x, y )

        polarOutputColors : List ( Float, Float )
        polarOutputColors =
            List.map entityToPolar simulationResult

        maxRadius : Float
        maxRadius =
            polarOutputColors |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 1.0

        minRadius : Float
        minRadius =
            polarInputColors |> List.map Tuple.first |> List.minimum |> Maybe.withDefault 0.0

        newColors : List Color
        newColors =
            List.map
                (\( r, theta ) ->
                    let
                        saturation =
                            Scale.convert (saturationScaleWithExtent ( minRadius, maxRadius )) r
                    in
                    Color.fromHsla
                        { hue = theta / (2 * Math.pi)
                        , lightness = Scale.convert lightnessScale saturation
                        , saturation = saturation
                        , alpha = 1.0
                        }
                )
                polarOutputColors
    in
    newColors



-----------------------------------------
---- View
-----------------------------------------


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
                    [ Path.element (area s.coordinates model) [ strokeWidth 3, fill <| Paint <| withOpacity 1.0 <| s.series.color ]
                    , Path.element (line s.coordinates model) [ stroke <| Paint <| s.series.color, strokeWidth 1, fill PaintNone ]
                    ]
            )


view : Model -> Document Msg
view model =
    { title = ""
    , body =
        [ viewChart model
        , button [ onClick OverlayClicked ] [ text "Overlaid" ]
        , button [ onClick StackedClicked ] [ text "Stacked" ]
        , button [ onClick StreamClicked ] [ text "Stream" ]
        , button [ onClick RidgeClicked ] [ text "Ridge" ]
        , viewLegends model
        ]
    }


viewChart : Model -> Svg Msg
viewChart model =
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis model ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            (viewSeries model)
        ]


viewLegends : Model -> Html Msg
viewLegends model =
    table []
        [ thead [] [ tr [] [ th [] [ text "Brand" ], th [] [ text "Color" ], th [] [ text "Remapped" ] ] ]
        , tbody [] (List.map2 viewSeriesLegend (List.reverse model.series) (List.reverse noDataSeries))
        ]


viewSeriesLegend : Series -> Series -> Html Msg
viewSeriesLegend series origSeries =
    tr []
        [ td [] [ text series.name ]
        , td [ style "background-color" (origSeries.color |> Color.toCssString) ] []
        , td [ style "background-color" (series.color |> Color.toCssString) ] []
        ]



-----------------------------------------
---- Interpolation
-----------------------------------------


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



-----------------------------------------
---- Update
-----------------------------------------


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

        RidgeClicked ->
            ( { model
                | config = Ridge
                , seriesT = Transition.for 500 (interpolateStackResult (Transition.value model.seriesT) (separate model.series))
              }
            , Cmd.none
            )


span : Model -> Int
span model =
    let
        pair : ( Int, Int )
        pair =
            Tuple.mapBoth Time.posixToMillis Time.posixToMillis model.xExtent
    in
    Tuple.second pair - Tuple.first pair


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



----------------------------------------------
--- View modes
----------------------------------------------


currentStacking : List Series -> Model -> StackResult String
currentStacking series model =
    case model.config of
        Overlaid ->
            overlaid series

        Stacked ->
            stacked series

        Stream ->
            stream series

        Ridge ->
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



----------------------------------------------
--- Subscriptions
----------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if Transition.isComplete model.xExtentT && Transition.isComplete model.seriesT then
            Sub.none

          else
            Browser.Events.onAnimationFrameDelta (round >> Tick)
        , Time.every 1000 DataRequested
        ]



----------------------------------------------
--- Random
----------------------------------------------


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
