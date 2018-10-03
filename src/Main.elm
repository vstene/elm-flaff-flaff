module Main exposing (Ball, Copter, Missile, Model, Msg(..), ballView, centerLine, copter1View, copter2View, fireMissile, init, keyAction, loopKeys, main, missileView, subscriptions, transform, update, updateBall, updateCopter, updateMissile, updateMissiles, updateTicks, view)

import AnimationFrame
import Html
import Html.Attributes exposing (style)
import Keyboard exposing (..)
import Set exposing (Set)
import Svg exposing (circle, g, rect, svg, text_)
import Svg.Attributes exposing (cx, cy, dy, fill, fontSize, height, r, rx, ry, viewBox, width, x, y)
import Time exposing (Time)



-- MODEL


type alias Model =
    { copter : Copter
    , ball : Ball
    , ticks : Int
    , keysDown : Set KeyCode
    , missiles : List Missile
    }


type alias Copter =
    { typeVisible : Int
    , x : Int
    , y : Int
    }


type alias Missile =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type alias Ball =
    { x : Int
    , y : Int
    , r : Int
    , dx : Int
    , dy : Int
    }


init =
    ( { copter = Copter 1 45 10
      , ball = Ball 550 40 10 8 8
      , ticks = 0
      , keysDown = Set.empty
      , missiles = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time
    | KeyUp Keyboard.KeyCode
    | KeyDown Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model
                |> updateTicks
                |> loopKeys (Set.toList model.keysDown)
                |> updateMissiles
                |> updateBall dt
                |> updateCopter dt
            , Cmd.none
            )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )


loopKeys keysPressed model =
    case keysPressed of
        key :: keysPressed ->
            loopKeys keysPressed (keyAction model key)

        [] ->
            model


fireMissile model =
    -- Fire missile from front of copter
    let
        x =
            model.copter.x + 55

        y =
            model.copter.y + 38

        width =
            10

        height =
            2
    in
    -- Only allow max N missiles
    if List.length model.missiles < 20 then
        { model | missiles = Missile x y width height :: model.missiles }

    else
        model


updateMissiles model =
    let
        missilesOnScreen =
            List.filter (\missile -> missile.x < 1000) model.missiles
    in
    { model | missiles = List.map updateMissile missilesOnScreen }


updateMissile missile =
    let
        delta =
            10
    in
    { missile | x = missile.x + delta }


keyAction model key =
    let
        delta =
            6

        minX =
            32

        maxX =
            420

        minY =
            0

        maxY =
            550

        copter_ =
            model.copter
    in
    -- ARROW UP
    if key == 38 && (model.copter.y - delta) > minY then
        { model | copter = { copter_ | y = model.copter.y - delta } }
        -- ARROW RIGHT

    else if key == 39 && (model.copter.x + delta) < maxX then
        { model | copter = { copter_ | x = model.copter.x + delta } }
        -- ARROW DOWN

    else if key == 40 && (model.copter.y + delta) < maxY then
        { model | copter = { copter_ | y = model.copter.y + delta } }
        -- ARROW LEFT

    else if key == 37 && (model.copter.x - delta) > minX then
        { model | copter = { copter_ | x = model.copter.x - delta } }
        -- SPACE BAR

    else if key == 32 then
        fireMissile model

    else
        model


centerLine =
    rect [ x "495", y "0", width "10", height "100%" ] []



{- Update ball every N ticks -}


updateBall dt model =
    let
        minX =
            515

        maxX =
            990

        minY =
            10

        maxY =
            590

        ball_ =
            model.ball

        modelX =
            if model.ball.dx > 0 then
                if (model.ball.x + model.ball.dx) > maxX then
                    { model | ball = { ball_ | x = model.ball.x + 5, dx = model.ball.dx * -1 } }

                else
                    { model | ball = { ball_ | x = model.ball.x + model.ball.dx } }

            else if (model.ball.x + model.ball.dx) < minX then
                { model | ball = { ball_ | x = model.ball.x - 5, dx = model.ball.dx * -1 } }

            else
                { model | ball = { ball_ | x = model.ball.x + model.ball.dx } }

        ballX_ =
            modelX.ball

        modelY =
            if modelX.ball.dy > 0 then
                if (modelX.ball.y + modelX.ball.dy) > maxY then
                    { modelX | ball = { ballX_ | y = modelX.ball.y + 5, dy = modelX.ball.dy * -1 } }

                else
                    { modelX | ball = { ballX_ | y = modelX.ball.y + modelX.ball.dy } }

            else if (modelX.ball.y + modelX.ball.dy) < minY then
                { modelX | ball = { ballX_ | y = modelX.ball.y - 5, dy = modelX.ball.dy * -1 } }

            else
                { modelX | ball = { ballX_ | y = modelX.ball.y + modelX.ball.dy } }
    in
    modelY



{- To reduce fps only do actions every N ticks -}


updateTicks model =
    let
        resetTicksOnValue =
            3

        ticks : Int
        ticks =
            if model.ticks < resetTicksOnValue then
                model.ticks + 1

            else
                0
    in
    { model | ticks = ticks }



{- Update copter every N ticks -}


updateCopter dt model =
    let
        copterType : Int
        copterType =
            if model.ticks == 0 then
                if model.copter.typeVisible == 1 then
                    2

                else
                    1

            else
                model.copter.typeVisible

        copter_ : Copter
        copter_ =
            model.copter
    in
    { model | copter = { copter_ | typeVisible = copterType } }



-- SUBSCRIPTIONS


subscriptions model =
    Sub.batch
        [ AnimationFrame.times Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]



-- VIEW


view model =
    let
        copterView =
            if model.copter.typeVisible == 1 then
                copter1View

            else
                copter2View
    in
    svg [ width "1000", height "600", viewBox "0 0 1000 600" ] ([ centerLine, copterView model.copter, ballView model.ball ] ++ List.map missileView model.missiles)


transform x y =
    Svg.Attributes.transform ("translate(" ++ x ++ "," ++ y ++ ")")


copter1View copter =
    g [ transform (toString copter.x) (toString copter.y) ]
        [ text_ [ fontSize "8", style [ ( "white-space", "pre" ) ] ]
            [ Svg.tspan [ x "18", dy "8px" ] [ Svg.text ":FLAFF:FLAFF" ]
            , Svg.tspan [ x "13", dy "8px" ] [ Svg.text "_^___" ]
            , Svg.tspan [ x "0", dy "10px" ] [ Svg.text "__/  [] \\" ]
            , Svg.tspan [ x "-29", dy "8px" ] [ Svg.text "SOI===__       \\" ]
            , Svg.tspan [ x "8", dy "8px" ] [ Svg.text "\\_______]" ]
            , Svg.tspan [ x "20", dy "8px" ] [ Svg.text "I   I" ]
            , Svg.tspan [ x "12", dy "0px" ] [ Svg.text "________/" ]
            ]
        ]


copter2View copter =
    g [ transform (toString copter.x) (toString copter.y) ]
        [ text_ [ fontSize "8", style [ ( "white-space", "pre" ) ] ]
            [ Svg.tspan [ x "-35", dy "8px" ] [ Svg.text "FLAFF:FLAFF:" ]
            , Svg.tspan [ x "13", dy "8px" ] [ Svg.text "_^___" ]
            , Svg.tspan [ x "-29", dy "10px" ] [ Svg.text "S     __/  [] \\" ]
            , Svg.tspan [ x "-29", dy "8px" ] [ Svg.text " O ===__       \\" ]
            , Svg.tspan [ x "-21", dy "8px" ] [ Svg.text "I     \\_______]" ]
            , Svg.tspan [ x "20", dy "8px" ] [ Svg.text "I   I" ]
            , Svg.tspan [ x "12", dy "0px" ] [ Svg.text "________/" ]
            ]
        ]


ballView ball =
    circle [ cx (toString ball.x), cy (toString ball.y), r (toString ball.r) ] []


missileView missile =
    rect [ x (toString missile.x), y (toString missile.y), width (toString missile.width), height (toString missile.height) ] []



-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
