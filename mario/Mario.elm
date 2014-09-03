module Mario where
{-| This is a partially complete version of Mario. Your challenge is to finish
the code!

(1) Finish the UPDATE section by providing real definitions for the following
    functions: jump, gravity, walk

(2) Improve the 'display' function so that Mario animates correctly as he moves
    around. Use the images in the mario/imgs/ directory so that he walks when
    he moves on the ground and is jumping when he is in the air.

(3) Our Model has no memory of which way mario was facing last frame, so he
    sometimes turns around abruptly when he stops moving. Add a way to track
    which way he was walking so he never turns around for no reason.

Search for TODO comments to find the spots you need to change.
-}

import Keyboard
import Window
import Debug


-- MODEL

type Model =
    { x  : Float
    , y  : Float
    , vx : Float
    , vy : Float
    }

type Keys = { x:Int, y:Int }

mario : Model
mario =
    { x = 0
    , y = 0 
    , vx = 0
    , vy = 0
    }


-- UPDATE

step : (Float, Keys) -> Model -> Model
step (dt, keys) mario =
    mario
        |> gravity dt
        |> jump keys
        |> walk keys
        |> physics dt

jump : Keys -> Model -> Model
jump keys mario =
    mario
    -- TODO: update mario to jump when the user presses up

gravity : Float -> Model -> Model
gravity dt mario =
    mario
    -- TODO: have gravity act on mario

physics : Float -> Model -> Model
physics dt mario =
    { mario |
        x <- mario.x + dt * mario.vx,
        y <- mario.y + dt * mario.vy
    }

walk : Keys -> Model -> Model
walk keys mario =
    mario
    -- TODO: make mario move left and right based on keyboard input


-- DISPLAY

display : (Int, Int) -> Model -> Element
display (w',h') mario =
  let (w,h) = (toFloat w', toFloat h')

      -- TODO: change the image based on what mario is doing right now
      src = "imgs/mario/stand/right.gif"

      marioImage = image 35 35 src

      groundY = 62 - h/2
  in
      collage w' h'
          [ rect w h
              |> filled (rgb 174 238 238)
          , rect w 50
              |> filled (rgb 74 167 43)
              |> move (0, 24 - h/2)
          , marioImage
              |> toForm
              |> Debug.trace "mario"
              |> move (mario.x, mario.y + groundY)
          ]


-- SIGNALS

main : Signal Element
main = lift2 display Window.dimensions (foldp step mario input)

input : Signal (Float, Keys)
input =
  let delta = lift (\t -> t/20) (fps 25)
      deltaArrows =
          lift2 (,) delta (Debug.watch "arrows" <~ Keyboard.arrows)
  in
      sampleOn delta deltaArrows
