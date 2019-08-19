module Main where

import Prelude
import Effect (Effect)
import Data.Maybe(Maybe(..), maybe)
import Data.Int (toNumber)
import Data.Foldable (foldr)
import Control.Alt(alt)
import Color (rgb)
import Data.Sparse.Polynomial((^))
import Graphics.Drawing( Drawing, render, fillColor, filled, rectangle
                       , scale, translate, everywhere)
import Graphics.Canvas.Geometry (class DrawableSet, arc, circle, drawIn
                                , halfline, length, line, meets, middle
                                , normalTo, point, rename, rightangle
                                , segment, vector
                                , Point(..), Line(..), Circle(..), HalfLine(..)
                                , Segment(..), abs, ord)
import DOM.Editor as DOM
import FRP.Behavior (Behavior, animate, unfold, step)
import FRP.Event(Event)
import FRP.Event.Time(interval)
import FRP.Behavior.Mouse (buttons, position)
import FRP.Event.Mouse (Mouse, getMouse, withPosition, withButtons)
import Data.Set(isEmpty)

width = 800.0 :: Number
height = 600.0 :: Number

origin = point "" 0.0 0.0 :: Point
topRight = point "" width 0.0 :: Point
bottomLeft = point "" 0.0 height :: Point
end = point "" width height :: Point

boxLocalMin = point "" (-6.0) (-5.0) :: Point
boxLocalMax = point "" 6.0 5.0 :: Point

frame :: Array Line
frame = 
  [ line origin topRight
  , line topRight end
  , line end bottomLeft
  , line bottomLeft origin]

somePosition :: Maybe {x :: Int, y :: Int} -> {x :: Number, y :: Number}
somePosition =
  maybe { x: 0.0, y: 0.0 }
        (\{ x, y } -> { x: toNumber x
                      , y: toNumber y }) 

moveWithButton ::
  Effect (Event { value :: String
                , pos :: {x :: Number, y :: Number}}) 
moveWithButton = 
  (\mouse -> 
    (\{value, buttons} -> 
      { value: if isEmpty buttons 
               then "buttonup" 
               else "buttondown"
      , pos: value}) <$>
        withButtons mouse ((\{value,pos} -> 
          somePosition pos) <$> withPosition mouse (interval 10))) <$> 
            getMouse

data Final = FP Point | FL Line | FC Circle | FH HalfLine | FS Segment
                      | FA (Array Final)

magFactor = 100.0 :: Number

mag :: Point -> Point
mag (Point {name, coordinates}) = Point {name, coordinates: magFactor^0*coordinates} 

instance drawableFinal :: DrawableSet Final where
  drawIn ctx (FP p) = drawIn ctx $ mag p
  drawIn ctx (FL (Line {a,b,c})) = drawIn ctx $ Line {a,b,c: magFactor*c}
  drawIn ctx (FC (Circle {center, radius})) = 
    drawIn ctx $ Circle {center: mag center, radius: magFactor*radius}
  drawIn ctx (FH (HalfLine {origin, direction})) = 
    drawIn ctx $ HalfLine {origin: mag origin, direction}
  drawIn ctx (FS (Segment {origin, extremity,asOriented})) = 
    drawIn ctx $ Segment {origin: mag origin, extremity: mag extremity, asOriented}
  drawIn ctx (FA arr) = drawIn ctx arr

type State = { drawing :: Drawing
             , box :: {pointMin :: Point, pointMax :: Point}
             , transVector :: {x :: Number, y :: Number}
             , scaleFactor :: {x :: Number, y :: Number}
             , xtemp :: Number
             , ytemp :: Number}

initialState :: (Final -> Drawing) -> State
initialState draw = 
  { drawing: let a = point "" (-4.0) (-4.0)
                 b = point "" (-4.0) 4.0
                 c = point "" 4.0 4.0
                 d = point "" 4.0 (-4.0)
             in (draw $ FA $ FS <$> [ segment a b Nothing
                                   , segment b c Nothing
                                   , segment c d Nothing
                                   , segment d a Nothing])
                <> (draw $ FC $ circle (point "" 0.0 0.0) 4.0)
  , box:  { pointMin: boxLocalMin
          , pointMax: boxLocalMax}
  , transVector: {x: -abs boxLocalMin, y: -ord boxLocalMin}
  , scaleFactor: { x: width / (abs boxLocalMax - abs boxLocalMin)
                 , y: height / (ord boxLocalMax - ord boxLocalMin)} 
  , xtemp: 0.0
  , ytemp: 0.0}

reframe :: State -> Drawing
reframe st = 
  let {x: ux, y: uy} = st.transVector
      {x: kx, y: ky} = st.scaleFactor
   in ({-scale kx ky <<< -}translate ux uy) st.drawing

ePage :: (Final -> Drawing) -> Effect (Behavior Drawing) 
ePage draw = liftA1 reframe  <$> 
    (\ event -> 
      unfold (\{value, pos: {x,y}} st -> 
        case value of
          "buttonup" -> st{transVector = {x: -abs boxLocalMin, y: -ord boxLocalMin}}
          "buttondown" -> 
                 let dx = (abs st.box.pointMin 
                         - abs st.box.pointMax) * (x-st.xtemp) / width
                     dy = (ord st.box.pointMin
                         - ord st.box.pointMax) * (y-st.ytemp) / height
                  in st{ transVector = { x: - magFactor * dx, y: - magFactor * dy}
                       , box = { pointMin: point "" (abs st.box.pointMin + dx)
                                                    (ord st.box.pointMin + dy)
                               , pointMax: point "" (abs st.box.pointMax + dx)
                                                    (ord st.box.pointMax + dy)}
                       , xtemp = x
                       , ytemp = y}
                       
          otherwise -> st{drawing = draw $ FA $ FL <$> frame}
         ) event (initialState draw)) <$> moveWithButton
    
main :: Effect Unit
main = do
  setup <- DOM.setup
  canvas <- DOM.getElementById "canvas" setup.document
  context2D <- DOM.getContext2D canvas
  _ <- DOM.setAttribute "width" (show width) canvas
  _ <- DOM.setAttribute "height" (show height) canvas
  
  let ctx = { color: rgb 5 4 9
            , lineWidth: 1.50}

  page <- ePage $ (drawIn ctx :: forall a. DrawableSet a => a -> Drawing)
  let background = filled (fillColor $ rgb 255 255 255) 
                          (rectangle 0.0 0.0 width height)
  _ <- animate (pure background <> page) (render context2D)
  pure unit
