module Main where

import Prelude
import Effect (Effect)
import Data.Maybe(Maybe(..), maybe, fromJust)
import Data.Array(filter, length, (..), catMaybes, uncons, dropWhile, head, tail)
import Data.Foldable (foldr)
import Control.Alt(alt)
import Color (rgb)
import Graphics.Drawing( Drawing, render, fillColor, filled, rectangle)
import Graphics.Canvas.Geometry ( class DrawableSet
                                , Circle(..), HalfLine(..), Line, Point, Segment(..)
                                , aPointOnLine, aVectorOfLine, abs, circle, drawIn
                                , line, meets, ord, point, segment, vector, (<+|))
import Graphics.Canvas.Geometry(length) as Geo
import DOM.Editor as DOM
import FRP.Behavior (Behavior, animate, unfold)
import FRP.Event(Event, create)
import FRP.Event.Time(interval)
import FRP.Event.Mouse (getMouse, withPosition, withButtons)
import Data.Set(isEmpty)
import Partial.Unsafe(unsafePartial)
import Partial
import Data.Int(toNumber, floor, ceil)

width = 800.0 :: Number
height = 600.0 :: Number

type Domain = Number -> Boolean
type Function = {domain :: Domain, expression :: Number -> Number}

square :: Function
square = {domain: const true, expression: \x -> x*x}

inverse :: Function
inverse = {domain: \x -> x /= 0.0, expression: \x -> 1.0 / x}

pen :: Partial => Box -> Array Segment -> Maybe {x :: Number, y :: Number} -> Array (Maybe {x :: Number, y :: Number}) -> Array Segment
pen from acc _        [] = acc
pen from acc (Just p) xs =
 let okY y = botY <= y && y <= topY
     botY = ord from.center - from.halfHeight
     topY = ord from.center + from.halfHeight
  in case uncons xs of
      Just {head: hd, tail: tl} -> 
          case hd of
              Just {x, y} -> pen from (
                               if okY p.y && okY y 
                                then acc <> [segment (point "" p.x p.y) 
                                                     (point "" x y) Nothing]
                                else acc) hd tl
              Nothing     -> let ys :: Array (Maybe {x :: Number, y :: Number})
                                 ys = dropWhile (_ == Nothing) tl
                               in if length ys == 0 
                                    then acc 
                                    else pen from acc (unsafePartial fromJust $ head ys) 
                                                      (unsafePartial fromJust $ tail ys)
      Nothing -> acc

plot :: (Final -> Drawing) -> Box -> Box -> Function -> Drawing
plot draw from to {domain, expression} = 
        let botX = abs from.center - from.halfWidth
            topX = abs from.center + from.halfWidth
            zs = dropWhile (_ == Nothing) $ 
              (\x -> if domain x then Just {x, y: expression x} else Nothing) <$> 
                 (\n -> botX + toNumber n * (topX - botX) / 100.0) <$> 0..100
        in if length zs == 0 
                   then mempty 
                   else foldr (\s acc -> acc <> (draw $ FS from to s) ) mempty $ 
                           unsafePartial pen from [] (unsafePartial fromJust $ head zs) 
                                                     (unsafePartial fromJust $ tail zs)
                           
type Box = {center :: Point, halfWidth :: Number, halfHeight :: Number}

remap :: Box -> Point -> Box -> Point
remap {center: c0, halfWidth: w0, halfHeight: h0}
      p 
      {center: c1, halfWidth: w1, halfHeight: h1} = 
        let v = vector c0 p
            x = abs v * w1 / w0
            y = ord v * h1 / h0
         in point "" (x + abs c1) (y + ord c1)

window = { center: point "" (width/2.0) (height/2.0)
         , halfWidth: width/2.0
         , halfHeight: height/2.0} :: Box

functionDisplay = { center: point "" (width/2.0) (height/6.0)
                  , halfWidth: width/2.0
                  , halfHeight: height/6.0} :: Box

local = {center: point "" 0.0 0.0, halfWidth: 6.0, halfHeight: 5.0} :: Box

frame :: Box -> Array Segment
frame {center, halfWidth, halfHeight} =
  let seg sx sy tx ty = segment (point "" (abs center + sx * halfWidth)
                                          (ord center + sy * halfHeight))
                                (point "" (abs center + tx *  halfWidth)
                                          (ord center + ty * halfHeight))
                                Nothing
  in [ seg (-1.0) (-1.0)   1.0  (-1.0)
     , seg   1.0  (-1.0)   1.0    1.0
     , seg   1.0    1.0  (-1.0)   1.0
     , seg (-1.0)   1.0  (-1.0) (-1.0)] 

somePosition :: Maybe {x :: Int, y :: Int} -> {x :: Number, y :: Number}
somePosition =
  maybe { x: 0.0, y: 0.0 }
        (\{ x, y } -> { x: toNumber x
                      , y: toNumber y }) 

type StringPos = {value :: String, pos :: {x :: Number, y :: Number}} 

moveWithButton :: Effect (Event StringPos) 
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

data Final = FA (Array Final)
  | FP Box Box Point 
  | FL Box Box Line 
  | FC Box Box Circle 
  | FH Box Box HalfLine 
  | FS Box Box Segment

instance drawableFinal :: DrawableSet Final where
  drawIn ctx (FP from to p) = drawIn ctx $ remap from p to
  drawIn ctx (FL from to l) = 
    let p = aPointOnLine l
        q = p <+| aVectorOfLine l
      in drawIn ctx $ line (remap from p to) (remap from q to)
  drawIn ctx (FC from to (Circle {center, radius})) =
    let p = remap from center to
        q = remap from (point "" (abs center + radius) (ord center)) to
      in drawIn ctx $ circle p (Geo.length $ vector p q)  
  drawIn ctx (FH from to (HalfLine {origin, direction})) = 
    let p = remap from origin to
        q = remap from (origin <+| direction) to
     in drawIn ctx $ HalfLine {origin: p, direction: vector p q}
  drawIn ctx (FS from to (Segment {origin, extremity,asOriented})) = 
    drawIn ctx $ Segment { origin: remap from origin to
                         , extremity: remap from extremity to
                         , asOriented}
  drawIn ctx (FA arr) = drawIn ctx arr

type State = { functionPlot :: Box -> Box -> Drawing
             , from :: Box
             , to :: Box
             , previousX :: Number
             , previousY :: Number}

grid :: Box -> Box -> Drawing
grid from to = 
  let topX = abs from.center + from.halfWidth
      botX = abs from.center - from.halfWidth
      topY = ord from.center + from.halfHeight
      botY = ord from.center - from.halfHeight
      segAtX x = FS from to $ segment (point "" x botY) (point "" x topY) Nothing
      segAtY y = FS from to $ segment (point "" botX y) (point "" topX y) Nothing
   in  ( foldr (<>) mempty $ 
         (\ n -> drawIn {color: rgb 50 50 50, lineWidth: 0.5} $
           segAtX $ toNumber n) <$> ceil botX .. floor topX)
           <>
      ( foldr (<>) mempty $ 
         (\ n -> drawIn {color: rgb 50 50 50, lineWidth: 0.5} $
           segAtY $ toNumber n) <$> ceil botY .. floor topY)
                 <> (drawIn {color: rgb 50 50 50, lineWidth: 1.5} $ segAtX 0.0)
                 <> (drawIn {color: rgb 50 50 50, lineWidth: 1.5} $ segAtY 0.0)

initialState :: (Final -> Drawing) -> State
initialState draw = 
  { functionPlot: \from to -> grid from to <> plot draw from to inverse 

  , from:  local
  , to: functionDisplay
  , previousX: 0.0
  , previousY: 0.0}

reframe :: State -> Drawing
reframe {functionPlot, from, to, previousX, previousY} =
  functionPlot from to

ePage :: ButtonEvent -> (Final -> Drawing) -> Effect (Behavior Drawing) 
ePage ev draw = liftA1 reframe  <$> 
  (\ event -> 
    unfold (\{value, pos: {x,y}} st -> 
      case value of
        "Clicked" -> st{from{halfWidth = st.from.halfWidth*1.1}}
        "buttonup" -> st{previousX = x, previousY = y}
        "buttondown" -> 
          let p = remap st.to (point "" st.previousX st.previousY) st.from
              q = remap st.to (point "" x y) st.from
           in st{ from{center = st.from.center <+| vector q p}
                , previousX = x
                , previousY = y}
                       
        otherwise -> st
           ) (event `alt` ev.event) (initialState draw)) <$> moveWithButton
   
inInterval :: Number -> Number -> Number -> Boolean
inInterval a b x = a <= x && x <= b

inBox :: Box -> Point -> Boolean
inBox {center, halfWidth, halfHeight} p = 
  inInterval (abs center - halfWidth) (abs center + halfWidth) (abs p) &&
           inInterval (ord center - halfHeight) (ord center + halfHeight) (ord p)

visibleInBox ::  Box -> Segment -> Maybe Segment
visibleInBox = unsafePartial \ b@{center, halfWidth, halfHeight} s@(Segment sCore) ->
  let candidates = filter ((_ == 1) <<< length) $ (s `meets` _) <$> frame b
   in case unit of
       unit | inBox b sCore.origin && inBox b sCore.extremity -> Just s
            | inBox b sCore.origin -> 
                let [[p]] = candidates 
                in Just $ segment sCore.origin p Nothing
            | inBox b sCore.extremity ->
               let [[p]] = candidates 
                in Just $ segment sCore.extremity p Nothing
            | otherwise -> 
                if length candidates == 2 
                  then let [[p],[q]] = candidates in Just $ segment p q Nothing
                  else Nothing

type ButtonEvent = {event :: Event StringPos, push :: StringPos -> Effect Unit}

cb :: forall a. ButtonEvent -> a -> Effect Unit
cb {event, push} ev = do
  push {value: "Clicked", pos: {x: 0.0, y: 0.0}}

main :: Effect Unit
main = do
  setup <- DOM.setup
  canvas <- DOM.getElementById "canvas" setup.document
  context2D <- DOM.getContext2D canvas
  _ <- DOM.setAttribute "width" (show width) canvas
  _ <- DOM.setAttribute "height" (show height) canvas
  
  let ctx = { color: rgb 5 4 9
            , lineWidth: 1.50}

  let white = fillColor $ rgb 255 255 255
  let displayHeight = 2.0 * functionDisplay.halfHeight
  let background = filled white (rectangle 0.0 0.0 width height)
  let outDisplay = filled white (rectangle 0.0   displayHeight 
                              width (height - displayHeight)) 
  button <- DOM.createElement "button" setup.document
  _ <- DOM.setTextContent "Click" button
  _ <- DOM.appendChild button setup.body
  ev <- create
  _ <- DOM.addEventListener (cb ev) DOM.click button
  page <- ePage ev $ (drawIn ctx :: forall a. DrawableSet a => a -> Drawing)
  _ <- animate (pure background 
                <> page 
                <> pure (drawIn ctx $ frame functionDisplay)) 
                   (render context2D)
  pure unit

