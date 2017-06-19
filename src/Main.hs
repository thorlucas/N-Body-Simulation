module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

type Pos = Point
type Vel = Vector
type Acc = Vector
type Mass = Float
data Particle = Particle Mass Pos Vel Acc deriving (Eq, Show)

type Model = [Particle]

g :: Float
g = 5

gravitate :: Particle -> Particle -> Acc
gravitate p1@(Particle _ (px1, py1) _ _) p2@(Particle m2 (px2, py2) _ _)
    | p1 == p2 = (0, 0)
    | otherwise = 
    let dx = px2 - px1
        dy = py2 - py1
        d  = sqrt $ dx^2 + dy^2
        theta = atan2 dy dx
        a  = m2 * g / d^2
    in  (a * cos theta, a * sin theta)

updateParticle :: Model -> Particle -> Particle
updateParticle ps p@(Particle m pos vel acc) =
    let accs = map (gravitate p) ps
        acc' = foldr (\(accx, accy) (x, y) -> (accx + x, accy + y)) (0, 0) accs
        vel' = (fst vel + fst acc, snd vel + snd acc)
        pos' = (fst pos + fst vel, snd pos + snd vel)
    in  Particle m pos' vel' acc'

step :: ViewPort -> Float -> Model -> Model
step _ _ ps = map (updateParticle ps) ps

render :: Model -> Picture
render ps = pictures $ map renderParticle ps
    where
        renderParticle :: Particle -> Picture
        renderParticle (Particle m (x, y) _ _) =
            let r = sqrt $ m / pi
            in  translate x y $ color white $ circleSolid r

main :: IO ()
main = let window = InWindow "N-Body Simulation" (640, 480) (100, 100)
           newModel = [Particle 10 (0, 0) (0, 0) (0, 0),
                       Particle 30 ((-150), (-50)) (0, 0) (0, 0),
                       Particle 10 ((-100), (-300)) (0, 0) (0, 0),
                       Particle 20 (100, (-150)) (0, 0) (0, 0)]
       in  simulate window black 60 newModel render step
