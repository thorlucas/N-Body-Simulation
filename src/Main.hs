module Main where

type Vector = (Float, Float)
type Pos = Vector
type Vel = Vector
type Acc = Vector
type Mass = Float
data Particle = Particle Mass Pos Vel Acc deriving (Eq, Show)

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

updateParticle :: [Particle] -> Particle -> Particle
updateParticle ps p@(Particle m pos vel acc) =
    let accs = map (gravitate p) ps
        acc' = foldr (\(accx, accy) (x, y) -> (accx + x, accy + y)) (0, 0) accs
        vel' = (fst vel + fst acc, snd vel + snd acc)
        pos' = (fst pos + fst vel, snd pos + snd vel)
    in  Particle m pos' vel' acc'

step :: [Particle] -> [Particle]
step ps = map (updateParticle ps) ps

main :: IO ()
main = putStrLn "Hello, Haskell!"
