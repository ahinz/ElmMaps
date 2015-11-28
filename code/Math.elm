module Math where

ln : Float -> Float
ln = logBase e

exp : Float -> Float
exp = (^) e

deg2rad : Float -> Float
deg2rad = (*) (pi / 180.0)

rad2deg : Float -> Float
rad2deg = (*) (180.0 / pi)
