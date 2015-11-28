module Maps.Util where

range : Int -> Int -> List Int
range x y =
  if x > y then
     []
  else
    let
      zeros = List.repeat (y - x + 1) 0
    in
      List.indexedMap (\i _ -> x + i) zeros

pairRange : (Int, Int) -> (Int, Int) -> List (Int, Int)
pairRange (x1, y1) (x2, y2) =
  let
    r1 = range x1 x2
    r2 = range y1 y2

    mapper a = List.map (\b -> (a,b)) r2
  in
    List.concatMap mapper r1
