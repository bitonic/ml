
let s = \x y z -> x z (y z)

let k = \x y -> x

let i = s k k

let pair = \x y -> (x, y)

let triple = \x y z -> (x, y, z)

let curry = \f x y -> f (x, y)

let uncurry = \f (x, y) -> f x y

let fst = \(x, y) -> x

let snd = \(x, y) -> y

let foo = let x = 4 in
          let y = 0 in
          plus x y

data Maybe a =
    Just a
  | Nothing

-- let fromJust = \(Just x) -> x

-- let fromMaybe = \x m -> case m of
--                             Just y -> y
--                           | Nothing -> x

-- let return = Just

-- let bind = \m f -> case m of
--                      Just x  -> f x
--                    | Nothing -> Nothing

-- let join = \m -> case m of
--                     Just (Just x) -> Just x
--                   | Nothing       -> Nothing

-- let subtract = \x y -> plus x (negate y)

-- let times = \x y -> case isZero y of
--                        True -> 0
--                      | False -> plus x (times x (subtract y 1))
