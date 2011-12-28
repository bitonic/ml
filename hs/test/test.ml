
s = \x y z -> x z (y z);

k = \x y -> x;

i = s k k;

pair = \x y -> (x, y);

triple = \x y z -> (x, y, z);

curry = \f x y -> f (x, y);

uncurry = \f (x, y) -> f x y;

fst = \(x, y) -> x;

snd = \(x, y) -> y;

foo = let x = 4 in
      let y = 0 in
      plus x y;

data Maybe a =
    Just a
  | Nothing;

fromJust = \(Just x) -> x;

fromMaybe = \x m -> case m of
    Just y -> y
  | Nothing -> x;

return = Just;

bind = \m f -> case m of
    Just x  -> f x
  | Nothing -> Nothing;

join = \m -> case m of
    Just (Just x) -> Just x
  | Nothing       -> Nothing;

subtract : Int -> Int -> Int;
subtract = \x y -> plus x (negate y);

times = \x y -> case isZero y of
    True -> 0
  | False -> plus x (times x (subtract y 1));

fix = \f -> f (fix f);

undefined = fix (\x -> x);

data Fix f = In (f (Fix f));

data Tree t = Leaf Int
            | Branch t t;

leaf = \n -> In (Leaf n);

branch = \l r -> In (Branch l r);

singleton = \i -> leaf i;

leq = \x y -> undefined;

insert = \x (In t) -> case t of
    Leaf y -> (case leq x y of
        True -> branch (leaf x) (leaf y)
      | False -> branch (leaf y) (leaf x))
  | Branch l r -> (case leq x y of
         True -> branch (insert x l) r
       | False -> branch l (insert x r));
