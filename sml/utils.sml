
structure Utils :> UTILS =
struct
    structure L = List

    fun intersperse _ [] = []
      | intersperse _ [x] = [x]
      | intersperse x (y :: ys) = y :: x :: intersperse x ys

    fun lookup x l = case L.find (fn (y, _) => x = y) l
                      of SOME (_, el) => SOME el
                       | NONE         => NONE

    fun sort cmp xs =
        let
            fun descending a as' (b :: bs) =
                if not (cmp a b) then descending b (a :: as') bs
                else (a :: as') :: sequences (b :: bs)
              | descending a as' bs = (a :: as') :: sequences bs

            and ascending a as' (b :: bs) =
                if cmp a b then ascending b (fn ys => as' (a :: ys)) bs
                else as' [a] :: sequences (b :: bs)
              | ascending a as' bs = as' [a] :: sequences bs

            and sequences (a :: b :: xs) =
                if not (cmp a b) then descending b [a] xs
                else ascending b (fn xs' => a :: xs') xs
              | sequences xs = [xs]

            fun merge (as' as (a :: as'')) (bs as (b :: bs')) =
                if not (cmp a b) then b :: merge as' bs'
                else a :: merge as'' bs
              | merge [] bs = bs
              | merge as' [] = as'

            fun mergePairs (a :: b :: xs) = merge a b :: mergePairs xs
              | mergePairs xs = xs

            fun mergeAll [x] = x
              | mergeAll xs = mergeAll (mergePairs xs)
        in
            mergeAll (sequences xs)
        end

    fun elem x = L.exists (fn y => x = y)

    fun difference l1 l2 = L.filter (fn x => not (elem x l2)) l1

    fun nub [] = []
      | nub (x :: xs) = x :: (nub (L.filter (fn y => not (x = y)) xs))

    fun curry f a b = f (a, b)

    fun uncurry f (a, b) = f a b

    fun id x = x

    fun const x _ = x

    fun flip f x y = f y x
end
