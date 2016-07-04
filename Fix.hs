-- 2016-06-29
-- OPLSS 2016
-- whoa, isomorphic to omega!
newtype Self t = Fold { unfold :: Self t -> t}

fix :: (a -> a) -> a
fix f = unroll . Fold $ f . unroll

unroll :: Self t -> t
unroll e = unfold e e
