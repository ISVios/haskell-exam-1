module Test where


f :: (a -> (Maybe b)) -> (Maybe a) -> (Maybe b)
f g (Just a) = g a
