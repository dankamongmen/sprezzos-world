module Resolver.Util where

while :: Monad m => m Bool -> m a -> a -> m a
while cond body dflt =
    do ok <- cond
       (if ok
        then (body >>= while cond body)
        else return dflt)
