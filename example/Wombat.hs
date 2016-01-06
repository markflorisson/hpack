module Wombat
(TWombat, makeWombat, getWeight)
where

data TWombat = TWombat Int

makeWombat :: Int -> TWombat
makeWombat = TWombat

getWeight :: TWombat -> Int
getWeight (TWombat w) = w
