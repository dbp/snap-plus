{-# LANGUAGE OverloadedStrings #-}

module Snap.Plus.Paths where

import           Prelude   hiding ((++))
import           Snap.Plus

class Paths k where
  indexPath :: k -> Text
  getKey :: k -> Int

showPath :: Paths k => k -> Text
showPath k = (indexPath k) ++ tshow (getKey k)

editPath :: Paths k => k -> Text
editPath k = showPath k ++ "/edit"

deletePath :: Paths k => k -> Text
deletePath k = showPath k ++ "/delete"

newPath :: Paths k => k -> Text
newPath k = indexPath k ++ "/new"
