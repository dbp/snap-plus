module Snap.Snaplet.PostgresqlSimple.Plus ( module Snap.Snaplet.PostgresqlSimple
                                          , singleQuery
                                          , singleQuery'
                                          , idQuery
                                          , numberQuery
                                          , numberQuery'
  ) where

import           Control.Monad                 (join)
import           Data.Maybe                    (fromJust, listToMaybe)
import           Snap.Snaplet.PostgresqlSimple

singleQuery :: (HasPostgres m, Functor m, ToRow q, FromRow r) => Query -> q -> m (Maybe r)
singleQuery stmt attrs = fmap listToMaybe $ query stmt attrs

singleQuery' :: (HasPostgres m, Functor m, FromRow r) => Query -> m (Maybe r)
singleQuery' stmt = fmap listToMaybe $ query_ stmt

idQuery :: (HasPostgres m, Functor m, ToRow q) => Query -> q -> m (Maybe Int)
idQuery stmt attrs = fmap (join . fmap listToMaybe . listToMaybe) $ query stmt attrs

numberQuery :: (HasPostgres m, Functor m, ToRow q) => Query -> q -> m Int
numberQuery q attrs = fmap (head.fromJust) $ singleQuery q attrs

numberQuery' :: (HasPostgres m, Functor m) => Query -> m Int
numberQuery' q = fmap (head.fromJust) $ singleQuery' q
