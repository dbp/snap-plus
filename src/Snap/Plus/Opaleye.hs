{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Snap.Plus.Opaleye (
    module Karamaan.Opaleye.Reexports
  , module Karamaan.Opaleye.Table
  , module Karamaan.Opaleye.SQL
  , module Karamaan.Opaleye.RunQuery
  , module Control.Category
  , module Control.Arrow
  , module Database.HaskellDB.Query
  , module Data.Profunctor
  , module Data.Profunctor.Product
  , module Data.Profunctor.Product.Default
  , module Data.Profunctor.Product.TH
  , module Karamaan.Opaleye.Wire
  , module Karamaan.Opaleye.Order
  , module Karamaan.Opaleye.ExprArr
  , module Karamaan.Opaleye.QueryArr
  , module Karamaan.Opaleye.MakeExpr
  , module Karamaan.Opaleye.Manipulation
  , showQuery
  , restrictNullable
  , I
  , MaybeWire
  , Con
  , econstant
  , eeq
  , runO
  , delO
  , insO
  , insOR
  , updO
  , onot
  ) where

import           Prelude                         hiding (not)

import           Control.Applicative
import           Control.Category                ((<<<))
import           Data.Text                       (Text, unpack)
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Clock.POSIX           (utcTimeToPOSIXSeconds)
import           Data.Time.LocalTime             (LocalTime, localTimeToUTC,
                                                  utc)
import           System.Time                     (ClockTime (TOD), toUTCTime)

import           Control.Arrow                   (arr, returnA, second)
import           Karamaan.Opaleye.ExprArr        (Expr, ExprArr)
import qualified Karamaan.Opaleye.ExprArr        as E
import           Karamaan.Opaleye.MakeExpr       (makeExpr, makeJustExpr,
                                                  makeMaybeExpr)
import           Karamaan.Opaleye.Manipulation   (Assocer, AssocerE,
                                                  TableExprRunner,
                                                  TableMaybeWrapper,
                                                  runDeleteConnDef,
                                                  runInsertConnDef,
                                                  runInsertReturningConnDef,
                                                  runUpdateConnDef)
import           Karamaan.Opaleye.Operators2     (not)
import           Karamaan.Opaleye.Order
import           Karamaan.Opaleye.QueryArr       (Query)
import           Karamaan.Opaleye.Reexports      hiding (sum)
import           Karamaan.Opaleye.RunQuery       (QueryRunner, fieldQueryRunner)
import           Karamaan.Opaleye.SQL            (showSqlForPostgresDefault)
import           Karamaan.Opaleye.Table          (Table (Table), makeTableDef,
                                                  queryTable)
import           Karamaan.Opaleye.Unpackspec     (Unpackspec)
import           Karamaan.Opaleye.Wire           (Wire (Wire))


import           Database.HaskellDB.PrimQuery    (Literal (DateLit))
import           Database.HaskellDB.Query        (ShowConstant (..))

import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default (Default, def)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)


import           Data.Pool                       (withResource)
import           Database.PostgreSQL.Simple      (Connection)
import           Snap                            (liftIO)
import           Snap.Snaplet.PostgresqlSimple   hiding (Query)

import           Data.Int                        (Int64)


-- NOTE(dbp 2014-08-02): This is a REALLY dumb aspect of the current
-- API. There are identically named functions for Exprs and Queries,
-- meaning there CANT be a single export module (without renaming,
-- which is what we're doing here).
econstant = E.constant
eeq = E.eq

onot = not

type I a = a
type MaybeWire a = Maybe (Wire a)
type Con s a = s

withPgConn :: (Functor m, HasPostgres m) => (Connection -> m a) -> m a
withPgConn f  =  do pool <- pgPool <$> getPostgresState
                    withResource pool f

runO :: (HasPostgres m, Functor m, Default QueryRunner a b) => Query a -> m [b]
runO q = withPgConn $ \con -> liftIO $ runQuery def q con

delO :: (HasPostgres m, Functor m, Default TableExprRunner t t) =>
        Table t -> ExprArr t (Wire Bool) -> m Int64
delO t e = withPgConn $ \con -> liftIO $ runDeleteConnDef con t e

insO :: (HasPostgres m, Functor m,
         Default (PPOfContravariant Assocer) t' t',
         Default TableMaybeWrapper t t')
     => Table t -> Expr t' -> m Int64
insO t e = withPgConn $ \con -> liftIO $ runInsertConnDef con t e

insOR :: (HasPostgres m, Functor m,
          Default (PPOfContravariant Assocer) maybeWires maybeWires,
          Default TableMaybeWrapper wires maybeWires,
          Default TableExprRunner wires wires,
          Default (PPOfContravariant AssocerE) resultWires resultWires,
          Default QueryRunner resultWires haskells) =>
          Table wires
          -> Expr maybeWires
          -> ExprArr wires resultWires
          -> m [haskells]
insOR t e r = withPgConn $ \con -> liftIO $ runInsertReturningConnDef con t e r


updO ::  (HasPostgres m, Functor m,
          Default TableExprRunner t t,
          Default (PPOfContravariant Assocer) t' t',
          Default TableMaybeWrapper t t') =>
     Table t -> ExprArr t t' -> ExprArr t (Wire Bool)
       -> m Int64
updO t e e' = withPgConn $ \con -> liftIO $ runUpdateConnDef con t e e'

instance ShowConstant Text where
  showConstant = showConstant . unpack

instance ShowConstant Int64 where
  showConstant = showConstant . (fromIntegral :: Int64 -> Int)

-- NOTE(dbp 2014-04-03): Ridiculous conversion because HaskellDB uses deprecated old-time library.
instance ShowConstant UTCTime where
  showConstant = DateLit . toUTCTime . uncurry TOD . second truncate
                         . properFraction . utcTimeToPOSIXSeconds

instance ShowConstant LocalTime where
  showConstant = DateLit . toUTCTime . uncurry TOD . second truncate
                         . properFraction . utcTimeToPOSIXSeconds . localTimeToUTC utc

showQuery :: Default (PPOfContravariant Unpackspec) a a
          => Query a
          -> IO ()
showQuery = putStrLn . show . showSqlForPostgresDefault

restrictNullable :: QueryArr (Wire a, Wire (Nullable a)) (Wire a)
restrictNullable = proc (d, i) -> do restrict <<< not <<< isNull -< i
                                     fromNullable -< (d, i)
