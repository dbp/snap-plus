{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Plus ( module Snap
                 , module Control.Applicative
                 , module Data.Text
                 , module Snap.Extras.CoreUtils
                 , (++)
                 , io
                 , tshow
                 , tNotNull
                 , readSafe
                 , Paramable(..)
                 , getParam
                 , requireParam
                 , getParamOr
                 , require
                 , require'
                 , getCurrentPath
                 , ifIsUrl
                 , matchesUrl
                 , fromMaybe
                 , redirect
                 , route
                 , addRoutes
                 , format
                 , routeFormats
                 , Format (..)
                 ) where

import           Control.Applicative   ((<$>), (<*>))
import           Control.Arrow         (first)
import           Control.Monad         (MonadPlus, join, mzero, unless)
import           Control.Monad.Trans   (MonadIO, liftIO)
import           Data.Maybe
import           Data.Monoid           (Monoid, mappend)
import           Data.Text             (Text, pack, toLower, unpack)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Prelude               hiding ((++))
import           Snap                  hiding (addRoutes, get, getParam,
                                        redirect, route)
import qualified Snap
import qualified Snap.Core
import           Snap.Extras.CoreUtils hiding (getParam')

(++) :: Monoid d => d -> d -> d
(++) = mappend

ifIsUrl :: MonadSnap m => Text -> m a -> m a -> m a
ifIsUrl u is not' = do matches <- matchesUrl u
                       if matches
                        then is
                        else not'

matchesUrl :: MonadSnap m => Text -> m Bool
matchesUrl u = do url <- fmap rqURI getRequest
                  return $ u `T.isPrefixOf` T.decodeUtf8 url

io :: MonadIO m => IO a -> m a
io = liftIO

tshow :: Show a => a -> Text
tshow = T.pack . show

tNotNull :: Text -> Bool
tNotNull = not.T.null

readSafe :: Read a => Text -> Maybe a
readSafe = fmap fst . listToMaybe . reads . T.unpack

class Paramable t where
  parseParamable :: Text -> Maybe t

instance Paramable Text where
  parseParamable = Just
instance Paramable Int where
  parseParamable = readSafe

requireParam :: (MonadSnap m, Paramable t) => Text -> m t
requireParam name = do param <- require $ Snap.getParam $ T.encodeUtf8 name
                       require' $ parseParamable $ T.decodeUtf8 param

getParam' :: MonadSnap m => Text -> m (Maybe Text)
getParam' name = do param <- Snap.getParam $ T.encodeUtf8 name
                    return (T.decodeUtf8 <$> param)

getParam :: (MonadSnap m, Paramable t) => Text -> m (Maybe t)
getParam name = do param <- getParam' name
                   case param of
                     Nothing -> return Nothing
                     Just p -> return $ parseParamable p

getParamOr :: (MonadSnap m, Paramable t) => t -> Text -> m t
getParamOr def name = do mVal <- getParam' name
                         return $ fromMaybe def $ join $ fmap parseParamable mVal

require :: MonadPlus m => m (Maybe a) -> m a
require ma = do a' <- ma
                case a' of
                  Nothing -> mzero
                  Just a -> return a

require' :: MonadPlus m => Maybe a -> m a
require' = require . return

getCurrentPath :: MonadSnap m => m Text
getCurrentPath = fmap ( T.decodeUtf8 . urlEncode . T.encodeUtf8
                      . fst . T.breakOn "?" . T.decodeUtf8 . rqURI) getRequest

redirect :: MonadSnap m => Text -> m a
redirect = Snap.Core.redirect . T.encodeUtf8

route :: MonadSnap m => [(Text, m a)] -> m a
route = Snap.Core.route . map (first T.encodeUtf8)

addRoutes :: [(Text, Handler b v ())] -> Initializer b v ()
addRoutes = Snap.addRoutes . map (first T.encodeUtf8)

data Format = HTML | JSON | JS | Other Text | NotSpecified deriving (Eq,Show,Read,Ord)
instance Paramable Format where
  parseParamable = Just . fromMaybe NotSpecified . readSafe . T.toUpper

format :: MonadSnap m => [Format] -> m a -> m a
format providedFormats action = do
  requestedFormat <- getParamOr NotSpecified "format"
  unless (requestedFormat `elem` providedFormats) pass
  action

routeFormats :: MonadSnap m => [([Format], m a)] -> m a
routeFormats = route . map r
  where r (f, action) = ("", format f action)
