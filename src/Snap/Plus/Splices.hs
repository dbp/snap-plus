{-# LANGUAGE OverloadedStrings #-}
module Snap.Plus.Splices where

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Heist
import qualified Heist.Interpreted        as I
import           Heist.Splices.BindStrict
import           Heist.Splices.Ignore
import qualified Text.XmlHtml             as X

import           Snap.Plus

siteSplices :: MonadSnap m => Splices (I.Splice m)
siteSplices = do "prefix-url" ## prefixUrlSplice
                 "suffix-url" ## suffixUrlSplice
                 "currentPath" ## pathSplice
                 bindStrictTag ## bindStrictImpl
                 ignoreTag ## ignoreImpl

prefixUrlSplice :: MonadSnap m => I.Splice m
prefixUrlSplice = do node <- getParamNode
                     case X.getAttribute "url" node of
                       Nothing -> return []
                       Just u -> lift $ ifIsUrl u (return $ X.elementChildren node) (return [])

suffixUrlSplice :: MonadSnap m => I.Splice m
suffixUrlSplice =
  do node <- getParamNode
     case X.getAttribute "url" node of
       Nothing -> return []
       Just u -> do url <- fmap (T.takeWhile (/= '?') . T.decodeUtf8 . rqURI) (lift getRequest)
                    return $ if u `T.isSuffixOf` url
                              then X.elementChildren node
                              else []

pathSplice :: MonadSnap m => I.Splice m
pathSplice = do path' <- lift getCurrentPath
                return [X.TextNode path']
