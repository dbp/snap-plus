{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Plus.Handlers where

import           Prelude   hiding ((++))
import           Snap.Plus

data Resource r m = Resource { indexHandlerField    :: m ()
                             , newHandlerField      :: m ()
                             , showHandlerField     :: r -> m ()
                             , editHandlerField     :: r -> m ()
                             , deleteHandlerField   :: r -> m ()
                             , moreMemberRoutes     :: [(Text, r -> m ())]
                             , moreCollectionRoutes :: [(Text, m ())]}

class MonadSnap m => ResourceAble r m where
  replace :: r -> m ()
  delete :: r -> m ()
  get :: Int -> m (Maybe r)

replaceEntity :: ResourceAble r m => r -> m ()
replaceEntity record = replace record

deleteEntity :: ResourceAble r m => r -> m ()
deleteEntity = delete

routeResource :: ResourceAble r m => Resource r m -> m ()
routeResource = route . resourceRoutes

resourceRoutes :: ResourceAble r m => Resource r m -> [(Text, m ())]
resourceRoutes (Resource indexHandler newHandler showHandler
                         editHandler deleteHandler member
                         collection) =
   (("", ifTop indexHandler) : collection)
   ++ [("new", newHandler)]
   ++ [(":id", do entity <- requestedEntity
                  routeWithEntity entity $
                    [("", ifTop . showHandler)
                    ,("edit", editHandler)
                    ,("delete", deleteHandler)]
                    ++ member)]
  where
    routeWithEntity entity = route . map (withEntity entity)
    withEntity entity (pathPart, handler) = (pathPart, handler entity)

requestedEntity :: ResourceAble r m => m r
requestedEntity = do
  key <- requireParam "id"
  require $ get key

home :: MonadSnap m => m ()
home = redirect "/"
