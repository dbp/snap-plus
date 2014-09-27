{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Main where


----------------------------------------------------------
-- Section 0: Imports.                                  --
----------------------------------------------------------
import           Control.Applicative  ((<$>), (<*>))
import           Control.Lens
import           Data.ByteString      (ByteString)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.DNS.Resolver
import           Test.Hspec
import           Test.Hspec.Snap
import           Text.Digestive

import           Snap.Plus
import           Snap.Plus.Forms


----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------
data App = App { _dns :: ResolvSeed }
makeLenses ''App
type AppHandler = Handler App App
instance HasDns AppHandler where
  getDnsSeed = use dns


routes :: [(Text, AppHandler ())]
routes = []

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    ns <- liftIO $ makeResolvSeed defaultResolvConf
    return (App ns)

main :: IO ()
main = hspec $ do
  describe "network-tests" $ snap (route routes) app $ do
    describe "email validation using presence of dns MX records" $
      do it "should validate @gmail.com" $
           form (Value (Just "a@gmail.com"))
                (emailFormSingle Nothing)
                (M.fromList [("address", "a@gmail.com")])
         it "should not validate @example.com" $
           form (ErrorPaths ["address"])
                (emailFormSingle Nothing)
                (M.fromList [("address", "e@example.com")])
