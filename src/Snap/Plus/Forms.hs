{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Snap.Plus.Forms where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Network.DNS.Lookup
import           Network.DNS.Resolver
import           Network.DNS.Types
import           Text.Digestive

import           Snap.Plus

class (Monad m, MonadIO m) => HasDns m where
  getDnsSeed :: m ResolvSeed

requiredForm :: Monad m => Text -> Form Text m (Maybe a) -> Form Text m a
requiredForm msg = validate (maybe (Error msg) Success)

nameForm :: Monad m => Maybe Text -> Form Text m Text
nameForm = nonEmpty . text

emailForm :: HasDns m => Maybe Text -> Form Text m (Maybe Text)
emailForm t = emailDnsCheck $ fst <$>
              (matching $ (,) <$> "address" .: emailValidateSimple (optionalText t)
                              <*> "confirm" .: emailValidateSimple (optionalText t))
  where matching = check "Email addresses do not match."
                         (maybe True (uncurry (==)) . uncurry (liftM2 (,)))

emailFormSingle :: HasDns m => Maybe Text -> Form Text m (Maybe Text)
emailFormSingle t = emailDnsCheck $ "address" .: emailValidateSimple (optionalText t)

emailValidateSimple :: Monad m
                    => Form Text m (Maybe Text)
                    -> Form Text m (Maybe Text)
emailValidateSimple = check "Email address not valid (missing @)." (maybe True ("@" `T.isInfixOf`))

emailDnsCheck :: HasDns m
              => Form Text m (Maybe Text)
              -> Form Text m (Maybe Text)
emailDnsCheck = checkM "Email address domain (after the @) not valid." $ maybe (return True) $ \e ->
                       do seed <- getDnsSeed
                          let host = T.encodeUtf8 (T.drop 1 (snd (T.breakOn "@" e)))
                          res <- liftIO $ withResolver seed (`lookupMX` host)
                          case res of
                            Left err -> case err of
                                          IllegalDomain -> return False
                                          NameError -> return False
                                          _ -> return True
                            Right [] -> return False
                            _ -> return True

passwordForm :: Monad m => Form Text m Text
passwordForm = nonEmptyTextForm

nonEmpty :: Monad m => Form Text m Text -> Form Text m Text
nonEmpty = check "Must not be blank" tNotNull

nonEmptyTextForm :: Monad m => Form Text m Text
nonEmptyTextForm = nonEmpty (text Nothing)


slugForm :: Monad m => Formlet Text m Text
slugForm t = T.toLower <$> check "Cannot have spaces" (not . T.isInfixOf " ") (text t)


deleteForm :: Monad m => Text -> Form Text m Bool
deleteForm t = snd <$> ((,) <$> "prompt" .: text (Just t)
                            <*> "confirm" .: bool Nothing)

numericTextForm :: Monad m => Form Text m Text
numericTextForm = check "Must be all numbers" ((all isDigit).T.unpack) (text Nothing)
