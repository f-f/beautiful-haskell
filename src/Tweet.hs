{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Tweet where

import           Control.Applicative
import qualified Data.ByteString.Char8 as S8
import           Data.Monoid
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.Environment
import qualified Web.Twitter.Conduit   as Twitter


-- Stuff copied mostly from the library examples:
-- https://github.com/himura/twitter-conduit/tree/master/sample


getOAuthTokens :: IO (Twitter.OAuth, Twitter.Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
    let oauth = Twitter.twitterOAuth
            { Twitter.oauthConsumerKey = consumerKey
            , Twitter.oauthConsumerSecret = consumerSecret
            }
        cred = Twitter.Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where
    getEnv' = (S8.pack <$>) . getEnv

getTWInfoFromEnv :: IO Twitter.TWInfo
getTWInfoFromEnv = do
    (oa, cred) <- getOAuthTokens
    return $ Twitter.setCredential oa cred Twitter.def

tweet :: T.Text -> IO ()
tweet status = do
    T.putStrLn $ "Post message: " <> status
    twInfo <- getTWInfoFromEnv
    mgr <- Twitter.newManager Twitter.tlsManagerSettings
    res <- Twitter.call twInfo mgr $ Twitter.update status
    print res
