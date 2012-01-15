
--------------------------------------------------------------------------------
-- |
-- Module      : Network.OpenID.Normalization
-- Copyright   : (c) Trevor Elliott, 2008
-- License     : BSD3
--
-- Maintainer  : Trevor Elliott <trevor@geekgateway.com>
-- Stability   : 
-- Portability : 
--

module Network.OpenID.Normalization where

-- Friends
import Network.OpenID.Types

-- Libraries
import Control.Applicative
import Control.Monad
import Data.List
import Network.URI hiding (scheme,path)


-- | Normalize an identifier.
normalizeIdentifier :: Identifier -> Maybe Identifier
normalizeIdentifier (Identifier str)
  | null str                  = Nothing
  | "xri://" `isPrefixOf` str = Identifier `fmap` xri str
  | head str `elem` "=@+$!"   = Identifier `fmap` xri str
  | otherwise = fmt `fmap` (url >>= norm)
  where
    url = parseURI str <|> parseURI ("http://" ++ str)

    norm uri = validScheme >> return u
      where
        scheme      = uriScheme uri
        validScheme = guard (scheme == "http:" || scheme == "https:")
        u = uri { uriFragment = "", uriPath = path }
        path | null (uriPath uri) = "/"
             | otherwise          = uriPath uri

    fmt u = Identifier
          $ normalizePathSegments
          $ normalizeEscape
          $ normalizeCase
          $ uriToString (const "") u []

-- |Dummy XRI resolver by using xri.net.
xri :: String -> Maybe String
xri x = Just $ "https://xri.net/" ++ x
