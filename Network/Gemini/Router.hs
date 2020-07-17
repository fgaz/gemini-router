{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Network.Gemini.Router (
-- * The Route monad transformer
  RouteT
, Route
, RouteIO
-- * Running Routes
, runRouteT
, runRouteT'
-- * Building Routes
, end
, dir
, capture
, input
, optionalInput
, custom
-- * Getters
, getRequest
, getPath
) where

import Network.Gemini.Server

import Data.Maybe (fromMaybe)

import Data.Functor.Identity (Identity)
import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))

import Network.URI (uriQuery, pathSegments, unEscapeString)

#if __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail (MonadFail(..))
#endif


-- The RouteT monad transformer
-------------------------------

-- | Represents a way of routing requests through different handlers
newtype RouteT m a = RouteT { runRouteT :: Request -> [String] -> m (Maybe a) }

type Route = RouteT Identity
type RouteIO = RouteT IO

instance Functor f => Functor (RouteT f) where
  fmap f r = RouteT $ \req path -> fmap f <$> runRouteT r req path

instance Applicative f => Applicative (RouteT f) where
  pure x = RouteT $ \_ _ -> pure $ pure x
  f <*> x = RouteT $ \req path ->
    fmap (<*>) (runRouteT f req path) <*> runRouteT x req path

instance Monad m => Monad (RouteT m) where
  rx >>= f = RouteT $ \req path -> do
    mx <- runRouteT rx req path
    runRouteT (maybe (RouteT $ \_ _ -> pure Nothing) f mx) req path

instance MonadTrans RouteT where
  lift = RouteT . const . const . fmap pure

instance MonadIO m => MonadIO (RouteT m) where
  liftIO = RouteT . const . const . fmap pure . liftIO

-- TODO all other transformers instances

instance Monad m => MonadFail (RouteT m) where
  --TODO or maybe we shoudl just throw an exception
  --or is it possible to somehow directly return Response 42 err mempty? it would require early return... like happstack
  fail _ = empty

-- | 'empty' skips to the next route.
-- @r1 '<|>' r2@ means go to @r2@ if @r1@ skips
instance Monad f => Alternative (RouteT f) where
  empty = RouteT $ \_ _ -> pure Nothing
  r1 <|> r2 = RouteT $ \req path -> do
    maybe1 <- runRouteT r1 req path
    maybe2 <- runRouteT r2 req path
    pure $ maybe1 <|> maybe2

-- Running routes
-------------------------------

-- MAYBE swap names with runRouteT
-- | Given a @run@ function for the inner 'Monad', make a 'Handler'
runRouteT' :: (m (Maybe Response) -> IO (Maybe Response)) -- ^ Inner @run@
           -> RouteT m Response
           -> Handler
runRouteT' runM r req = fromMaybe notFound <$> runM (runRouteT r req path)
  where
    notFound = Response 51 "Not found" mempty
    path = unEscapeString <$> pathSegments req

-- Building Routes
-------------------------------

-- | Match on the end of the path
end :: Applicative f
    => RouteT f a -- ^ Route to run
    -> RouteT f a
end r = RouteT $ \req path -> case path of
  [] -> runRouteT r req path
  _ -> pure Nothing

-- | Match on a specific path segment
dir :: Applicative f
    => String -- ^ What the segment must match
    -> RouteT f a -- ^ Route to run on the rest of the path
    -> RouteT f a
dir str r = RouteT $ \req path -> case path of
  frag:rest | frag == str -> runRouteT r req rest
  _                       -> pure Nothing

-- TODO use a parsing class
-- | Match on an arbitrary path segment, and capture it
capture :: Applicative f
        => (String -> RouteT f a) -- ^ Function that takes the segment and
                                  -- returns the route to run on the rest of
                                  -- the path
        -> RouteT f a
capture f = RouteT $ \req path -> case path of
  frag:rest -> runRouteT (f frag) req rest
  _         -> pure Nothing

-- TODO use a parsing class
-- | Require a query string, by asking the client (code 10) if necessary
input :: Applicative f
      => String -- ^ String to return to the client if there is no query string
      -> (String -> RouteT f Response) -- ^ Function that takes the query string
                                       -- and returns the route to run on the
                                       -- rest of the path
      -> RouteT f Response
input q f = RouteT $ \req path -> case uriQuery req of
  '?':query -> runRouteT (f $ unEscapeString query) req path
  _         -> pure $ pure $ Response 10 q mempty

-- | Capture, if present, the query string
optionalInput :: Applicative f
              => (Maybe String -> RouteT f a) -- ^ Function that takes the
                                              -- query string (if present) and
                                              -- returns the route to run on
                                              -- the rest of the path
              -> RouteT f a
optionalInput f = RouteT $ \req path -> case uriQuery req of
  '?':query -> runRouteT (f $ Just $ unEscapeString query) req path
  _         -> runRouteT (f Nothing)                       req path

-- | Build custom routes. Takes a function that takes the request and the
-- remaining path segments and returns the result. A 'Nothing' makes the
-- request fall through to the next route
custom :: (Request -> [String] -> m (Maybe a)) -> RouteT m a
custom = RouteT

-- Getters
-------------------------------

getRequest :: Applicative m => RouteT m Request
getRequest = RouteT $ \req _ -> pure $ Just req

getPath :: Applicative m => RouteT m [String]
getPath = RouteT $ \_ path -> pure $ Just path

