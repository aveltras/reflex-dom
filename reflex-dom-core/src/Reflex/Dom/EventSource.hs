{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.EventSource
  ( module Reflex.Dom.EventSource
  ) where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Reflex.Class
import Reflex.Dom.EventSource.Foreign
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad hiding (forM, mapM, mapM_, sequence)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Default
import Data.IORef
import Data.Maybe (isJust)
import Data.Text
import GHCJS.DOM.EventSource (getReadyState)
import GHCJS.DOM.Types (runJSM, askJSM, MonadJSM, liftJSM, JSM)
import GHCJS.Marshal
import qualified Language.Javascript.JSaddle.Monad as JS (catch)

data EventSourceConfig t
    = EventSourceConfig { _eventSourceConfig_close :: Event t ()
                        , _eventSourceConfig_reconnect :: Bool
                        }

instance Reflex t => Default (EventSourceConfig t) where
  def = EventSourceConfig never True

data EventSourceMessage a
    = EventSourceMessage { _eventSourceMessage_data :: a
                         , _eventSourceMessage_eventType :: Text
                         , _eventSourceMessage_lastEventId :: Text
                         }

type EventSource t = RawEventSource t ByteString

data RawEventSource t a
    = RawEventSource { _eventSource_recv :: Event t (Either ByteString (EventSourceMessage a))
                     , _eventSource_open :: Event t ()
                     , _eventSource_error :: Event t ()
                     }

eventSource :: (MonadJSM m, MonadJSM (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m)
             => Text -> EventSourceConfig t -> m (EventSource t)
eventSource url config = do
  (eRecv, onMessage) <- newTriggerEvent
  (eOpen, triggerEOpen) <- newTriggerEventWithOnComplete
  (eError, triggerEError) <- newTriggerEvent
  currentEventSourceRef <- liftIO $ newIORef Nothing
  payloadQueue <- liftIO newTQueueIO
  isOpen       <- liftIO newEmptyTMVarIO
  let onOpen = triggerEOpen () $ liftIO $ void $ atomically $ tryPutTMVar isOpen ()
      onError = triggerEError ()
      start = do
        es <- newEventSource url
          (onBSMessage >=> liftIO . onMessage . fmap toEventSourceMessage)
          (liftIO onOpen)
          (liftIO onError)
        liftIO $ writeIORef currentEventSourceRef $ Just es
        return ()

  performEvent_ . (liftJSM start <$) =<< getPostBuild
  performEvent_ $ ffor (_eventSourceConfig_close config) $ \_ -> liftJSM $ do
    mes <- liftIO $ readIORef currentEventSourceRef
    case mes of
      Nothing -> return ()
      Just es -> do
        closeEventSource es
        liftIO $ writeIORef currentEventSourceRef Nothing

  return $ RawEventSource eRecv eOpen eError

  where
    toEventSourceMessage jsMsg = EventSourceMessage
      { _eventSourceMessage_data = _eventSourceJSMessage_data jsMsg
      , _eventSourceMessage_eventType = _eventSourceJSMessage_eventType jsMsg
      , _eventSourceMessage_lastEventId = _eventSourceJSMessage_lastEventId jsMsg
      }

forkJSM :: JSM () -> JSM ()
forkJSM a = do
  jsm <- askJSM
  void $ liftIO $ forkIO $ runJSM a jsm

-- #ifdef USE_TEMPLATE_HASKELL
-- makeLensesWith (lensRules & simpleLenses .~ True) ''EventSourceConfig
-- makeLensesWith (lensRules & simpleLenses .~ True) ''EventSource
-- #else

-- eventSourceConfig_close :: Lens' (EventSourceConfig t) (Event t ())
-- eventSourceConfig_close f (EventSourceConfig x1 x2) = (\y -> EventSourceConfig y x2) <$> f x1
-- {-# INLINE eventSourceConfig_close #-}

-- eventSourceConfig_reconnect :: Lens' (EventSourceConfig t) Bool
-- eventSourceConfig_reconnect f (EventSourceConfig x1 x2) = (\y -> EventSourceConfig x1 y) <$> f x2
-- {-# INLINE eventSourceConfig_reconnect #-}

-- eventSourceMessage_data :: Lens' (EventSourceMessage a) a
-- eventSourceMessage_data f (EventSourceMessage x1 x2 x3) = (\y -> EventSourceMessage y x2 x3) <$> f x1
-- {-# INLINE eventSourceMessage_data #-}

-- eventSourceMessage_eventType :: Lens' (EventSourceMessage a) Text
-- eventSourceMessage_eventType f (EventSourceMessage x1 x2 x3) = (\y -> EventSourceMessage x1 y x3) <$> f x2
-- {-# INLINE eventSourceMessage_eventType #-}

-- eventSourceMessage_lastEventId :: Lens' (EventSourceMessage a) Text
-- eventSourceMessage_lastEventId f (EventSourceMessage x1 x2 x3) = (\y -> EventSourceMessage x1 x2 y) <$> f x3
-- {-# INLINE eventSourceMessage_lastEventId #-}

-- eventSource_recv :: Lens' (EventSource t) (Event t (EventSourceMessage a))
-- eventSource_recv f (EventSource x1 x2 x3) = (\y -> EventSource y x2 x3) <$> f x1
-- {-# INLINE eventSource_recv #-}

-- eventSource_open :: Lens' (EventSource t) (Event t ())
-- eventSource_open f (EventSource x1 x2 x3) = (\y -> EventSource x1 y x3) <$> f x2
-- {-# INLINE eventSource_open #-}

-- eventSource_error :: Lens' (EventSource t) (Event t ())
-- eventSource_error f (EventSource x1 x2 x3) = (\y -> EventSource x1 x2 y) <$> f x3
-- {-# INLINE eventSource_error #-}

-- #endif
