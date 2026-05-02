{-# LANGUAGE CPP #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# LANGUAGE LambdaCase #-}

module Reflex.Dom.EventSource.Foreign
  ( module Reflex.Dom.EventSource.Foreign
  , JSVal
  ) where

import Prelude hiding (all, concat, concatMap, div, mapM, mapM_, sequence, span)

import Control.Lens ((^.))
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHCJS.DOM.Types (JSM, JSVal, liftJSM, fromJSValUnchecked)
import GHCJS.DOM.MessageEvent
import Foreign.JavaScript.Utils (bsFromMutableArrayBuffer, bsToArrayBuffer)
import GHCJS.Foreign (JSType(..), jsTypeOf)
import Language.Javascript.JSaddle (fun, new, jsg, js0, js2, jss, js1)
import Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import Language.Javascript.JSaddle.Types (ghcjsPure)

newtype JSEventSource = JSEventSource { unEventSource :: JSVal }

data EventSourceJSMessage a
  = EventSourceJSMessage
    { _eventSourceJSMessage_data :: a
    , _eventSourceJSMessage_eventType :: Text
    , _eventSourceJSMessage_lastEventId :: Text
    }

closeEventSource :: JSEventSource -> JSM ()
closeEventSource (JSEventSource es) = void $ es ^. js0 "close"

newEventSource
  :: Text -- url
  -> (Either ByteString (EventSourceJSMessage JSVal) -> JSM ()) -- onmessage
  -> JSM () -- onopen
  -> JSM () -- onerror
  -> JSM JSEventSource
newEventSource url onMessage onOpen onError = do
  let onOpenWrapped = fun $ \_ _ _ -> onOpen
      onErrorWrapped = fun $ \_ _ _ -> onError
      onMessageWrapped = fun $ \_ _ (e:_) -> do
        let e' = MessageEvent e
        d <- getData e'
        liftJSM $ ghcjsPure (jsTypeOf d) >>= \case
          String -> onMessage $ Right $ EventSourceJSMessage
            { _eventSourceJSMessage_data = d
            , _eventSourceJSMessage_eventType = T.pack "message" -- TODO: eventType
            , _eventSourceJSMessage_lastEventId = T.pack "event-id" -- TODO: lastEventId
            }
          _ -> do
            ab <- mutableArrayBufferFromJSVal d
            bsFromMutableArrayBuffer ab >>= onMessage . Left
        
        -- let e' = e
        -- data_ <- e' ^. js0 "data" >>= fromJSValUnchecked
        -- -- eventType <- e' ^. js0 "type" >>= fromJSValUnchecked
        -- -- lastEventId <- e' ^. js0 "lastEventId" >>= fromJSValUnchecked
        -- liftJSM $ onMessage $ EventSourceJSMessage
        --   { _eventSourceJSMessage_data = data_
        --   , _eventSourceJSMessage_eventType = T.pack "message" -- TODO: eventType
        --   , _eventSourceJSMessage_lastEventId = T.pack "event-id" -- TODO: lastEventId
        --   }
  es <- new (jsg "EventSource") (url)
  _ <- es ^. js2 "addEventListener" "open" onOpenWrapped
  _ <- es ^. js2 "addEventListener" "error" onErrorWrapped
  _ <- es ^. js2 "addEventListener" "message" onMessageWrapped
  return $ JSEventSource es

onBSMessage :: Either ByteString (EventSourceJSMessage JSVal) -> JSM (Either ByteString (EventSourceJSMessage ByteString))
onBSMessage = either (pure . Left) $ \msg -> do
  bsVal <- fromJSValUnchecked $ _eventSourceJSMessage_data msg
  pure $ Right $ msg { _eventSourceJSMessage_data = encodeUtf8 bsVal }
