module Main where

import Control.Monad (mfilter)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Coerce (coerce)
import Data.JSString (JSString)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Document (createElement, getBody)
import GHCJS.DOM.Node (appendChild)
import GHCJS.Foreign (isUndefined)
import qualified GHCJS.Foreign as Foreign
import GHCJS.Marshal (FromJSVal(fromJSVal), ToJSVal(toJSVal))
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.Types (JSVal)
import qualified JavaScript.Array as Array
import JavaScript.Object (getProp, setProp)
import JavaScript.Object.Internal (Object(Object))
import qualified JavaScript.Object.Internal as OI
import React (ReactClass, ReactElement, Props(Props), (.:))
import qualified React
import qualified React.DOM as DOM
import qualified React.Router as Router

children :: React.ReactM ps st (Maybe ReactElement)
children = do
  Props props <- React.getProps
  js_children <- liftIO $ getProp "children" props
  pure $ if isUndefined js_children
         then Nothing
         else Just $ coerce js_children

foreign import javascript "console.log($1)" js_consoleLog :: JSVal -> IO ()

appSpec :: React.Spec React.OnlyAttributes React.OnlyAttributes
appSpec =
  React.spec . pure $ do
    liftIO . js_consoleLog . coerce =<< React.getProps
    cMay <- children
    pure $
      DOM.div_ [] $
        [ DOM.h1_ [React.key .: "h1"] ["App"]
        , DOM.ul_ [React.key .: "ul"]
          [ DOM.li_ [React.key .: "1"] [ Router.link "/about" ["About"] ]
          , DOM.li_ [React.key .: "2"] [ Router.link "/inbox" ["Inbox"] ]
          , DOM.li_ [React.key .: "3"] [ Router.link "/inbox/messages/123" ["Foobar"] ]
          ]
        ] ++ maybe [] ((:[]) . DOM.div_ [React.key .: "content"] . (:[])) cMay

React.makeClass "app" [| appSpec |]
app :: ReactClass React.OnlyAttributes

aboutSpec :: React.Spec React.OnlyAttributes React.OnlyAttributes
aboutSpec = (React.spec render) { React.displayName = Just "About" }
  where
    render = pure $ do
      liftIO $ js_consoleLog $ pToJSVal $ ("render about" :: JSString)
      pure $
        DOM.div_ []
          [ DOM.h3_ [] ["About"]
          ]

React.makeClass "about" [| aboutSpec |]
about :: ReactClass React.OnlyAttributes

inboxSpec :: React.Spec React.OnlyAttributes React.OnlyAttributes
inboxSpec =
  React.spec . pure $ do
    cMay <- children
    liftIO $ js_consoleLog $ pToJSVal $ ("render inbox" :: JSString)
    pure $
      DOM.div_ [] $
        [ DOM.h2_ [] ["Inbox"]
        , DOM.div_ [] (maybe ["Welcome to your Inbox"] (:[]) cMay)
        ]

React.makeClass "inbox" [| inboxSpec  |]
inbox :: ReactClass React.OnlyAttributes

data MessageParams = MessageParams
  { messageIndex          :: Int
  }

instance FromJSVal MessageParams where
  fromJSVal jv = runMaybeT $ do
    let obj = Object jv
    lift $ js_consoleLog jv
    pjv <- lift (getProp "id" obj)
    MessageParams <$> MaybeT (fromJSVal pjv)
instance ToJSVal MessageParams where
  toJSVal (MessageParams p) = do
    o <- OI.create
    jv <- toJSVal p
    setProp "id" jv o
    pure $ coerce o

messageSpec :: React.Spec (Router.Params MessageParams) React.OnlyAttributes
messageSpec =
  React.spec . pure $ do
    liftIO $ js_consoleLog $ pToJSVal $ ("render message" :: JSString)

    liftIO . js_consoleLog . coerce =<< React.getProps
    msgIndex <- messageIndex <$> Router.getParams
    pure $
      DOM.div_ []
        [ DOM.h3_ [] [React.text $ "Message " ++ show msgIndex] ]

React.makeClass "message" [| messageSpec |]
message :: ReactClass (Router.Params MessageParams)

router :: ReactElement
router =
  Router.router Router.hashHistory $ Router.route "/" (coerce app)
    [ Router.route "about" (coerce about) []
    , Router.route "inbox" (coerce inbox)
      [ Router.route "messages/:id" (coerce message) []
      ]
    ]

main :: IO ()
main = do
  js_consoleLog $ pToJSVal ("aboutSpec" :: JSString)
  js_consoleLog =<< coerce . fst <$> React.buildSpec aboutSpec
  js_consoleLog $ pToJSVal ("about" :: JSString)
  js_consoleLog $ coerce about
  doc <- fromMaybe (error "no current document!") <$> currentDocument
  body <- fromMaybe (error "no body!") <$> getBody doc
  mountpoint <- fromMaybe (error "couldn't create a div!") <$> createElement doc (Just ("div" :: JSString))
  _ <- appendChild body (Just mountpoint)
  React.render router mountpoint Nothing
