module React.Router where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Coerce (coerce)
import Data.JSString (JSString)
import GHCJS.Marshal (FromJSVal(fromJSVal), ToJSVal(toJSVal))
import GHCJS.Types (JSVal)
import JavaScript.Object (getProp, setProp)
import qualified JavaScript.Object.Internal as OI
import React
  ( ReactProps(ReactProps), PropName(PropName), OnlyAttributes, (.:)
  , ReactM, getProps, key
  , ReactClass(ReactClass), ReactNode
  , Factory, createFactory, runFactory
  )

newtype RouterHistory = RouterHistory { unRouterHistory :: JSVal }

newtype Route = Route { unRoute :: ReactNode }

newtype Params p = Params { unParams :: p }

instance FromJSVal p => FromJSVal (Params p) where
  fromJSVal jv = runMaybeT $ do
    let obj = OI.Object jv
    pjv <- lift (getProp "params" obj)
    Params <$> MaybeT (fromJSVal pjv)
instance ToJSVal p => ToJSVal (Params p) where
  toJSVal (Params p) = do
    o <- OI.create
    jv <- toJSVal p
    setProp "params" jv o
    pure $ coerce o

foreign import javascript unsafe "ReactRouter.Router" js_Router :: ReactClass OnlyAttributes
foreign import javascript unsafe "ReactRouter.Link" js_Link :: ReactClass OnlyAttributes
foreign import javascript unsafe "ReactRouter.IndexRoute" js_IndexRoute :: ReactClass OnlyAttributes
foreign import javascript unsafe "ReactRouter.Route" js_Route :: ReactClass OnlyAttributes
foreign import javascript unsafe "ReactRouter.hashHistory" hashHistory :: RouterHistory
foreign import javascript unsafe "ReactRouter.browserHistory" browserHistory :: RouterHistory
foreign import javascript unsafe "ReactRouter.createMemoryHistory" createMemoryHistory :: RouterHistory

{-# NOINLINE routerFactory #-}
routerFactory :: Factory OnlyAttributes
routerFactory = createFactory js_Router

router :: RouterHistory -> Route -> ReactNode
router history rootRoute =
  runFactory routerFactory props [unRoute rootRoute]
  where
    props = [PropName "history" .: unRouterHistory history]

{-# NOINLINE routeFactory #-}
routeFactory :: Factory OnlyAttributes
routeFactory = createFactory js_Route

route :: (Foldable t, Functor t) => JSString -> ReactNode -> t Route -> Route
route path comp children = Route $ runFactory routeFactory props (unRoute <$> children)
  where
    props = [key .: path, PropName "path" .: path, PropName "component" .: comp]

{-# NOINLINE indexRouteFactory #-}
indexRouteFactory :: Factory OnlyAttributes
indexRouteFactory = createFactory js_IndexRoute

indexRoute :: (Foldable t, Functor t) => ReactNode -> t Route -> Route
indexRoute comp children = Route $ runFactory indexRouteFactory props (unRoute <$> children)
  where
    props = [PropName "component" .: comp]

{-# NOINLINE linkFactory #-}
linkFactory :: Factory OnlyAttributes
linkFactory = createFactory js_Link

link :: Foldable t => JSString -> t ReactNode -> ReactNode
link to els = runFactory linkFactory props els
  where
    props = [PropName "to" .: to]

getParams :: FromJSVal p => ReactM ps st p
getParams = pure . unParams =<< maybe (fail "missing params") pure =<< liftIO . fromJSVal . coerce =<< getProps

