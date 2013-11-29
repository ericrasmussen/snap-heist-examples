{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import           Heist
import           Data.Monoid

-- these imports are from our standalone modules in src/handlers
import qualified Loop as L
import qualified Conditional as C
import qualified LoopCompiled as LC

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", indexHandler)
         , ("/loop", L.loopHandler)
         , ("/loopcompiled", LC.tutorialsHandler)
         , ("/conditional", C.conditionalHandler)
         , ("/conditionaltemplate", C.conditionalTemplateHandler)
         , ("assets", serveDirectory "assets")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "A snap demo application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addConfig h $ mempty { hcCompiledSplices = LC.myCompiledSplices }
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes
    return $ App h s

--------------------------------------------------------------------------------
-- | Our glorious index page
indexHandler :: Handler App App ()
indexHandler = render "index"
