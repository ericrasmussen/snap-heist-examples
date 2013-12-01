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
import qualified Heist.Compiled as C
import           Data.Monoid

-- these imports are from our standalone modules in src/handlers
import qualified Loop as L
import qualified Conditional as C
import qualified LoopCompiled as LC
import qualified ConditionalCompiled as CC

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", indexHandler)
         -- compiled splices
         , ("/compiled/loop", LC.loopHandler)
         , ("/compiled/conditional/text", CC.conditionalHandler)
         , ("/compiled/conditional/template", CC.conditionalTemplateHandler)
         -- interpreted splices
         , ("/interpreted/loop", L.loopHandler)
         , ("/interpreted/conditional/text", C.conditionalHandler)
         , ("/interpreted/conditional/template", C.conditionalTemplateHandler)
         , ("assets", serveDirectory "assets")
         ]


------------------------------------------------------------------------------
-- | Compose all the compiled splices imported from the handler modules
allCompiledSplices :: Monad n => Splices (C.Splice n)
allCompiledSplices = mconcat [ LC.allTutorialSplices
                             , CC.tutorialSplices
                             , CC.allAuthorSplices
                             ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "A snap demo application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addConfig h $ mempty { hcCompiledSplices = allCompiledSplices }
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    addRoutes routes
    return $ App h s

--------------------------------------------------------------------------------
-- | Our glorious index page
indexHandler :: Handler App App ()
indexHandler = render "index"
