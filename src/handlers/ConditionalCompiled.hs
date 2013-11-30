{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | The purpose of this module is to show off various ways to conditionally
-- include content in Heist templates. You will see some copy/paste and shared
-- code here in other modules, but the intent is to make this a standalone
-- module so you don't need to follow a bunch of imports in order to understand
-- these examples.

module ConditionalCompiled
  ( conditionalHandler
  , tutorialSplices
  --, conditionalTemplateHandler
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Monoid
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Compiled as C
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------

-- simple data type for Tutorials that may or may not have an author
data Tutorial = Tutorial {
    title  :: T.Text
  , url    :: T.Text
  , author :: Maybe T.Text
  }

-- unlike the interpreted version, we want values of Tutorial elevated to
-- RuntimeSplice n Tutorial so they can be passed in to compiled splices. We
-- could use this same pattern for values pulled from a database or other source
tutorialA :: Monad n => RuntimeSplice n Tutorial
tutorialA = return Tutorial {
    title  = "Heist Template Tutorial"
  , url    = "http://snapframework.com/docs/tutorials/heist"
  , author = Nothing
  }

tutorialB :: Monad n => RuntimeSplice n Tutorial
tutorialB = return Tutorial {
    title  = "Looping and Control Flow in Heist"
  , url    = "http://softwaresimply.blogspot.com/2011/04/looping-and-control-flow-in-heist.html"
  , author = Just "mightybyte"
  }

--------------------------------------------------------------------------------
-- * A handler to demonstrate conditional text attached to a template node

-- | This is the Snap Handler that will be associated with a route in
-- src/Site.hs
conditionalHandler :: Handler App App ()
conditionalHandler = cRender "conditional/tutorials"

-- | Creates compiled splices for a template that expects two different
-- tutorials to be inserted. We could map across a list of Tutorials, but to
-- keep the example simple the two tutorials are inserted one by one. You can
-- see a more advanced (and practical) example in src/handlers/LoopCompiled.hs
--
-- Note: Splices form a monoid so we can combine the two sets of splices with
-- mappend
tutorialSplices :: Monad n => Splices (C.Splice n)
tutorialSplices = applyS tutorialA splicesA `mappend` applyS tutorialB splicesB

-- | Creates compiled splices that can be applied to a runtime Tutorial splice
-- to bind the splices to the values of that tutorial
splicesA :: Monad n => Splices (RuntimeSplice n Tutorial -> C.Splice n)
splicesA = mapS (C.pureSplice . C.textSplice) $ do
  "titleA"       ## title
  "urlA"         ## url
  "maybeAuthorA" ## maybeAuthor

-- | Same as splicesA, repeated here to belabor the point
splicesB :: Monad n => Splices (RuntimeSplice n Tutorial -> C.Splice n)
splicesB = mapS (C.pureSplice . C.textSplice) $ do
  "titleB"       ## title
  "urlB"         ## url
  "maybeAuthorB" ## maybeAuthor

-- | Returns conditional Text based on whether or not we know the tutorial's
-- author
maybeAuthor :: Tutorial -> T.Text
maybeAuthor t = case author t of
  Nothing -> "no credited author"
  Just a  -> a


{-
--------------------------------------------------------------------------------
-- * A handler to demonstrate conditionally rendering an entire template and
-- attaching it to a node.

-- | Similar to conditionalHandler, except it conditionally inserts a rendered
-- template instead of Text
conditionalTemplateHandler :: Handler App App ()
conditionalTemplateHandler =
  renderWithSplices "conditional/authors" authorInfoSplices

authorInfoSplices :: Monad n => Splices (I.Splice n)
authorInfoSplices = do
  "authorA" ## authorTemplateSplice (author tutorialA)
  "authorB" ## authorTemplateSplice (author tutorialB)

-- | Creates a splice from a template when we have an author name, otherwise
-- returns an empty list (note that Template is a type synonym for [Node], so an
-- empty list is the base case)
authorTemplateSplice :: Monad n => Maybe T.Text -> I.Splice n
authorTemplateSplice Nothing = return []
authorTemplateSplice (Just a) =
  I.callTemplateWithText "authorinfo" ("authorName" ## a)

-}
