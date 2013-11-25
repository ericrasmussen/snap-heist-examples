{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | The purpose of this module is to show off various ways to conditionally
-- include content in Heist templates. You will see some copy/paste and shared
-- code here in other modules, but the intent is to make this a standalone
-- module so you don't need to follow a bunch of imports in order to understand
-- these examples.

module Conditional
  ( conditionalHandler
  , conditionalTemplateHandler
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------

-- simple data type for Tutorials that may or may not have an author
data Tutorial = Tutorial {
    title  :: T.Text
  , url    :: T.Text
  , author :: Maybe T.Text
  }

-- an example tutorial without a primary author
tutorialA = Tutorial {
    title  = "Heist Template Tutorial"
  , url    = "http://snapframework.com/docs/tutorials/heist"
  , author = Nothing
  }

-- an example tutorial with an author
tutorialB = Tutorial {
    title  = "Looping and Control Flow in Heist"
  , url    = "http://softwaresimply.blogspot.com/2011/04/looping-and-control-flow-in-heist.html"
  , author = Just "mightybyte"
  }


--------------------------------------------------------------------------------
-- * A handler to demonstrate conditional text attached to a template node

-- | This is the Snap Handler that will be associated with a route in
-- src/Site.hs
conditionalHandler :: Handler App App ()
conditionalHandler = heistLocal (I.bindSplices tutorialSplices) $ rendered
  where rendered = render "conditional/tutorials"

-- | Creates splices for a template that expects two different tutorials to be
-- inserted. We could map across a list of Tutorials, but to keep the example
-- simple the two tutorials are inserted one by one. You can see a more
-- advanced (and practical) example in src/handlers/Loop.hs
tutorialSplices :: Monad n => Splices (I.Splice n)
tutorialSplices = do
  -- this tutorial has no author
  "titleA"       ## I.textSplice (title  tutorialA)
  "urlA"         ## I.textSplice (url    tutorialA)
  "maybeAuthorA" ## authorSplice (author tutorialA)
  -- this tutorial has an author
  "titleB"       ## I.textSplice (title  tutorialB)
  "urlB"         ## I.textSplice (url    tutorialB)
  "maybeAuthorB" ## authorSplice (author tutorialB)


-- | Creates a text splice from an author name (the Just case) or default
-- text (the Nothing case)
authorSplice :: Monad n => Maybe T.Text -> I.Splice n
authorSplice Nothing  = I.textSplice "no credited author"
authorSplice (Just a) = I.textSplice a


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

