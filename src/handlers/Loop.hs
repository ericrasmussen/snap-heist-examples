{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module demonstrates how you can repeat a section of a template by
-- mapping splice functions across a list of resources.
module Loop
  ( loopHandler
  ) where

------------------------------------------------------------------------------
import qualified Data.Text as T
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
--------------------------------------------------------------------------------

-- simple data type for Tutorials
data Tutorial = Tutorial {
    title  :: T.Text
  , url    :: T.Text
  , author :: Maybe T.Text
  }

-- the full Tutorial list is available here as a constant, but in our handlers
-- we could retrieve the list from a database or other source instead.
tutorials :: [Tutorial]
tutorials = [
    Tutorial {
        title  = "Heist Template Tutorial"
      , url    = "http://snapframework.com/docs/tutorials/heist"
      , author = Nothing
  }
  , Tutorial {
        title  = "Compiled Splices Tutorial"
      , url    = "http://snapframework.com/docs/tutorials/compiled-splices"
      , author = Nothing
  }
  , Tutorial {
        title  = "Attribute Splices Tutorial"
      , url    = "http://snapframework.com/docs/tutorials/attribute-splices"
      , author = Nothing
  }
  , Tutorial {
        title  = "Views, Controllers, and Heist"
      , url    = "http://softwaresimply.blogspot.com/2011/04/views-controllers-and-heist.html"
      , author = Just "mightybyte"
      }
  , Tutorial {
        title  = "Looping and Control Flow in Heist"
      , url    = "http://softwaresimply.blogspot.com/2011/04/looping-and-control-flow-in-heist.html"
      , author = Just "mightybyte"
      }
  ]

--------------------------------------------------------------------------------
-- * A handler to demonstrate rendering a list of Tutorial, where the author
-- record may or may not be present.

-- | This is the Snap Handler that will be associated with a route in
-- src/Site.hs
loopHandler :: Handler App App ()
loopHandler = renderWithSplices "loop/tutorials" allTutorialSplices

-- | These splices will be bound to templates/loop/tutorials.tpl via the
-- loopHandler
allTutorialSplices :: Splices (SnapletISplice App)
allTutorialSplices = "allTutorials" ## (renderTutorials tutorials)

-- | This function maps over a list of Tutorials, creates splices from each,
-- and binds the list of splices. When this is bound to "allTutorials" in
-- allTutorialSplices, all the nodes inside "allTutorials" will be repeated
-- once for each item in the Tutorial list
renderTutorials :: [Tutorial] -> SnapletISplice App
renderTutorials = I.mapSplices $ I.runChildrenWith . splicesFromTutorial

-- | Binds splices for a single Tutorial
splicesFromTutorial :: Monad n => Tutorial -> Splices (I.Splice n)
splicesFromTutorial t = do
  "tutorialTitle"  ## I.textSplice (title t)
  "tutorialURL"    ## I.textSplice (url   t)
  "tutorialAuthor" ## tutorialAuthorSplice (author t)

-- | If called with Just text, renders the author's name using the "author"
-- template. Otherwise returns an empty list, which is the base case for
-- a Template (note that Template is a type synonym for [Node])
tutorialAuthorSplice :: Monad n => Maybe T.Text -> I.Splice n
tutorialAuthorSplice Nothing = return []
tutorialAuthorSplice (Just a) =
  I.callTemplateWithText "author" ("authorName" ## a)

