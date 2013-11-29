{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- | This module demonstrates how you can repeat a section of a template by
-- mapping splice functions across a list of resources.
module LoopCompiled
  ( allTutorialSplices
  , loopHandler
  ) where

------------------------------------------------------------------------------
import qualified Data.Text as T
import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Heist
import qualified Heist.Compiled as C
------------------------------------------------------------------------------
import           Application

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State (get, StateT, StateT(..))

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
-- src/Site.hs. Note that unlike the interpreted Loop module, we can't bind
-- splices at render time. Instead, they're part of the Heist config when the
-- Heist Snaplet is initiated. See src/Site.hs for details.
loopHandler :: Handler App App ()
loopHandler = cRender "loop/tutorials"

allTutorialSplices :: Monad n => Splices (C.Splice n)
allTutorialSplices =
  "allTutorials" ## (renderTutorials tutorialsRuntime)

-- | There is no equivalent for this in the interpreted Loop example, where we
-- can more easily work with the Tutorial list directly. The purpose of this
-- function is to create a RuntimeSplice with the Tutorial list so we can work
-- it using compiled Heist functions.
-- Note: you could use any monad here, including IO (or a monad transformer with
-- an IO base) so you could retrieve these from a database, a file, or some
-- other source.
tutorialsRuntime :: Monad n => RuntimeSplice n [Tutorial]
tutorialsRuntime = return tutorials

-- | This function maps over a RuntimeSplice with a list of Tutorials, runs it
-- against the inner nodes of the current node, and creates a splice containing
-- all the rendered inner nodes for each Tutorial
renderTutorials :: Monad n => RuntimeSplice n [Tutorial] -> C.Splice n
renderTutorials = C.manyWithSplices C.runChildren splicesFromTutorial

-- | Creates a compiled splice from a single Tutorial
splicesFromTutorial :: Monad n => Splices (RuntimeSplice n Tutorial -> C.Splice n)
splicesFromTutorial = mapS (C.pureSplice . C.textSplice) $ do
  "tutorialTitle"  ## title
  "tutorialURL"    ## url
  "tutorialAuthor" ## maybeAuthor

-- | Returns conditional Text based on whether or not we know the tutorial's
-- author
maybeAuthor :: Tutorial -> T.Text
maybeAuthor t = case author t of
  Nothing -> ""
  Just a  -> a


