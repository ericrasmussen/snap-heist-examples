{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- | This module demonstrates how you can repeat a section of a template by
-- mapping splice functions across a list of resources.
module LoopCompiled
  ( myCompiledSplices
  , tutorialsHandler
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

-- unlike interpreted splices, we don't bind anything at runtime. it's bound
-- at the top level in our heist config
tutorialsHandler :: Handler App App ()
tutorialsHandler = cRender "loop/tutorials"

-- create a runtime splice from our constant tutorial list. this computation
-- could reach out to a database or another source
tutorialsRuntime :: Monad n => RuntimeSplice n [Tutorial]
tutorialsRuntime = return tutorials

-- runs all the child nodes with many splices across a list of tutorials
-- similar to the interpreted map splices function
tutorialsSplice :: Monad n => RuntimeSplice n [Tutorial] -> C.Splice n
tutorialsSplice = C.manyWithSplices C.runChildren tutorialSplices

-- create runtime splices for a single Tutorial
tutorialSplices :: Monad n => Splices (RuntimeSplice n Tutorial -> C.Splice n)
tutorialSplices = mapS (C.pureSplice . C.textSplice) $ do
  "tutorialTitle"  ## title
  "tutorialURL"    ## url
  "tutorialAuthor" ## maybeAuthor

maybeAuthor :: Tutorial -> T.Text
maybeAuthor t = case author t of
  Nothing -> ""
  Just a  -> a

myCompiledSplices :: Monad n => Splices (C.Splice n)
myCompiledSplices = do
  "allTutorials" ## (tutorialsSplice tutorialsRuntime)

