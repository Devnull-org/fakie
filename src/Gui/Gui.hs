{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Gui.Gui where

import           Common
import           Data.GI.Base
import qualified GI.Gtk       as Gtk

gui :: IO ()
gui = do
  print "Starting gui app"
  Gtk.init Nothing
  win <- new Gtk.Window [#title := "Introduction"]
  on win #destroy Gtk.mainQuit
  #resize win 640 480

  msg <- new Gtk.Label [#label := "Hello"]
  #add win msg

  #showAll win
  Gtk.main
