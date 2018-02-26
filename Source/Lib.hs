
module Lib
    ( run
    ) where

import Data.Text

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util


class BuildSystem b where
    fileending :: b -> Text
    build :: b -> IO ()


run :: FilePath -> IO ()
run buildFilePath = shakeArgs shakeOptions{shakeFiles=buildFilePath} $ do
    phony "echo" $ do
        putNormal "echo back!"
