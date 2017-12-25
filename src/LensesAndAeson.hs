{-# LANGUAGE OverloadedStrings #-}
module LensesAndAeson
    (
    ) where


import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

jsonBlob:: String
jsonBlob = "[{\"someObject\": {\"version\": [1, 0, 3]}}]"

myVal = jsonBlob ^? nth 0 . key "someObject" . key "version" . nth 1
