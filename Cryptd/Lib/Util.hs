-- | Miscellaneous utility functions
module Cryptd.Lib.Util (formatSummary) where

import Data.Version (Version, showVersion)

-- | Format a summary of a program with the specified 'Version'.
formatSummary :: Version -> String -> String
formatSummary version summary =
    summary ++ " (version: " ++ showVersion version ++ ")"
