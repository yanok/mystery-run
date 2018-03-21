module PreProcess where

import Result
import Runner

-- for now just repack
preProcess :: RawResult -> Result
preProcess = Result . getRaw
