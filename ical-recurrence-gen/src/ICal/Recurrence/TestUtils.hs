module ICal.Recurrence.TestUtils where

import qualified Data.Map as M
import ICal.Conformance.TestUtils
import ICal.PropertyType.RecurrenceRule.Gen ()
import ICal.Recurrence

shouldRecur :: R a -> IO a
shouldRecur func = shouldConform $ runR M.empty func
