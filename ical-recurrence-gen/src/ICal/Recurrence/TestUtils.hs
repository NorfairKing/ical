module ICal.Recurrence.TestUtils where

import Conformance.TestUtils
import ICal.PropertyType.RecurrenceRule.Gen ()
import ICal.Recurrence

shouldRecur :: R a -> IO a
shouldRecur func = shouldConform $ runRWithoutZones func
