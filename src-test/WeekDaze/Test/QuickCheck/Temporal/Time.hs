{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2013-2015 Dr. Alistair Ward

	This file is part of WeekDaze.

	WeekDaze is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	WeekDaze is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with WeekDaze.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Temporal.Time.Time'.
-}

module WeekDaze.Test.QuickCheck.Temporal.Time(
-- * Types
-- ** Type-synonyms
	Time,
-- * Constants
	nTimeslotsPerDay,
	nTimeslotsPerWeek,
	results
) where

import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO
import qualified	WeekDaze.Identifiers.TimeslotId	as Identifiers.TimeslotId
import qualified	WeekDaze.Size			as Size
import qualified	WeekDaze.Temporal.Time		as Temporal.Time
import			WeekDaze.Test.QuickCheck.Identifiers.TimeslotId()
import			WeekDaze.Test.QuickCheck.Temporal.Day()

instance Test.QuickCheck.Arbitrary timeslotId => Test.QuickCheck.Arbitrary (Temporal.Time.Time timeslotId) where
	arbitrary	= uncurry Temporal.Time.mkTime `fmap` Test.QuickCheck.arbitrary {-pair-}

-- | Defines a concrete type for testing.
type Time	= Temporal.Time.Time Identifiers.TimeslotId.TimeslotId

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_readPrependedWhiteSpace,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_shift,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_read,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_readTrailingGarbage
 ] where
	prop_readPrependedWhiteSpace :: Time -> Test.QuickCheck.Property
	prop_readPrependedWhiteSpace	= Test.QuickCheck.label "prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace

	prop_shift :: Size.NTimeslots -> Identifiers.TimeslotId.TimeslotId -> Test.QuickCheck.Property
	prop_shift i timeslotId	= Test.QuickCheck.label "prop_shift" $ Temporal.Time.shift (negate i') (Temporal.Time.shift i' timeslotId) == timeslotId where
		i'	= i `mod` fromEnum (maxBound :: Identifiers.TimeslotId.TimeslotId)

	prop_read :: String -> Test.QuickCheck.Property
	prop_read garbage	= Test.QuickCheck.label "prop_read" $ case (reads garbage :: [(Time, String)]) of
		[_]	-> True
		_	-> True	-- Unless the read-implementation throws an exception.

	prop_readTrailingGarbage :: Time -> String -> Test.QuickCheck.Property
	prop_readTrailingGarbage time	= Test.QuickCheck.label "prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) time

-- | The constant number of /time-slots/ per day.
nTimeslotsPerDay :: Size.NTimeslots
nTimeslotsPerDay	= succ {-fence-post-} $ fromEnum (maxBound :: Identifiers.TimeslotId.TimeslotId) - fromEnum (minBound :: Identifiers.TimeslotId.TimeslotId)

-- | The constant number of /time-slots/ per week.
nTimeslotsPerWeek :: Size.NTimeslots
nTimeslotsPerWeek	= Temporal.Time.calculateNTimeslotsPerWeek nTimeslotsPerDay

