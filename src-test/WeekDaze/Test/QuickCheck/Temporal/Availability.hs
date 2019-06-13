{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Temporal.Availability.Availability'.
-}

module WeekDaze.Test.QuickCheck.Temporal.Availability(
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	Factory.Data.Interval
import qualified	Test.QuickCheck
import qualified	WeekDaze.Temporal.Availability	as Temporal.Availability
import qualified	WeekDaze.Temporal.Day		as Temporal.Day
import			WeekDaze.Test.QuickCheck.Temporal.Day()

instance Test.QuickCheck.Arbitrary Temporal.Availability.Availability where
	arbitrary	= fmap Temporal.Availability.mkAvailability (Test.QuickCheck.elements [1 .. Temporal.Day.nDaysPerWeek] >>= Test.QuickCheck.vector)

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [
	prop_isAvailableOn,
	prop_countDaysPerWeekAvailable,
	prop_countInternalAvailabilityGaps,
	prop_calculateAvailabilityRatio,
	prop_findUnions
#ifdef USE_HDBC
	,prop_mysql
#endif
 ] where
	prop_isAvailableOn, prop_countDaysPerWeekAvailable, prop_countInternalAvailabilityGaps, prop_calculateAvailabilityRatio, prop_findUnions	:: Temporal.Availability.Availability -> Test.QuickCheck.Property

	prop_isAvailableOn availability	= Test.QuickCheck.label "prop_isAvailableOn" . (
		if Temporal.Availability.isFulltime availability
			then id
			else not
	 ) $ all (`Temporal.Availability.isAvailableOn` availability) Temporal.Day.range

	prop_countDaysPerWeekAvailable	= Test.QuickCheck.label "prop_countDaysPerWeekAvailable" . (\r -> all ($ r) [(>= 0), (<= Temporal.Day.nDaysPerWeek)]) . Temporal.Availability.countDaysPerWeekAvailable

	prop_countInternalAvailabilityGaps	= Test.QuickCheck.label "prop_countInternalAvailabilityGaps" . (<= floor (toRational Temporal.Day.nDaysPerWeek / 2)) . Temporal.Availability.countInternalAvailabilityGaps

	prop_calculateAvailabilityRatio	= Test.QuickCheck.label "prop_calculateAvailabilityRatio" . (`Factory.Data.Interval.elem'` (Factory.Data.Interval.closedUnitInterval :: (Rational, Rational))) . Temporal.Availability.calculateAvailabilityRatio

	prop_findUnions	= Test.QuickCheck.label "prop_findUnions" . uncurry (==) . (Temporal.Availability.findUnions &&& Temporal.Availability.findIntersections) . (return {-to List-monad-} :: a -> [a])

#ifdef USE_HDBC
	prop_mysql :: Temporal.Availability.Availability -> Test.QuickCheck.Property
	prop_mysql availability	= Test.QuickCheck.label "prop_mysql" . (== availability) . Temporal.Availability.fromMySqlSet $ Temporal.Availability.toMySqlSet availability
#endif
