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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Temporal.TimeslotRequest.TimeslotRequest'.
-}

module WeekDaze.Test.QuickCheck.Temporal.TimeslotRequest(
-- * Types
-- ** Type-synonyms
--	TimeslotRequest,
-- * Constants
	results
) where

import qualified	Data.Foldable
import qualified	Data.Set
import qualified	Test.QuickCheck
import qualified	WeekDaze.Identifiers.TimeslotId		as Identifiers.TimeslotId
import qualified	WeekDaze.Temporal.Time			as Temporal.Time
import qualified	WeekDaze.Temporal.TimeslotRequest	as Temporal.TimeslotRequest
import			WeekDaze.Test.QuickCheck.Temporal.Time()

instance Test.QuickCheck.Arbitrary timeslotId => Test.QuickCheck.Arbitrary (Temporal.TimeslotRequest.TimeslotRequest timeslotId) where
	arbitrary	= Test.QuickCheck.oneof [
		return {-to Gen-monad-} $ Temporal.TimeslotRequest.Specifically Data.Set.empty,
		fmap Temporal.TimeslotRequest.Ideally Test.QuickCheck.arbitrary,
		fmap (Temporal.TimeslotRequest.Specifically . Data.Set.singleton) Test.QuickCheck.arbitrary
	 ]

-- | Defines a concrete type for testing.
type TimeslotRequest	= Temporal.TimeslotRequest.TimeslotRequest Identifiers.TimeslotId.TimeslotId

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [
	prop_isASpecifiedTime,
	prop_isASpecifiedDay,
	prop_countSpecifiedTimes,
	prop_findDistinctRunlengthsOfSpecifiedTimes
 ] where
	prop_isASpecifiedTime, prop_isASpecifiedDay, prop_countSpecifiedTimes, prop_findDistinctRunlengthsOfSpecifiedTimes :: TimeslotRequest -> Test.QuickCheck.Property

	prop_isASpecifiedTime timeslotRequest	= Test.QuickCheck.label "prop_isASpecifiedTime" . Data.Foldable.all (
		`Temporal.TimeslotRequest.isASpecifiedTime` timeslotRequest
	 ) $ Temporal.TimeslotRequest.getSpecifiedTimes timeslotRequest

	prop_isASpecifiedDay timeslotRequest	= Test.QuickCheck.label "prop_isASpecifiedDay" . Data.Foldable.all (
		(`Temporal.TimeslotRequest.isASpecifiedDay` timeslotRequest) . Temporal.Time.getDay
	 ) $ Temporal.TimeslotRequest.getSpecifiedTimes timeslotRequest

	prop_countSpecifiedTimes timeslotRequest	= Test.QuickCheck.label "prop_countSpecifiedTimes" $ if Temporal.TimeslotRequest.countSpecifiedTimes timeslotRequest == 0
		then Temporal.TimeslotRequest.isNull timeslotRequest
		else Temporal.TimeslotRequest.isSpecific timeslotRequest

	prop_findDistinctRunlengthsOfSpecifiedTimes	= Test.QuickCheck.label "prop_findDistinctRunlengthsOfSpecifiedTimes" . Data.Set.null . Temporal.TimeslotRequest.findDistinctRunlengthsOfSpecifiedTimes

