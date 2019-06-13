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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for 'StudentView.TimetableForWeek.TimetableForWeek'.
-}

module WeekDaze.Test.QuickCheck.StudentView.TimetableForWeek(
-- * Types
-- ** Type-synonyms
--	TimetableForWeek,
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	Test.QuickCheck
import			ToolShed.Test.QuickCheck.Arbitrary.Array()
import qualified	WeekDaze.Identifiers.Level			as Identifiers.Level
import qualified	WeekDaze.Identifiers.LocationId			as Identifiers.LocationId
import qualified	WeekDaze.Identifiers.TeacherId			as Identifiers.TeacherId
import qualified	WeekDaze.Identifiers.TimeslotId			as Identifiers.TimeslotId
import qualified	WeekDaze.Model.TimetableForWeek			as Model.TimetableForWeek
import qualified	WeekDaze.StudentView.TimetableForWeek		as StudentView.TimetableForWeek
import qualified	WeekDaze.Temporal.Day				as Temporal.Day
import			WeekDaze.Test.QuickCheck.Identifiers.TimeslotId()
import qualified	WeekDaze.Test.QuickCheck.StudentView.Lesson	as Test.StudentView.Lesson
import			WeekDaze.Test.QuickCheck.Temporal.Day()

-- | Defines a concrete type for testing.
type TimetableForWeek	= StudentView.TimetableForWeek.TimetableForWeek Identifiers.TimeslotId.TimeslotId Identifiers.LocationId.LocationId Identifiers.TeacherId.TeacherId Identifiers.Level.Level

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 1000} `mapM` [prop_isBookedOnAdjacentDay] where
	prop_isBookedOnAdjacentDay :: TimetableForWeek -> Test.StudentView.Lesson.Lesson -> Test.QuickCheck.Property
	prop_isBookedOnAdjacentDay studentViewTimetableForWeek lesson	= Test.QuickCheck.label "prop_isBookedOnAdjacentDay" $ all (
		uncurry (||) . (
			Model.TimetableForWeek.isBookedOnAdjacentDay studentViewTimetableForWeek lesson . Temporal.Day.getYesterday . Temporal.Day.getYesterday &&& Model.TimetableForWeek.isBookedOnAdjacentDay studentViewTimetableForWeek lesson . Temporal.Day.getTomorrow . Temporal.Day.getTomorrow
		)
	 ) $ filter (
		Model.TimetableForWeek.isBookedOnAdjacentDay studentViewTimetableForWeek lesson
	 ) Temporal.Day.range

