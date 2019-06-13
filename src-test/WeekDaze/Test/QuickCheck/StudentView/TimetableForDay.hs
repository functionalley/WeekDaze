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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for 'StudentView.TimetableForDay.TimetableForDay'.
-}

module WeekDaze.Test.QuickCheck.StudentView.TimetableForDay(
-- * Types
-- ** Type-synonyms
--	TimetableForDay,
-- * Constants
	results
) where

import			Data.Array.IArray((!))
import qualified	Data.Array.Base
import qualified	Data.Array.IArray
import qualified	Data.List
import qualified	Test.QuickCheck
import qualified	ToolShed.Data.List.Runlength
import			ToolShed.Test.QuickCheck.Arbitrary.Array()
import qualified	WeekDaze.Identifiers.Level		as Identifiers.Level
import qualified	WeekDaze.Identifiers.LocationId		as Identifiers.LocationId
import qualified	WeekDaze.Identifiers.TeacherId		as Identifiers.TeacherId
import qualified	WeekDaze.Identifiers.TimeslotId		as Identifiers.TimeslotId
import qualified	WeekDaze.Model.TimetableForDay		as Model.TimetableForDay
import qualified	WeekDaze.StudentView.TimetableForDay	as StudentView.TimetableForDay
import qualified	WeekDaze.Temporal.Time			as Temporal.Time
import			WeekDaze.Test.QuickCheck.Identifiers.TimeslotId()
import			WeekDaze.Test.QuickCheck.StudentView.Lesson()

-- | Defines a concrete type for testing.
type TimetableForDay	= StudentView.TimetableForDay.TimetableForDay Identifiers.TimeslotId.TimeslotId Identifiers.LocationId.LocationId Identifiers.TeacherId.TeacherId Identifiers.Level.Level

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_countRunlengthAt,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_boundRunlengthAt,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_spanRunlengthAt,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_spanRunlengthAt',
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_findGeneralisedLessonRunlengths,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_findGeneralisedLessonRunlengths',
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_findGeneralisedLessonRunlengthsByTimeslotId
 ] where
	prop_countRunlengthAt, prop_boundRunlengthAt, prop_spanRunlengthAt, prop_spanRunlengthAt' :: TimetableForDay -> Identifiers.TimeslotId.TimeslotId -> Test.QuickCheck.Property
	prop_countRunlengthAt studentViewTimetableForDay timeslotId	= Test.QuickCheck.label "prop_countRunlengthAt" $ Model.TimetableForDay.countRunlengthAt timeslotId studentViewTimetableForDay >= 1
	prop_boundRunlengthAt studentViewTimetableForDay timeslotId	= Test.QuickCheck.label "prop_boundRunlengthAt" $ Model.TimetableForDay.countRunlengthAt timeslotId studentViewTimetableForDay == succ {-fence-post-} (uncurry Temporal.Time.calculateAbsoluteDistance (Model.TimetableForDay.boundRunlengthAt timeslotId studentViewTimetableForDay))
	prop_spanRunlengthAt studentViewTimetableForDay timeslotId	= Test.QuickCheck.label "prop_spanRunlengthAt" $ Model.TimetableForDay.countRunlengthAt timeslotId studentViewTimetableForDay == length (Model.TimetableForDay.spanRunlengthAt timeslotId studentViewTimetableForDay)
	prop_spanRunlengthAt' studentViewTimetableForDay		= Test.QuickCheck.label "prop_spanRunlengthAt'" . (== 1) . length . Data.List.nub . map (studentViewTimetableForDay !) . (`Model.TimetableForDay.spanRunlengthAt` studentViewTimetableForDay)

	prop_findGeneralisedLessonRunlengths, prop_findGeneralisedLessonRunlengths', prop_findGeneralisedLessonRunlengthsByTimeslotId :: TimetableForDay -> Test.QuickCheck.Property
	prop_findGeneralisedLessonRunlengths studentViewTimetableForDay	= Test.QuickCheck.label "prop_findGeneralisedLessonRunlengths" $ ToolShed.Data.List.Runlength.decode (Model.TimetableForDay.findGeneralisedLessonRunlengths studentViewTimetableForDay) == Data.Array.IArray.elems studentViewTimetableForDay

	prop_findGeneralisedLessonRunlengths' studentViewTimetableForDay	= Test.QuickCheck.label "prop_findGeneralisedLessonRunlengths'" $ sum (
		map ToolShed.Data.List.Runlength.getLength $ Model.TimetableForDay.findGeneralisedLessonRunlengths studentViewTimetableForDay
	 ) == Data.Array.Base.numElements studentViewTimetableForDay

	prop_findGeneralisedLessonRunlengthsByTimeslotId studentViewTimetableForDay	= Test.QuickCheck.label "prop_findGeneralisedLessonRunlengthsByTimeslotId" . (
		== fst {-min-} (Data.Array.IArray.bounds studentViewTimetableForDay)
	 ) . fst {-timeslotId-} . head $ Model.TimetableForDay.findGeneralisedLessonRunlengthsByTimeslotId studentViewTimetableForDay

