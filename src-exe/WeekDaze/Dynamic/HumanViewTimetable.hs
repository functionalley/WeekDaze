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

 [@DESCRIPTION@]

	Encapsulates the /timetable/ as seen from both /human-resource/ views;
	i.e. from the /student-body/'s point-of-view & from the /teacher/'s.
-}

module WeekDaze.Dynamic.HumanViewTimetable(
-- * Types
-- ** Data-types
	TimetablePair(
--		MkTimetablePair,
		getStudentViewTimetable,
		getTeacherViewTimetable
	),
-- * Functions
-- ** Constructor
	mkTimetablePair,
-- ** Mutators
	book,
	bookAtomically
) where

import			Control.Arrow((&&&))
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	WeekDaze.Dynamic.TeacherViewTimetableUtilities		as Dynamic.TeacherViewTimetableUtilities
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.TeacherView.Timetable				as TeacherView.Timetable

-- | Couples the two /human-resource/ views of a /timetable/, to facilitate synchronised changes.
data TimetablePair timeslotId locationId teacherId level	= MkTimetablePair {
	getStudentViewTimetable	:: StudentView.Timetable.Timetable timeslotId locationId teacherId level,	-- ^ The /timetable/ as seen from the /student-body/'s point-of-view.
	getTeacherViewTimetable	:: TeacherView.Timetable.Timetable teacherId timeslotId locationId level	-- ^ The /timetable/ as seen from the /teacher/'s point-of-view.
}

-- | Constructor.
mkTimetablePair :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			teacherId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> TimetablePair timeslotId locationId teacherId level
mkTimetablePair executionOptions problemAnalysis	= uncurry ($) . (MkTimetablePair &&& Dynamic.TeacherViewTimetableUtilities.fromStudentViewTimetable executionOptions problemAnalysis)

{- |
	* Book a /student-view lesson/ in both human views of the /timetable/.

	* CAVEAT: doesn't check whether there's already a /lesson/ at the specified /coordinates/, which it may overwrite.
-}
book :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			teacherId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ExecutionConfiguration.ExecutionOptions.PermitTemporaryStudentBodyMerger
	-> StudentView.Timetable.Booking timeslotId locationId teacherId level	-- ^ The booked from the /student/'s point-of-view.
	-> TimetablePair timeslotId locationId teacherId level
	-> TimetablePair timeslotId locationId teacherId level
book permitTemporaryStudentBodyMerger studentViewBooking	= uncurry MkTimetablePair . (
	Model.Timetable.defineTimeslot (Model.Timetable.toGeneralisedBooking studentViewBooking) . getStudentViewTimetable &&& TeacherView.Timetable.bookStudentViewLesson permitTemporaryStudentBodyMerger studentViewBooking . getTeacherViewTimetable
 )

{- |
	* Sequentially make the specified /booking/s, in the specified order, until either they're exhausted or the supplied predicate fails.

	* Returns either the /TimetablePair/ including all the required /booking/s, or 'Nothing' on failure.

	* CAVEAT: doesn't perform any native checks,
	so it up to the caller to check whether the required bookings have already been made;
	or whether to overwrite existing /booking/s.
-}
bookAtomically :: (
	Data.Array.IArray.Ix	timeslotId,
	Data.Foldable.Foldable	foldable,
	Eq			level,
	Eq			locationId,
	Ord			teacherId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ExecutionConfiguration.ExecutionOptions.PermitTemporaryStudentBodyMerger
	-> (
		TimetablePair timeslotId locationId teacherId level
		-> StudentView.Timetable.Booking timeslotId locationId teacherId level
		-> Bool
	)											-- ^ Predicate based on a /TimetablePair/ & /booking/.
	-> TimetablePair timeslotId locationId teacherId level					-- ^ The initial /timetable/ on which to operate, as seen from the perspective of both /student/s & /teacher/s.
	-> foldable (StudentView.Timetable.Booking timeslotId locationId teacherId level)	-- ^ The ordered bookings to be placed.
	-> Maybe (TimetablePair timeslotId locationId teacherId level)				-- ^ Returns a /timetable-pair/ iff the /booking/s were completed.
bookAtomically permitTemporaryStudentBodyMerger predicate timetablePair	= Data.Foldable.foldl' (
	\maybeTimetablePair studentViewBooking	-> maybeTimetablePair >>= \timetablePair' -> if predicate timetablePair' studentViewBooking
		then Just $ book permitTemporaryStudentBodyMerger studentViewBooking timetablePair'
		else Nothing
 ) (Just timetablePair)
