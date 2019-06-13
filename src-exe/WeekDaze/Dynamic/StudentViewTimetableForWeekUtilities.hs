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

	Contains utilities dependent on both the static data derived from either configuration or command-line options,
	& on the dynamic state of the current 'StudentView.TimetableForWeek.TimetableForWeek'.
-}

module WeekDaze.Dynamic.StudentViewTimetableForWeekUtilities(
-- * Functions
	calculateUtilisationRatio,
	countUnbookedLessons,
	countUnbookedByLesson,
--	completelyBookedByLesson,
	findIncompletelyBookedKnowledgeRequirementsFor,
-- ** Predicates
	isSufficientlyBooked
) where

import			Control.Arrow((&&&))
import			Data.Map((!))
import			Data.Set((\\))
import qualified	Data.Array.IArray
import qualified	Data.Map
import qualified	Data.Set
import qualified	ToolShed.Data.Pair
import qualified	WeekDaze.Aggregate.StudentBody				as Aggregate.StudentBody
import qualified	WeekDaze.Data.Course					as Data.Course
import qualified	WeekDaze.Data.Student					as Data.Student
import qualified	WeekDaze.Model.Lesson					as Model.Lesson
import qualified	WeekDaze.Model.TimetableForWeek				as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.Size						as Size
import qualified	WeekDaze.StudentView.Lesson				as StudentView.Lesson
import qualified	WeekDaze.StudentView.TimetableForWeek			as StudentView.TimetableForWeek

-- | The number of /lesson/s in a given /subject/, which are still to be booked, according to the weekly requirement for the /course/ to which it belongs.
countUnbookedLessons :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> StudentView.TimetableForWeek.TimetableForWeek timeslotId locationId teacherId level								-- ^ The current /timetable/ for this /student-body/.
	-> StudentView.Lesson.Lesson locationId teacherId level												-- ^ The /lesson/ we want to book.
	-> Size.NTimeslots
countUnbookedLessons problemParameters timetableForWeek	= uncurry (-) . (
	Data.Course.getRequiredLessonsPerWeek . ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters &&& (`Model.TimetableForWeek.countSubjectWorkload` timetableForWeek) . Model.Lesson.getSubject
 )

{- |
	* True if enough /lesson/s in a given /subject/ have been booked according to the weekly requirement for the /course/ to which it belongs.

	* CAVEAT: an excessive number of /booking/s also qualifies.
-}
isSufficientlyBooked :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> StudentView.TimetableForWeek.TimetableForWeek timeslotId locationId teacherId level								-- ^ The current /timetable/ for this /student-body/.
	-> StudentView.Lesson.Lesson locationId teacherId level												-- ^ The /lesson/ we want to book.
	-> Bool
isSufficientlyBooked problemParameters timetableForWeek	= (<= 0) . countUnbookedLessons problemParameters timetableForWeek

{- |
	* The number of each type of /lesson/ still to be booked, according to the weekly requirement for the /course/ to which it belongs.

	* CAVEAT: only those /lesson/s which have been booked in the /timetable/ are counted;
	since only these have a well-defined /course/ & consequently a known /requiredLessonsPerWeek/.

	* CAVEAT: incorrect unless one can guarantee that all /lesson/s for a given /subject/ are identical;
	otherwise the /workload/ returned for a /subject/ could be split amongst different /lesson/s.
-}
countUnbookedByLesson :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> StudentView.TimetableForWeek.TimetableForWeek timeslotId locationId teacherId level								-- ^ The current /timetable/ for this /student-body/.
	-> Data.Map.Map (StudentView.Lesson.Lesson locationId teacherId level) Size.NTimeslots
countUnbookedByLesson problemParameters	= Data.Map.mapWithKey (
	\studentViewLesson -> (Data.Course.getRequiredLessonsPerWeek (ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson) -)
 ) . Model.TimetableForWeek.countWorkloadByLesson

{- |
	* Identifies /lesson/s which have been sufficiently booked according to the weekly requirements for the corresponding /course/.

	* CAVEAT: if none of a particular /lesson/ has been booked, then there won't be any matching key in the map, even though the available /course/s are unbooked.

	* CAVEAT: an excessive number of /booking/s also qualifies.
-}
completelyBookedByLesson :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> StudentView.TimetableForWeek.TimetableForWeek timeslotId locationId teacherId level								-- ^ The current /timetable/ for this /student-body/.
	-> Data.Map.Map (StudentView.Lesson.Lesson locationId teacherId level) Bool
completelyBookedByLesson problemParameters	= Data.Map.map (<= 0) . countUnbookedByLesson problemParameters

{- |
	* The number of /lesson/s booked for the specified /student-body/, relative to the portion of the week for which they're supposed to be taught.

	* This factors into the denominator, both the 'Temporal.Availability.Availability' of the /student-body/,
	& the ratio of that working-time for which they're required to be taught rather than for free-study.
-}
calculateUtilisationRatio :: (
	Data.Array.IArray.Ix	timeslotId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Aggregate.StudentBody.StudentBody
	-> StudentView.TimetableForWeek.TimetableForWeek timeslotId locationId teacherId level
	-> teachingRatio
calculateUtilisationRatio problemParameters problemAnalysis studentBody	= Model.TimetableForWeek.calculateUtilisationRatio (
	ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis
 ) $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters ! studentBody

{- |
	* Finds those /knowledge-requirement/s for the specified /student/, which haven't been completely booked (including subjects which haven't been booked at all).

	* CAVEAT: it doesn't represent the extent to which a /course/ is unbooked, nor could it guarantee to able to define this extent,
	because completely unbooked /knowledge-requirement/s don't have a well-defined /course/ & therefore may have an unknown /requiredLessonsPerWeek/.
-}
findIncompletelyBookedKnowledgeRequirementsFor :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> StudentView.TimetableForWeek.TimetableForWeek timeslotId locationId teacherId level
	-> Data.Student.Profile level stream teachingRatio
	-> Data.Student.KnowledgeRequirements level
findIncompletelyBookedKnowledgeRequirementsFor problemParameters timetableForWeek	= ToolShed.Data.Pair.mirror (
	\\ Data.Set.map Model.Lesson.getSubject (
		Data.Set.filter (completelyBookedByLesson problemParameters timetableForWeek !) $ Model.TimetableForWeek.extractDistinctLessons timetableForWeek
	) -- Remove fully-booked subjects.
 ) . Data.Student.getKnowledgeRequirements

