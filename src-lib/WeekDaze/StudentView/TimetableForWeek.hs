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

	* Defines the data-structure, to required to hold the specification of the activities of a single week, for any single /student-body/.

	* It forms a complete /timetable/, when combined with similar structures for all other /student-bodies/.
-}

module WeekDaze.StudentView.TimetableForWeek(
-- * Types
-- ** Type-synonyms
	Booking,
	TimetableForWeek,
-- * Functions
	extractRoutineBySubject
) where

import			Control.Arrow((&&&))
import qualified	Data.Array.IArray
import qualified	Data.Map
import qualified	Data.Set
import qualified	WeekDaze.Data.Subject			as Data.Subject
import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.Model.TimetableForWeek		as Model.TimetableForWeek
import qualified	WeekDaze.StudentView.LessonResourceIds	as StudentView.LessonResourceIds

-- | A booking seen from one point-of-view.
type Booking timeslotId locationId teacherId level	= Model.TimetableForWeek.Booking timeslotId (StudentView.LessonResourceIds.LessonResourceIds locationId teacherId) level

-- | A timetable for one 'Data.Student.Body' with identical scheduling requirements, for any week.
type TimetableForWeek timeslotId locationId teacherId level	= Model.TimetableForWeek.TimetableForWeek timeslotId (StudentView.LessonResourceIds.LessonResourceIds locationId teacherId) level

{- |
	* Once a /teacher/ has been assigned to teach a /student-body/, in a /subject/, at a /location/, a routine has been established;
	the /student-body/ wouldn't normally be taught that /subject/, by any other /teacher/, or in any other /location/.
	If the routine is adhered to, then each /subject/ maps to a singleton set of /resource-Id/s.

	* CAVEAT: this concept doesn't apply to other views of the /timetable/:
		/teacher/-view;		the same /teacher/ can teach other /student-bodies/, in that /subject/, & at that /location/.
		/location/-view;	the same /location/ can host other /student-bodies/, for tuition of that /subject/, & by that /teacher/.
-}
extractRoutineBySubject :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> TimetableForWeek timeslotId locationId teacherId level	-- ^ The current /timetable/.
	-> Data.Map.Map (Data.Subject.Subject level) (Data.Set.Set (StudentView.LessonResourceIds.LessonResourceIds locationId teacherId))
extractRoutineBySubject	= foldr (
	uncurry (
		Data.Map.insertWith Data.Set.union
	) . (
		Model.Lesson.getSubject &&& Data.Set.singleton . Model.Lesson.getResourceIds
	)
 ) Data.Map.empty . Model.TimetableForWeek.extractLessons

