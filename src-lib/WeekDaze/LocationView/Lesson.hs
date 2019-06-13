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

 [@DESCRIPTION@]	Defines the data required to define a /time-slot/, in the /timetable/ viewed from the perspective of a /location/.
-}

module WeekDaze.LocationView.Lesson(
-- * Types
-- ** Type-synonyms
	Lesson,
-- ** Translation
	fromStudentView,
	toStudentView
) where

import qualified	WeekDaze.Aggregate.StudentClass		as Aggregate.StudentClass
import qualified	WeekDaze.LocationView.LessonResourceIds	as LocationView.LessonResourceIds
import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.StudentView.Lesson		as StudentView.Lesson

-- | Specialise for the case in which a /lesson/ is viewed from the perspective of the /location/.
type Lesson teacherId level	= Model.Lesson.Lesson (LocationView.LessonResourceIds.LessonResourceIds teacherId) level

-- | Convert from the /student/-view to a /location/-view.
fromStudentView :: Show teacherId => Aggregate.StudentClass.StudentClass -> StudentView.Lesson.Lesson locationId teacherId level -> Lesson teacherId level
fromStudentView studentClass studentViewLesson	= studentViewLesson {
	Model.Lesson.getResourceIds	= LocationView.LessonResourceIds.fromStudentView studentClass $ Model.Lesson.getResourceIds studentViewLesson
}

-- | Convert from the /location/-view to a /student/-view.
toStudentView :: locationId -> Lesson teacherId level -> StudentView.Lesson.Lesson locationId teacherId level
toStudentView locationId lesson	= lesson {
	Model.Lesson.getResourceIds	= LocationView.LessonResourceIds.toStudentView locationId $ Model.Lesson.getResourceIds lesson
}

