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

 [@DESCRIPTION@]	Defines the data required to define a /time-slot/, in the /timetable/ viewed from the perspective of a /teacher/.
-}

module WeekDaze.TeacherView.Lesson(
-- * Types
-- ** Type-synonyms
	Lesson,
-- ** Translation
	fromStudentView,
	toStudentView
) where

import qualified	WeekDaze.Aggregate.StudentClass		as Aggregate.StudentClass
import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.StudentView.Lesson		as StudentView.Lesson
import qualified	WeekDaze.TeacherView.LessonResourceIds	as TeacherView.LessonResourceIds

-- | Specialise for the case in which a 'Model.Lesson.Lesson' is viewed from the perspective of the /teacher/.
type Lesson locationId level	= Model.Lesson.Lesson (TeacherView.LessonResourceIds.LessonResourceIds locationId) level

-- | Convert from the /student/-view to a /teacher/-view.
fromStudentView :: Show locationId => StudentView.Lesson.Lesson locationId teacherId level -> Aggregate.StudentClass.StudentClass -> Lesson locationId level
fromStudentView studentViewLesson studentClass	= studentViewLesson {
	Model.Lesson.getResourceIds	= TeacherView.LessonResourceIds.fromStudentView (Model.Lesson.getResourceIds studentViewLesson) studentClass
}

-- | Convert from the /teacher/-view to a /student/-view.
toStudentView :: teacherId -> Lesson locationId level -> StudentView.Lesson.Lesson locationId teacherId level
toStudentView teacherId lesson	= lesson {
	Model.Lesson.getResourceIds	= TeacherView.LessonResourceIds.toStudentView teacherId $ Model.Lesson.getResourceIds lesson
}
