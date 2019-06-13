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

 [@DESCRIPTION@]	Defines the data required to define a /time-slot/, in the /timetable/ viewed from the perspective of a /student-body/.
-}

module WeekDaze.StudentView.Lesson(
-- * Types
-- ** Type-synonyms
	Lesson
) where

import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.StudentView.LessonResourceIds	as StudentView.LessonResourceIds

-- | Specialise for the case in which a /lesson/ is viewed from the perspective of a /student-body/.
type Lesson locationId teacherId level	= Model.Lesson.Lesson (StudentView.LessonResourceIds.LessonResourceIds locationId teacherId) level

