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

 [@DESCRIPTION@] Defines the type of a /teacher/'s personal /timetable/ for any week.
-}

module WeekDaze.TeacherView.TimetableForWeek(
-- * Types
-- ** Type-synonyms
	TimetableForWeek,
-- * Functions
	countLocationChanges
) where

import			Control.Arrow((&&&))
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.Model.TimetableForWeek		as Model.TimetableForWeek
import qualified	WeekDaze.TeacherView.LessonResourceIds	as TeacherView.LessonResourceIds

-- | A timetable for one 'teacher', for any week.
type TimetableForWeek timeslotId locationId level	= Model.TimetableForWeek.TimetableForWeek timeslotId (TeacherView.LessonResourceIds.LessonResourceIds locationId) level

{- |
	* Counts the number of times a /teacher/ has to relocate.

	* CAVEAT: an unallocated /time-slot/ is considered to have an unspecified /location/,
	which matches that of any adjacent unallocated /time-slot/s, but never that of an adjacent /lesson/.
-}
countLocationChanges :: (Data.Array.IArray.Ix timeslotId, Eq locationId) => TimetableForWeek timeslotId locationId level -> Int
countLocationChanges	= foldr (
	\(x, y)	-> if x /= y then succ else id	-- Count the number of times (Maybe locationId) changes.
 ) 0 . uncurry zip . (
	init &&& tail
 ) . Data.Foldable.concatMap (
	map (
		fmap $ TeacherView.LessonResourceIds.getLocationId . Model.Lesson.getResourceIds
	) . Data.Array.IArray.elems
 )

