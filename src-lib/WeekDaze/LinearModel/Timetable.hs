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

 [@DESCRIPTION@]	A flattened /timetable/, with undefined /time-slot/s removed.
-}

module WeekDaze.LinearModel.Timetable(
-- * Types
-- ** Type-synonyms
	Timetable,
-- * Functions
	unbookWhere,
-- ** Translation
	fromTimetable
) where

import			Control.Arrow((&&&))
import qualified	Data.Array.IArray
import qualified	Data.Map
import qualified	WeekDaze.LinearModel.TimetableForWeek	as LinearModel.TimetableForWeek
import qualified	WeekDaze.Model.Timetable		as Model.Timetable

-- | A flattened /timetable/, compressed by removing all undefined /time-slot/s.
type Timetable observerId timeslotId resourceIds level	= [Model.Timetable.Booking observerId timeslotId resourceIds level]

-- | Serialises the /lesson/-definitions in the specified /timetable/.
fromTimetable :: Data.Array.IArray.Ix timeslotId => Model.Timetable.Timetable observerId timeslotId resourceIds level -> Timetable observerId timeslotId resourceIds level
fromTimetable timetable	= [
	((observerId, time), lesson) |
		(observerId, timetableForWeek)	<- Data.Map.toList timetable,
		(time, lesson)			<- LinearModel.TimetableForWeek.fromTimetableForWeek timetableForWeek
 ] -- List-comprehension.

-- | Unbook anything matching the specified predicate.
unbookWhere :: (Data.Array.IArray.Ix timeslotId, Ord observerId)
	=> (Model.Timetable.Booking observerId timeslotId resourceIds level -> Bool)	-- ^ Predicate used to select bookings.
	-> Model.Timetable.Timetable observerId timeslotId resourceIds level
	-> Model.Timetable.Timetable observerId timeslotId resourceIds level
unbookWhere predicate	= uncurry ($) . (Model.Timetable.undefineTimeslots &&& map Model.Timetable.getBookedCoordinates . filter predicate . fromTimetable)

