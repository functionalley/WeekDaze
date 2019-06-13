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

 [@DESCRIPTION@]	A flattened /timetable/, for one week, without any undefined /time-slot/s.
-}

module WeekDaze.LinearModel.TimetableForWeek(
-- * Types
-- ** Type-synonyms
	TimetableForWeek,
-- * Functions
-- ** Translation
	fromTimetableForWeek
) where

import qualified	Data.Array.IArray
import qualified	WeekDaze.Model.TimetableForWeek	as Model.TimetableForWeek
import qualified	WeekDaze.Temporal.Time		as Temporal.Time

-- | A flattened /timetable/, with undefined /time-slot/s removed.
type TimetableForWeek timeslotId resourceIds level	= [Model.TimetableForWeek.Booking timeslotId resourceIds level]

-- | Serialises the /lesson/-definitions in the weekly /timetable/.
fromTimetableForWeek :: Data.Array.IArray.Ix timeslotId => Model.TimetableForWeek.TimetableForWeek timeslotId resourceIds level -> TimetableForWeek timeslotId resourceIds level
fromTimetableForWeek timetableForWeek	= [
	(Temporal.Time.mkTime day timeslotId, lesson) |
		(day, timetableForDay)				<- Data.Array.IArray.assocs timetableForWeek,
		(timeslotId, Just {-i.e. allocated-} lesson)	<- Data.Array.IArray.assocs timetableForDay
 ] -- List-comprehension.

