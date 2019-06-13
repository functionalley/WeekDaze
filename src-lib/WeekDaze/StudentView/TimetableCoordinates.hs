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

	* Defines the coordinates, used to access any time-slot, in the conceptually /3-D/ data-structure of the timetable required by /student/s.

	* Each time-slot can contain a /lesson/-definition.
-}

module WeekDaze.StudentView.TimetableCoordinates(
-- * Types
-- ** Type-synonyms
	ObserverId,
	Coordinates,
	Vector
) where

import qualified	WeekDaze.Aggregate.StudentBody		as Aggregate.StudentBody
import qualified	WeekDaze.Model.TimetableCoordinates	as Model.TimetableCoordinates

-- | 'Timetable' is designed to be viewed by a /student-body/.
type ObserverId	= Aggregate.StudentBody.StudentBody

-- | Defines the three coordinates of a time-slot, within a /timetable/ designed to be viewed by a /student/.
type Coordinates timeslotId	= Model.TimetableCoordinates.Coordinates ObserverId timeslotId

-- | An ordered sequence of 'Coordinates'.
type Vector timeslotId	= [Coordinates timeslotId]

