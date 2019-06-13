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

	Define the type for the size of various quantities.
-}

module WeekDaze.Size(
-- * Types
-- ** Type-synonyms
	NDays,
	NTimetables,
	NLocations,
	NStudents,
	NTeachers,
	NTimeslots
) where

-- | A number of /day/s.
type NDays	= Int

-- | A number of /timetables/.
type NTimetables	= Int

-- | A number of /location/s.
type NLocations	= Int

-- | A number of /student/s.
type NStudents	= Int

-- | A number of /teacher/s.
type NTeachers	= Int

-- | A number of /time-slot/s.
type NTimeslots	= Int
