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

	* Defines the coordinates, used to access any time-slot in the conceptually /3-D/ generic /timetable/.

	* Each time-slot can contain a /lesson/-definition.
-}

module WeekDaze.Model.TimetableCoordinates(
-- * Types
-- ** Type-synonyms
	Coordinates,
	Vector,
-- * Functions
-- ** Accessors
	getObserverId,
	getTime
) where

import qualified	WeekDaze.Temporal.Time	as Temporal.Time

{- |
	* Each /time-slot/ in a /timetable/ is defined by three coordinates, though the two temporal coordinates (/day/ & /timeslot-id/) have been paired to form a pseudo /time/.

	* The third /coordinate/, which depends on the intended /observer/, separates the weekly /timetable/ for one observer, from those for all other /observers/ of the same type.

	* The whole structure can be visualised as a cuboid, with an optional /lesson/ defined at each Cartesian coordinate.
-}
type Coordinates observerId timeslotId	= (observerId, Temporal.Time.Time timeslotId)

-- | Accessor.
getObserverId :: Coordinates observerId timeslotId -> observerId
getObserverId	= fst

-- | Accessor.
getTime :: Coordinates observerId timeslotId -> Temporal.Time.Time timeslotId
getTime	= snd

-- | An ordered sequence of 'Coordinates'.
type Vector observerId timeslotId	= [Coordinates observerId timeslotId]
