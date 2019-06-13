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

 [@DESCRIPTION@]	The specification for a potential booking, of either a /lesson/ or nothing, at a /time/, in a /timetable/.
-}

module WeekDaze.Model.GeneralisedBooking(
-- * Types
-- ** Type-synonyms
	GeneralisedBooking,
-- * Functions
-- ** Accessors
	getCoordinates,
	getMaybeLesson
) where

import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.Model.TimetableCoordinates	as Model.TimetableCoordinates

-- | The details of a booking, of either a /lesson/ or nothing, at a /coordinate/, in the /timetable/ for an /observer/.
type GeneralisedBooking observerId timeslotId resourceIds level	= (Model.TimetableCoordinates.Coordinates observerId timeslotId, Maybe (Model.Lesson.Lesson resourceIds level))

-- | Accessor.
getCoordinates :: GeneralisedBooking observerId timeslotId resourceIds level -> Model.TimetableCoordinates.Coordinates observerId timeslotId
getCoordinates	= fst

-- | Accessor.
getMaybeLesson :: GeneralisedBooking observerId timeslotId resourceIds level -> Maybe (Model.Lesson.Lesson resourceIds level)
getMaybeLesson	= snd
