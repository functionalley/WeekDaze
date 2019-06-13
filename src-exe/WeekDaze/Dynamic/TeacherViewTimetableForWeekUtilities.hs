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

	Contains utilities dependent on both the static data derived from either configuration or command-line options,
	& on the dynamic state of the current 'TeacherView.TimetableForWeek.TimetableForWeek'.
-}

module WeekDaze.Dynamic.TeacherViewTimetableForWeekUtilities(
-- * Functions
	calculateUtilisationRatio
) where

import			Data.Map((!))
import qualified	Data.Array.IArray
import qualified	WeekDaze.Model.TimetableForWeek			as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis	as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters	as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.TeacherView.TimetableForWeek		as TeacherView.TimetableForWeek

{- |
	* The number of /lesson/s booked for the specified /teacher/, relative to the portion of the week for which they're contracted to teach.

	* This factors into the denominator, both the 'Temporal.Availability.Availability' of the /teacher/ (who may be part-time),
	& the ratio of that working-time for which they're contracted to teach rather than administer.
-}
calculateUtilisationRatio :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> teacherId
	-> TeacherView.TimetableForWeek.TimetableForWeek timeslotId locationId level
	-> teachingRatio
calculateUtilisationRatio problemParameters problemAnalysis teacherId	= Model.TimetableForWeek.calculateUtilisationRatio (
	ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis
 ) $ ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters ! teacherId

