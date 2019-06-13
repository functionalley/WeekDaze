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
	& on the dynamic state of the current 'LocationView.Timetable.Timetable'.
-}

module WeekDaze.Dynamic.LocationViewTimetableUtilities(
-- * Functions
-- ** Translation
	fromStudentViewTimetable
) where

import qualified	Data.Array.IArray
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.LocationView.Timetable				as LocationView.Timetable
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable

-- | A convenient interface to 'LocationView.Timetable.fromStudentViewTimetable'.
fromStudentViewTimetable :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			teacherId,
	Ord			locationId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio	-- ^ Static configuration-data.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level							-- ^ The /timetable/ to invert.
	-> LocationView.Timetable.Timetable locationId timeslotId teacherId level
fromStudentViewTimetable executionOptions problemAnalysis	= LocationView.Timetable.fromStudentViewTimetable (
	ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger executionOptions
 ) $ ProblemConfiguration.ProblemAnalysis.getFreeLocationViewTimetable problemAnalysis

