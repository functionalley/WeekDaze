{-
	Copyright (C) 2014-2015 Dr. Alistair Ward

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

	* The structure produced by function "WeekDaze.Implementation.EvolutionaryAlgorithm.selectDeterministicStudentViewTimetable".
-}

module WeekDaze.OutputFormat.DeterministicStudentViewTimetableSelection(
-- * Types
-- ** Type-synonyms
--	QuantifiedTraversalOrder,
-- ** Data-types
	DeterministicStudentViewTimetableSelection(
--		MkDeterministicStudentViewTimetableSelection,
		getInitialTimetableCriteriaValues,
		getStudentViewTimetable,
		getInitialLessonCriteriaStatistics,
		getWeightedMeanOfTimetableCriteria
	),
-- ** Constructor
	mkDeterministicStudentViewTimetableSelection
) where

import qualified	WeekDaze.Implementation.DeterministicConstructor	as Implementation.DeterministicConstructor
import qualified	WeekDaze.Model.TimetableAxisTriple			as Model.TimetableAxisTriple
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable

-- | The weighted mean over all heterogeneous /timetable-criteria/, of the deterministic /timetable/ resulting from a specific raster-scan.
type QuantifiedTraversalOrder weightedMean	= (Model.TimetableAxisTriple.Axes, weightedMean);

-- | The return-type of 'Implementation.EvolutionaryAlgorithm.selectDeterministicStudentViewTimetable'.
data DeterministicStudentViewTimetableSelection criterionValue criterionWeight level locationId mean standardDeviation teacherId timeslotId weightedMean	= MkDeterministicStudentViewTimetableSelection {
	getStudentViewTimetable			:: StudentView.Timetable.Timetable timeslotId locationId teacherId level,						-- ^ The initial deterministic /student-view timetable/.
	getInitialLessonCriteriaStatistics	:: [Maybe (Implementation.DeterministicConstructor.LessonCriteriaStatistics criterionValue mean standardDeviation)],	-- ^ Statistics gathered from the initial deterministic /timetable/ resulting from the best raster-scan, over the value of each unweighted homogeneous /lesson-criterion/.
	getInitialTimetableCriteriaValues	:: [Maybe criterionValue],												-- ^ The heterogeneous /timetable-criteria/ ('Nothing' where the corresponding weight is zero), of the initial deterministic /timetable/ resulting from the best raster-scan.
	getWeightedMeanOfTimetableCriteria	:: Either weightedMean [QuantifiedTraversalOrder weightedMean]								-- ^ The weighted mean over all heterogeneous /timetable-criteria/, of either just the specified initial deterministic /timetable/, or the /timetable/ resulting from each traversal order.
} deriving Show

-- | Constructor.
mkDeterministicStudentViewTimetableSelection
	:: StudentView.Timetable.Timetable timeslotId locationId teacherId level						-- ^ The initial deterministic /student-view timetable/.
	-> [Maybe (Implementation.DeterministicConstructor.LessonCriteriaStatistics criterionValue mean standardDeviation)]	-- ^ Statistics gathered from the initial deterministic /timetable/ resulting from the best raster-scan, over the value of each unweighted homogeneous /lesson-criterion/.
	-> [Maybe criterionValue]												-- ^ The heterogeneous /timetable-criteria/, of the initial deterministic /timetable/ resulting from the best raster-scan.
	-> Either weightedMean [QuantifiedTraversalOrder weightedMean]								-- ^ The weighted mean over all heterogeneous /timetable-criteria/, of either the specified initial deterministic /timetable/, or for each traversal order.
	-> DeterministicStudentViewTimetableSelection criterionValue criterionWeight level locationId mean standardDeviation teacherId timeslotId weightedMean
mkDeterministicStudentViewTimetableSelection studentViewTimetable initialLessonCriteriaStatistics initialTimetableCriteria meanOfWeightedTimetableCriteriaByTraversalOrder	= MkDeterministicStudentViewTimetableSelection {
	getStudentViewTimetable			= studentViewTimetable,
	getInitialLessonCriteriaStatistics	= initialLessonCriteriaStatistics,
	getInitialTimetableCriteriaValues	= initialTimetableCriteria,
	getWeightedMeanOfTimetableCriteria	= meanOfWeightedTimetableCriteriaByTraversalOrder
}

