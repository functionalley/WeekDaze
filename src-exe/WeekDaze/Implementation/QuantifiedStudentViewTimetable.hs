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

 [@DESCRIPTION@]	Encapsulates a /student-view timetable/ with an evaluation of its fitness for purpose.
-}

module WeekDaze.Implementation.QuantifiedStudentViewTimetable(
-- * Types
-- ** Data-types
	QuantifiedStudentViewTimetable(
--		MkQuantifiedStudentViewTimetable,
		getStudentViewTimetable,
		getTimetableCriteriaValues,
		getWeightedMeanOfTimetableCriteria
	),
-- * Functions
	calculateImprovementFactor,
-- ** Constructor
	mkQuantifiedStudentViewTimetable
) where

import qualified	Control.DeepSeq
import qualified	Control.Monad.Writer	-- The lazy instance.
import qualified	Data.Array.IArray
import qualified	ToolShed.Data.Pair
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.Implementation.TimetableFitness		as Implementation.TimetableFitness
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable

-- | A /timetable/ quantified by the individual weighted /timetable-criteria/ & their mean.
data QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue	= MkQuantifiedStudentViewTimetable {
	getStudentViewTimetable			:: StudentView.Timetable.Timetable timeslotId locationId teacherId level,	-- ^ The /timetable/.
	getWeightedMeanOfTimetableCriteria	:: weightedMean,								-- ^ The /fitness/ of the /timetable/, according to the weighted mean of the /timetable-criteria/.
	getTimetableCriteriaValues		:: [Maybe criterionValue]							-- ^ The /timetable-criteria/ values; 'Nothing' if the corresponding criterion-weight is zero.
}

instance (Data.Array.IArray.Ix timeslotId, Eq level, Eq locationId, Eq teacherId) => Eq (QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue) where
	l == r	= getStudentViewTimetable l == getStudentViewTimetable r	-- Stricter than mere comparison between 'getWeightedMeanOfTimetableCriteria'.

instance Control.DeepSeq.NFData weightedMean => Control.DeepSeq.NFData (QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue) where
	rnf	= Control.DeepSeq.rnf . getWeightedMeanOfTimetableCriteria	-- Evaluation of the other fields is a prerequisite for the 'weightedMean', but explicitly evaluating only the latter, requires a minimal context.

{- |
	* Constructor.

	* Takes a /timetable/ & quantifies it with the /weighted mean/ over all /timetable-criteria/.

	* Records the value of each /timetable-criterion/.
-}
mkQuantifiedStudentViewTimetable :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			criterionValue,
	Enum			timeslotId,
	Eq			campus,
	Fractional		criterionValue,
	Fractional		weightedMean,
	Ord			level,
	Ord			locationId,
	Ord			synchronisationId,
	Ord			teacherId,
	Real			criterionValue,
	Real			criterionWeight,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue
mkQuantifiedStudentViewTimetable problemParameters executionOptions problemAnalysis studentViewTimetable	= uncurry (
	MkQuantifiedStudentViewTimetable studentViewTimetable
 ) . Control.Monad.Writer.runWriter $ Implementation.TimetableFitness.evaluateFitness problemParameters executionOptions problemAnalysis studentViewTimetable

-- | Quantifies the factor by which the second /timetable/ is fitter than the first, according to the weighted mean of their /timetable-criteria/.
calculateImprovementFactor :: (
	Enum		weightedMean,
	Eq		weightedMean,
	Fractional	weightedMean
 )
	=> QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue	-- ^ The original against which to compare.
	-> QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue	-- ^ The proposed alternative.
	-> Maybe weightedMean											-- ^ Infinity is represented by 'Nothing'.
calculateImprovementFactor original proposed	= case ToolShed.Data.Pair.mirror getWeightedMeanOfTimetableCriteria (proposed, original) of
	(0, 0)	-> Just 0	-- Zero improvement.
	(_, 0)	-> Nothing	-- Infinite improvement.
	ratio	-> Just . pred $ uncurry (/) ratio

