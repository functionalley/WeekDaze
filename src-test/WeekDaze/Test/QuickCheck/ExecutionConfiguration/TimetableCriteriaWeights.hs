{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'ExecutionConfiguration.TimetableCriteriaWeights.TimetableCriteriaWeights'.
-}

module WeekDaze.Test.QuickCheck.ExecutionConfiguration.TimetableCriteriaWeights() where

import qualified	Test.QuickCheck
import qualified	WeekDaze.ExecutionConfiguration.TimetableCriteriaWeights	as ExecutionConfiguration.TimetableCriteriaWeights
import			WeekDaze.Test.QuickCheck.ExecutionConfiguration.CriterionWeight()

instance (
	Fractional	criterionWeight,
	Real		criterionWeight
 ) => Test.QuickCheck.Arbitrary (ExecutionConfiguration.TimetableCriteriaWeights.TimetableCriteriaWeights criterionWeight) where
	arbitrary	= do
		weightOfMaximiseComplianceWithFreePeriodPreferences			<- Test.QuickCheck.arbitrary
		weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity		<- Test.QuickCheck.arbitrary
		weightOfMaximiseMeanStudentClassSize					<- Test.QuickCheck.arbitrary
		weightOfMaximiseSynchronisationOfSynchronisedCourses			<- Test.QuickCheck.arbitrary
		weightOfMaximiseWeightedMeanStudentBodyUtilisationRatio			<- Test.QuickCheck.arbitrary
		weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest	<- Test.QuickCheck.arbitrary
		weightOfMinimiseDispersionOfStudentFreePeriodsPerDay			<- Test.QuickCheck.arbitrary
		weightOfMinimiseDispersionOfTeacherFreePeriodsPerDay			<- Test.QuickCheck.arbitrary
		weightOfMinimiseDispersionOfTeacherWorkload				<- Test.QuickCheck.arbitrary
		weightOfMinimiseMeanInterCampusMigrationsOfStudents			<- Test.QuickCheck.arbitrary
		weightOfMinimiseMeanInterCampusMigrationsOfTeachers			<- Test.QuickCheck.arbitrary
		weightOfMinimiseMeanLocationChangesOfTeachers				<- Test.QuickCheck.arbitrary
		weightOfMinimiseMeanLocusOperandiOfTeachers				<- Test.QuickCheck.arbitrary
		weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge		<- Test.QuickCheck.arbitrary
		weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge		<- Test.QuickCheck.arbitrary
		weightOfMinimiseMeanStudentBodyCombinationsPerLesson			<- Test.QuickCheck.arbitrary
		weightOfMinimiseRatioOfConsecutiveEqualLessons				<- Test.QuickCheck.arbitrary
		weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay		<- Test.QuickCheck.arbitrary

		return {-to Gen-monad-} ExecutionConfiguration.TimetableCriteriaWeights.MkTimetableCriteriaWeights {
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseComplianceWithFreePeriodPreferences			= weightOfMaximiseComplianceWithFreePeriodPreferences,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity	= weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseMeanStudentClassSize					= weightOfMaximiseMeanStudentClassSize,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseSynchronisationOfSynchronisedCourses			= weightOfMaximiseSynchronisationOfSynchronisedCourses,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio		= weightOfMaximiseWeightedMeanStudentBodyUtilisationRatio,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest	= weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay			= weightOfMinimiseDispersionOfStudentFreePeriodsPerDay,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay			= weightOfMinimiseDispersionOfTeacherFreePeriodsPerDay,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseDispersionOfTeacherWorkload				= weightOfMinimiseDispersionOfTeacherWorkload,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanInterCampusMigrationsOfStudents			= weightOfMinimiseMeanInterCampusMigrationsOfStudents,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers			= weightOfMinimiseMeanInterCampusMigrationsOfTeachers,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanLocationChangesOfTeachers			= weightOfMinimiseMeanLocationChangesOfTeachers,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanLocusOperandiOfTeachers				= weightOfMinimiseMeanLocusOperandiOfTeachers,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge		= weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge	= weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson			= weightOfMinimiseMeanStudentBodyCombinationsPerLesson,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseRatioOfConsecutiveEqualLessons			= weightOfMinimiseRatioOfConsecutiveEqualLessons,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay		= weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay
		}

