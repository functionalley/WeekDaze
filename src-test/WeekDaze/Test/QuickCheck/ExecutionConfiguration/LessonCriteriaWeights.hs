{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2013-2016 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'ExecutionConfiguration.LessonCriteriaWeights.LessonCriteriaWeights'.
-}

module WeekDaze.Test.QuickCheck.ExecutionConfiguration.LessonCriteriaWeights() where

import qualified	Test.QuickCheck
import qualified	WeekDaze.ExecutionConfiguration.LessonCriteriaWeights	as ExecutionConfiguration.LessonCriteriaWeights
import			WeekDaze.Test.QuickCheck.ExecutionConfiguration.CriterionWeight()

instance (
	Fractional	criterionWeight,
	Real		criterionWeight
 ) => Test.QuickCheck.Arbitrary (ExecutionConfiguration.LessonCriteriaWeights.LessonCriteriaWeights criterionWeight) where
	arbitrary	= do
		weightOfAreResourcesReused				<- Test.QuickCheck.arbitrary
		weightOfGreatestMinimumConsecutiveLessons		<- Test.QuickCheck.arbitrary
		weightOfGreatestRemainingCourseLessons			<- Test.QuickCheck.arbitrary
		weightOfGreatestSynchronisedCourseSetSize		<- Test.QuickCheck.arbitrary
		weightOfIsCoreKnowledgeRequirement			<- Test.QuickCheck.arbitrary
		weightOfIsSpecialistInTopic				<- Test.QuickCheck.arbitrary
		weightOfMatchCourseClassSizeToLocationCapacity		<- Test.QuickCheck.arbitrary
		weightOfMaximiseRelativeFacilityUtilisation		<- Test.QuickCheck.arbitrary
		weightOfMaximiseStudentClassSizeOverCourseClassSize	<- Test.QuickCheck.arbitrary
		weightOfMaximiseStudentClassSizeOverLocationCapacity	<- Test.QuickCheck.arbitrary
		weightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	<- Test.QuickCheck.arbitrary
		weightOfMinimiseBookingOfLocationByOtherTeachers	<- Test.QuickCheck.arbitrary
		weightOfMinimiseDeviationFromTimeslotRequest		<- Test.QuickCheck.arbitrary
		weightOfMinimiseInterCampusMigrationsOfStudents		<- Test.QuickCheck.arbitrary
		weightOfMinimiseInterCampusMigrationsOfTeachers		<- Test.QuickCheck.arbitrary
		weightOfMinimiseStudentBodyCombinations			<- Test.QuickCheck.arbitrary
		weightOfMinimiseTeachersLocusOperandi			<- Test.QuickCheck.arbitrary
		weightOfMinimiseWasteOfScarceFacilities			<- Test.QuickCheck.arbitrary

		return {-to Gen-monad-} ExecutionConfiguration.LessonCriteriaWeights.MkLessonCriteriaWeights {
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfAreResourcesReused				= weightOfAreResourcesReused,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfGreatestMinimumConsecutiveLessons		= weightOfGreatestMinimumConsecutiveLessons,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfGreatestRemainingCourseLessons			= weightOfGreatestRemainingCourseLessons,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfGreatestSynchronisedCourseSetSize		= weightOfGreatestSynchronisedCourseSetSize,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfIsCoreKnowledgeRequirement			= weightOfIsCoreKnowledgeRequirement,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfIsSpecialistInTopic				= weightOfIsSpecialistInTopic,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMatchCourseClassSizeToLocationCapacity		= weightOfMatchCourseClassSizeToLocationCapacity,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseRelativeFacilityUtilisation		= weightOfMaximiseRelativeFacilityUtilisation,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverCourseClassSize	= weightOfMaximiseStudentClassSizeOverCourseClassSize,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverLocationCapacity	= weightOfMaximiseStudentClassSizeOverLocationCapacity,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	= weightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseBookingOfLocationByOtherTeachers	= weightOfMinimiseBookingOfLocationByOtherTeachers,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseDeviationFromTimeslotRequest		= weightOfMinimiseDeviationFromTimeslotRequest,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseInterCampusMigrationsOfStudents		= weightOfMinimiseInterCampusMigrationsOfStudents,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseInterCampusMigrationsOfTeachers		= weightOfMinimiseInterCampusMigrationsOfTeachers,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseStudentBodyCombinations			= weightOfMinimiseStudentBodyCombinations,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseTeachersLocusOperandi			= weightOfMinimiseTeachersLocusOperandi,
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseWasteOfScarceFacilities			= weightOfMinimiseWasteOfScarceFacilities
		}

