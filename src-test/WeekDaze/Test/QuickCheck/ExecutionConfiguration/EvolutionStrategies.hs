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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'ExecutionConfiguration.EvolutionStrategies.EvolutionStrategies'.
-}

module WeekDaze.Test.QuickCheck.ExecutionConfiguration.EvolutionStrategies() where

import qualified	Test.QuickCheck
import qualified	WeekDaze.ExecutionConfiguration.EvolutionStrategies	as ExecutionConfiguration.EvolutionStrategies
import			WeekDaze.Test.QuickCheck.ExecutionConfiguration.TimetableBreederFecundity()

instance (
	Fractional	fecundityDecayRatio,
	Fractional	populationDiversityRatio
 ) => Test.QuickCheck.Arbitrary (ExecutionConfiguration.EvolutionStrategies.EvolutionStrategies fecundityDecayRatio populationDiversityRatio) where
	arbitrary	= do
		synchronisedCourseMutationFecundity		<- Test.QuickCheck.arbitrary
		synchronisedCourseByDayMutationFecundity	<- Test.QuickCheck.arbitrary
		excessRunlengthMutationFecundity		<- Test.QuickCheck.arbitrary
		homogeneousStudentViewLessonMutationFecundity	<- Test.QuickCheck.arbitrary
		incompleteCourseMutationFecundity		<- Test.QuickCheck.arbitrary
		randomLessonMutationFecundity			<- Test.QuickCheck.arbitrary
		singletonStudentClassMutationFecundity		<- Test.QuickCheck.arbitrary
		splitSessionMutationFecundity			<- Test.QuickCheck.arbitrary
		studentBodyCombinationMutationFecundity		<- Test.QuickCheck.arbitrary
		studentViewTimetableForDayMutationFecundity	<- Test.QuickCheck.arbitrary
		studentViewTimetableForWeekMutationFecundity	<- Test.QuickCheck.arbitrary
		synchronousLessonMutationFecundity		<- Test.QuickCheck.arbitrary
		randomLessonMutationNTrials			<- Test.QuickCheck.arbitrary
		randomLessonMutationNTimeslots			<- Test.QuickCheck.arbitrary
		studentViewTimetableForDayMutationMaybeNDays	<- Test.QuickCheck.arbitrary
		studentViewTimetableForWeekMutationNTrials	<- Test.QuickCheck.arbitrary
		studentViewTimetableForWeekMutationNTimeslots	<- Test.QuickCheck.arbitrary
		fecundityDecayRatio				<- (recip . fromIntegral) `fmap` Test.QuickCheck.elements [1 :: Int .. 10]
		minimumPopulationDiversityRatio			<- (recip . fromIntegral) `fmap` Test.QuickCheck.elements [1 :: Int .. 10]
		maybeNInitialScouts				<- Test.QuickCheck.elements $ Nothing : map Just [1 .. 5]

		return {-to Gen-monad-} ExecutionConfiguration.EvolutionStrategies.MkEvolutionStrategies {
			ExecutionConfiguration.EvolutionStrategies.getSynchronisedCourseMutationFecundity		= synchronisedCourseMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getSynchronisedCourseByDayMutationFecundity		= synchronisedCourseByDayMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getExcessRunlengthMutationFecundity			= excessRunlengthMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getHomogeneousStudentViewLessonMutationFecundity	= homogeneousStudentViewLessonMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getIncompleteCourseMutationFecundity			= incompleteCourseMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getRandomLessonMutationFecundity			= randomLessonMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getSingletonStudentClassMutationFecundity		= singletonStudentClassMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getSplitSessionMutationFecundity			= splitSessionMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getStudentBodyCombinationMutationFecundity		= studentBodyCombinationMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForDayMutationFecundity	= studentViewTimetableForDayMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForWeekMutationFecundity	= studentViewTimetableForWeekMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getSynchronousLessonMutationFecundity		= synchronousLessonMutationFecundity,
			ExecutionConfiguration.EvolutionStrategies.getRandomLessonMutationNTrials			= randomLessonMutationNTrials `mod` 10,
			ExecutionConfiguration.EvolutionStrategies.getRandomLessonMutationNTimeslots			= randomLessonMutationNTimeslots `mod` 10,
			ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForDayMutationMaybeNDays	= fmap (`mod` 10) studentViewTimetableForDayMutationMaybeNDays,
			ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForWeekMutationNTrials	= studentViewTimetableForWeekMutationNTrials `mod` 10,
			ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForWeekMutationNTimeslots	= studentViewTimetableForWeekMutationNTimeslots `mod` 10,
			ExecutionConfiguration.EvolutionStrategies.getFecundityDecayRatio				= fecundityDecayRatio,
			ExecutionConfiguration.EvolutionStrategies.getMinimumPopulationDiversityRatio			= minimumPopulationDiversityRatio,
			ExecutionConfiguration.EvolutionStrategies.getMaybeNInitialScouts				= maybeNInitialScouts
		}

