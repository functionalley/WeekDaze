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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'ExecutionConfiguration.ExecutionOptions.ExecutionOptions'.
-}

module WeekDaze.Test.QuickCheck.ExecutionConfiguration.ExecutionOptions(
-- * Types
-- ** Type-synonyms
	CriterionWeight,
	FecundityDecayRatio,
	PopulationDiversityRatio
) where

import qualified	Test.QuickCheck
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import			WeekDaze.Test.QuickCheck.ExecutionConfiguration.EvolutionStrategies()
import			WeekDaze.Test.QuickCheck.ExecutionConfiguration.LessonCriteriaWeights()
import			WeekDaze.Test.QuickCheck.ExecutionConfiguration.OptimiseLessonCriteriaWeights()
import			WeekDaze.Test.QuickCheck.ExecutionConfiguration.TimetableCriteriaWeights()
import			WeekDaze.Test.QuickCheck.Model.TimetableAxisTriple()

instance (
	Enum				fecundityDecayRatio,
	Enum				populationDiversityRatio,
	Fractional			criterionWeight,
	Fractional			fecundityDecayRatio,
	Fractional			populationDiversityRatio,
	Real				criterionWeight,
	Show				criterionWeight,
	Test.QuickCheck.Arbitrary	criterionWeight
 ) => Test.QuickCheck.Arbitrary (ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio) where
	arbitrary	= do
		evolutionStrategies			<- Test.QuickCheck.arbitrary
		maybeHint				<- Test.QuickCheck.oneof [
			fmap (Just . Left) Test.QuickCheck.arbitrary,
			fmap (Just . Right . ('f' :) . show . (`mod` (10 :: Int))) Test.QuickCheck.arbitrary,
			return {-to Gen-monad-} Nothing
		 ]
		maybeRandomSeed				<- Test.QuickCheck.arbitrary
		optimiseLessonCriteriaWeights		<- Test.QuickCheck.arbitrary
		lessonCriteriaWeights			<- Test.QuickCheck.arbitrary
		permitTemporaryStudentBodyMerger	<- Test.QuickCheck.arbitrary
		reduceStudentBodyRegister		<- Test.QuickCheck.arbitrary
		removeRedundantCourses			<- Test.QuickCheck.arbitrary
		removePointlessGroups			<- Test.QuickCheck.arbitrary
		removeUnsubscribedGroups		<- Test.QuickCheck.arbitrary
		timetableCriteriaWeights		<- Test.QuickCheck.arbitrary
		zeroInappropriateOptions		<- Test.QuickCheck.arbitrary

		return {-to Gen-monad-} . ExecutionConfiguration.ExecutionOptions.setPermitTemporaryStudentBodyMerger permitTemporaryStudentBodyMerger $ ExecutionConfiguration.ExecutionOptions.MkExecutionOptions {
			ExecutionConfiguration.ExecutionOptions.getEvolutionStrategies			= evolutionStrategies,
			ExecutionConfiguration.ExecutionOptions.getMaybeHint				= maybeHint,
			ExecutionConfiguration.ExecutionOptions.getMaybeRandomSeed			= maybeRandomSeed,
			ExecutionConfiguration.ExecutionOptions.getOptimiseLessonCriteriaWeights	= optimiseLessonCriteriaWeights,
			ExecutionConfiguration.ExecutionOptions.getLessonCriteriaWeights		= lessonCriteriaWeights,
			ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger	= undefined,	-- CAVEAT: this must be coordinated with various criteria-weights.
			ExecutionConfiguration.ExecutionOptions.getReduceStudentBodyRegister		= reduceStudentBodyRegister,
			ExecutionConfiguration.ExecutionOptions.getRemoveRedundantCourses		= removeRedundantCourses,
			ExecutionConfiguration.ExecutionOptions.getRemovePointlessGroups		= removePointlessGroups,
			ExecutionConfiguration.ExecutionOptions.getRemoveUnsubscribedGroups		= removeUnsubscribedGroups,
			ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights		= timetableCriteriaWeights,
			ExecutionConfiguration.ExecutionOptions.getZeroInappropriateOptions		= zeroInappropriateOptions
		}

-- | Defines a concrete type for testing.
-- type CriterionWeight		= Rational
type CriterionWeight		= Double

-- | Defines a concrete type for testing.
-- type FecundityDecayRatio	= Rational
type FecundityDecayRatio	= Double

-- | Defines a concrete type for testing.
-- type PopulationDiversityRatio	= Rational
type PopulationDiversityRatio	= Double

