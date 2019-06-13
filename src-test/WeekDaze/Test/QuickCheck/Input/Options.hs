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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'Input.Options.Options'.
-}

module WeekDaze.Test.QuickCheck.Input.Options(
-- * Types
-- ** Type-synonyms
	Options
) where

import qualified	Test.QuickCheck
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions			as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.Identifiers.Campus						as Identifiers.Campus
import qualified	WeekDaze.Identifiers.Level						as Identifiers.Level
import qualified	WeekDaze.Identifiers.LocationId						as Identifiers.LocationId
import qualified	WeekDaze.Identifiers.SynchronisationId					as Identifiers.SynchronisationId
import qualified	WeekDaze.Identifiers.TeacherId						as Identifiers.TeacherId
import qualified	WeekDaze.Identifiers.TimeslotId						as Identifiers.TimeslotId
import qualified	WeekDaze.Input.Options							as Input.Options
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters				as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.Test.QuickCheck.Data.Student					as Test.Data.Student
import qualified	WeekDaze.Test.QuickCheck.ExecutionConfiguration.ExecutionOptions	as Test.ExecutionConfiguration.ExecutionOptions
import			WeekDaze.Test.QuickCheck.OutputConfiguration.Options()
import			WeekDaze.Test.QuickCheck.ProblemConfiguration.ProblemParameters()

instance (
	Enum				fecundityDecayRatio,
	Enum				populationDiversityRatio,
	Enum				timeslotId,
	Fractional			criterionWeight,
	Fractional			fecundityDecayRatio,
	Fractional			minimumContrastRatio,
	Fractional			populationDiversityRatio,
	Fractional			teachingRatio,
	Ord				level,
	Ord				locationId,
	Ord				minimumContrastRatio,
	Ord				synchronisationId,
	Ord				teacherId,
	Ord				timeslotId,
	Real				criterionWeight,
	Real				teachingRatio,
	Show				campus,
	Show				criterionWeight,
	Show				level,
	Show				locationId,
	Show				minimumContrastRatio,
	Show				synchronisationId,
	Show				timeslotId,
	Test.QuickCheck.Arbitrary	campus,
	Test.QuickCheck.Arbitrary	criterionWeight,
	Test.QuickCheck.Arbitrary	level,
	Test.QuickCheck.Arbitrary	locationId,
	Test.QuickCheck.Arbitrary	stream,
	Test.QuickCheck.Arbitrary	synchronisationId,
	Test.QuickCheck.Arbitrary	teacherId,
	Test.QuickCheck.Arbitrary	timeslotId
 ) => Test.QuickCheck.Arbitrary (Input.Options.Options campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId) where
	arbitrary	= do
		(configVersion, problemParameters, executionOptions, outputOptions)	<- Test.QuickCheck.arbitrary

		let
			options	= Input.Options.MkOptions {
				Input.Options.getConfigVersion		= configVersion,
				Input.Options.getExecutionOptions	= executionOptions,
				Input.Options.getOutputOptions		= outputOptions,
				Input.Options.getProblemParameters	= problemParameters
			}

-- Force a superficially redundant pass through those mutators which, as a side-effect, checks the compatibility of options.
		return {-to Gen-monad-} . Input.Options.setTimeslotIdBounds (
			ProblemConfiguration.ProblemParameters.getTimeslotIdBounds problemParameters
		 ) $ if ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger executionOptions
			then Input.Options.setPermitTemporaryStudentBodyMerger True options
			else options

-- | Defines a concrete type for testing.
type Options	= Input.Options.Options Identifiers.Campus.Campus Test.ExecutionConfiguration.ExecutionOptions.CriterionWeight Test.ExecutionConfiguration.ExecutionOptions.FecundityDecayRatio Identifiers.Level.Level Identifiers.LocationId.LocationId Test.ExecutionConfiguration.ExecutionOptions.PopulationDiversityRatio Identifiers.SynchronisationId.SynchronisationId Identifiers.TeacherId.TeacherId Test.Data.Student.TeachingRatio Identifiers.TimeslotId.TimeslotId

