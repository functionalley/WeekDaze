{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'ProblemConfiguration.ProblemParameters.ProblemParameters'.
-}

module WeekDaze.Test.QuickCheck.ProblemConfiguration.ProblemParameters(
-- * Types
-- ** Type-synonyms
	ProblemParameters
) where

import qualified	Control.Arrow
import qualified	Data.Map
import qualified	Test.QuickCheck
import qualified	WeekDaze.Identifiers.Campus			as Identifiers.Campus
import qualified	WeekDaze.Identifiers.Level			as Identifiers.Level
import qualified	WeekDaze.Identifiers.LocationId			as Identifiers.LocationId
import qualified	WeekDaze.Identifiers.SynchronisationId		as Identifiers.SynchronisationId
import qualified	WeekDaze.Identifiers.TeacherId			as Identifiers.TeacherId
import qualified	WeekDaze.Identifiers.TimeslotId			as Identifiers.TimeslotId
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters	as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.Test.QuickCheck.Data.Teacher		as Test.Data.Teacher
import			WeekDaze.Test.QuickCheck.Aggregate.StudentBody()
import			WeekDaze.Test.QuickCheck.Data.Group()
import			WeekDaze.Test.QuickCheck.Data.Student()
import			WeekDaze.Test.QuickCheck.Identifiers.Stream()
import			WeekDaze.Test.QuickCheck.Identifiers.TeacherId()
import			WeekDaze.Test.QuickCheck.ProblemConfiguration.ProblemValidationSwitches()
import			WeekDaze.Test.QuickCheck.ProblemConfiguration.TimetableValidationSwitches()

#if !MIN_VERSION_QuickCheck(2,8,2)
import			ToolShed.Test.QuickCheck.Arbitrary.Map()
#endif

instance (
	Fractional			teachingRatio,
	Ord				level,
	Ord				locationId,
	Ord				synchronisationId,
	Ord				teacherId,
	Ord				timeslotId,
	Real				teachingRatio,
	Show				level,
	Show				locationId,
	Show				synchronisationId,
	Show				timeslotId,
	Show				campus,
	Test.QuickCheck.Arbitrary	level,
	Test.QuickCheck.Arbitrary	locationId,
	Test.QuickCheck.Arbitrary	stream,
	Test.QuickCheck.Arbitrary	synchronisationId,
	Test.QuickCheck.Arbitrary	teacherId,
	Test.QuickCheck.Arbitrary	timeslotId,
	Test.QuickCheck.Arbitrary	campus
 ) => Test.QuickCheck.Arbitrary (ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId) where
	arbitrary	= do
		(problemValidationSwitches, timetableValidationSwitches, locationCatalogue, studentBodyRegister, teacherRegister)	<- Test.QuickCheck.arbitrary
		groupCatalogue														<- fmap (
			Data.Map.fromList . map (Control.Arrow.first $ ('g' :) . show . (`mod` (10 :: Int))) . Data.Map.toList
		 ) Test.QuickCheck.arbitrary	-- Avoid constructing keys containing escaped characters.
		timeslotIdBounds													<- fmap (
			\bounds@(l, r) -> if l > r
				then (r, l)
				else bounds
		 ) Test.QuickCheck.arbitrary

		return {-to Gen-monad-} ProblemConfiguration.ProblemParameters.MkProblemParameters {
			ProblemConfiguration.ProblemParameters.getProblemValidationSwitches	= problemValidationSwitches,
			ProblemConfiguration.ProblemParameters.getTimetableValidationSwitches	= timetableValidationSwitches,
			ProblemConfiguration.ProblemParameters.getGroupCatalogue		= groupCatalogue,
			ProblemConfiguration.ProblemParameters.getLocationCatalogue		= locationCatalogue,
			ProblemConfiguration.ProblemParameters.getStudentBodyRegister		= studentBodyRegister,
			ProblemConfiguration.ProblemParameters.getTeacherRegister		= teacherRegister,
			ProblemConfiguration.ProblemParameters.getTimeslotIdBounds		= timeslotIdBounds
		}

-- | Defines a concrete type for testing.
type ProblemParameters	= ProblemConfiguration.ProblemParameters.ProblemParameters Identifiers.Campus.Campus Identifiers.Level.Level Identifiers.LocationId.LocationId Identifiers.SynchronisationId.SynchronisationId Identifiers.TeacherId.TeacherId Test.Data.Teacher.TeachingRatio Identifiers.TimeslotId.TimeslotId

