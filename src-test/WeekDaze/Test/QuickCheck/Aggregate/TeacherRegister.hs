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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Aggregate.TeacherRegister.TeacherRegister'.
-}

module WeekDaze.Test.QuickCheck.Aggregate.TeacherRegister(
-- * Types
-- ** Type-synonyms
--	TeacherRegister,
-- * Constants
	results
) where

import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Test.QuickCheck
import qualified	WeekDaze.Aggregate.TeacherRegister	as Aggregate.TeacherRegister
import qualified	WeekDaze.Data.Course			as Data.Course
import qualified	WeekDaze.Data.Resource			as Data.Resource
import qualified	WeekDaze.Identifiers.Level		as Identifiers.Level
import qualified	WeekDaze.Identifiers.LocationId		as Identifiers.LocationId
import qualified	WeekDaze.Identifiers.SynchronisationId	as Identifiers.SynchronisationId
import qualified	WeekDaze.Identifiers.TeacherId		as Identifiers.TeacherId
import qualified	WeekDaze.Identifiers.TimeslotId		as Identifiers.TimeslotId
import qualified	WeekDaze.Test.QuickCheck.Data.Teacher	as Test.Data.Teacher
import			WeekDaze.Test.QuickCheck.Identifiers.TeacherId()

#if !MIN_VERSION_QuickCheck(2,8,2)
import			ToolShed.Test.QuickCheck.Arbitrary.Map()
#endif

-- | Defines a concrete type for testing.
type TeacherRegister	= Aggregate.TeacherRegister.TeacherRegister Identifiers.TeacherId.TeacherId Identifiers.SynchronisationId.SynchronisationId Identifiers.Level.Level Identifiers.TimeslotId.TimeslotId Identifiers.LocationId.LocationId Test.Data.Teacher.TeachingRatio

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [
	prop_locationIds,
	prop_subjects,
	prop_findSuitableCourses,
	prop_countAvailableTeacherDays
 ] where
	prop_locationIds, prop_subjects, prop_findSuitableCourses, prop_countAvailableTeacherDays :: TeacherRegister -> Test.QuickCheck.Property
	prop_locationIds teacherRegister	= Test.QuickCheck.label "prop_locationIds" . Data.Foldable.all (`Aggregate.TeacherRegister.isInhabited` teacherRegister) $ Aggregate.TeacherRegister.extractDistinctOwnLocationIds teacherRegister

	prop_subjects teacherRegister	= Test.QuickCheck.label "prop_subjects" . not . Data.Foldable.any (\subject -> Data.Map.null $ Aggregate.TeacherRegister.findSuitableCourseByTeacherId 1 subject teacherRegister) $ Aggregate.TeacherRegister.extractDistinctSubjects teacherRegister

	prop_findSuitableCourses teacherRegister	= Test.QuickCheck.label "prop_findSuitableCourses" . not . Data.Foldable.any (
		\course -> Data.Map.null $ Aggregate.TeacherRegister.findSuitableCourseByTeacherId (Data.Maybe.fromMaybe maxBound $ Data.Course.getMaybeMaximumClassSize course) (Data.Course.getSubject course) teacherRegister
	 ) $ Aggregate.TeacherRegister.extractDistinctCourses teacherRegister

	prop_countAvailableTeacherDays teacherRegister	= Test.QuickCheck.label "prop_countAvailableTeacherDays" $ Aggregate.TeacherRegister.countAvailableTeacherDays teacherRegister == Data.Foldable.sum (Data.Map.map Data.Resource.countDaysPerWeekAvailable teacherRegister)

