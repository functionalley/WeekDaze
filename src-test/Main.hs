{-
	Copyright (C) 2015 Dr. Alistair Ward

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

	* The entry-point to the application's test-suite.
-}

module Main(main) where

import qualified	Control.Monad
import qualified	System.Exit
import qualified	ToolShed.Test.QuickCheck.Result
import qualified	WeekDaze.Test.QuickCheck.Aggregate.LocationCatalogue				as Test.QuickCheck.Aggregate.LocationCatalogue
import qualified	WeekDaze.Test.QuickCheck.Aggregate.StudentBody					as Test.QuickCheck.Aggregate.StudentBody
import qualified	WeekDaze.Test.QuickCheck.Aggregate.StudentBodyRegister				as Test.QuickCheck.Aggregate.StudentBodyRegister
import qualified	WeekDaze.Test.QuickCheck.Aggregate.StudentClass					as Test.QuickCheck.Aggregate.StudentClass
import qualified	WeekDaze.Test.QuickCheck.Aggregate.TeacherRegister				as Test.QuickCheck.Aggregate.TeacherRegister
import qualified	WeekDaze.Test.QuickCheck.Colour.HTMLColour					as Test.QuickCheck.Colour.HTMLColour
import qualified	WeekDaze.Test.QuickCheck.Colour.RGB						as Test.QuickCheck.Colour.RGB
import qualified	WeekDaze.Test.QuickCheck.Data.Course						as Test.QuickCheck.Data.Course
import qualified	WeekDaze.Test.QuickCheck.Data.Location						as Test.QuickCheck.Data.Location
import qualified	WeekDaze.Test.QuickCheck.Data.Student						as Test.QuickCheck.Data.Student
import qualified	WeekDaze.Test.QuickCheck.Data.Subject						as Test.QuickCheck.Data.Subject
import qualified	WeekDaze.Test.QuickCheck.Data.Teacher						as Test.QuickCheck.Data.Teacher
import qualified	WeekDaze.Test.QuickCheck.ExecutionConfiguration.Criterion			as Test.QuickCheck.ExecutionConfiguration.Criterion
import qualified	WeekDaze.Test.QuickCheck.ExecutionConfiguration.OptimiseLessonCriteriaWeights	as Test.QuickCheck.ExecutionConfiguration.OptimiseLessonCriteriaWeights
import qualified	WeekDaze.Test.QuickCheck.Identifiers.Campus					as Test.QuickCheck.Identifiers.Campus
import qualified	WeekDaze.Test.QuickCheck.Identifiers.Level					as Test.QuickCheck.Identifiers.Level
import qualified	WeekDaze.Test.QuickCheck.Identifiers.LocationId					as Test.QuickCheck.Identifiers.LocationId
import qualified	WeekDaze.Test.QuickCheck.Identifiers.TeacherId					as Test.QuickCheck.Identifiers.TeacherId
import qualified	WeekDaze.Test.QuickCheck.Identifiers.TimeslotId					as Test.QuickCheck.Identifiers.TimeslotId
import qualified	WeekDaze.Test.QuickCheck.Input.ConfigVersion					as Test.QuickCheck.Input.ConfigVersion
import qualified	WeekDaze.Test.QuickCheck.Model.TimetableAxis					as Test.QuickCheck.Model.TimetableAxis
import qualified	WeekDaze.Test.QuickCheck.Model.TimetableAxisTraversal				as Test.QuickCheck.Model.TimetableAxisTraversal
import qualified	WeekDaze.Test.QuickCheck.Model.TimetableAxisTriple				as Test.QuickCheck.Model.TimetableAxisTriple
import qualified	WeekDaze.Test.QuickCheck.StudentView.Lesson					as Test.QuickCheck.StudentView.Lesson
import qualified	WeekDaze.Test.QuickCheck.StudentView.Timetable					as Test.QuickCheck.StudentView.Timetable
import qualified	WeekDaze.Test.QuickCheck.StudentView.TimetableForDay				as Test.QuickCheck.StudentView.TimetableForDay
import qualified	WeekDaze.Test.QuickCheck.StudentView.TimetableForWeek				as Test.QuickCheck.StudentView.TimetableForWeek
import qualified	WeekDaze.Test.QuickCheck.Temporal.Availability					as Test.QuickCheck.Temporal.Availability
import qualified	WeekDaze.Test.QuickCheck.Temporal.Day						as Test.QuickCheck.Temporal.Day
import qualified	WeekDaze.Test.QuickCheck.Temporal.Time						as Test.QuickCheck.Temporal.Time
import qualified	WeekDaze.Test.QuickCheck.Temporal.TimeslotRequest				as Test.QuickCheck.Temporal.TimeslotRequest
import			WeekDaze.Test.QuickCheck.Input.Options()	-- Merely to force compilation.

-- | Entry-point.
main :: IO ()
main	= mapM_ (
	(`Control.Monad.unless` System.Exit.exitFailure) . all ToolShed.Test.QuickCheck.Result.isSuccessful =<<
 ) [
	Test.QuickCheck.Aggregate.LocationCatalogue.results,
	Test.QuickCheck.Aggregate.StudentBodyRegister.results,
	Test.QuickCheck.Aggregate.StudentBody.results,
	Test.QuickCheck.Aggregate.StudentClass.results,
	Test.QuickCheck.Aggregate.TeacherRegister.results,
	Test.QuickCheck.Colour.HTMLColour.results,
	Test.QuickCheck.Colour.RGB.results,
	Test.QuickCheck.Data.Course.results,
	Test.QuickCheck.Data.Location.results,
	Test.QuickCheck.Data.Student.results,
	Test.QuickCheck.Data.Subject.results,
	Test.QuickCheck.Data.Teacher.results,
	Test.QuickCheck.ExecutionConfiguration.Criterion.results,
	Test.QuickCheck.ExecutionConfiguration.OptimiseLessonCriteriaWeights.results,
	Test.QuickCheck.Identifiers.Campus.results,
	Test.QuickCheck.Identifiers.Level.results,
	Test.QuickCheck.Identifiers.LocationId.results,
	Test.QuickCheck.Identifiers.TeacherId.results,
	Test.QuickCheck.Identifiers.TimeslotId.results,
	Test.QuickCheck.Input.ConfigVersion.results,
	Test.QuickCheck.Model.TimetableAxis.results,
	Test.QuickCheck.Model.TimetableAxisTraversal.results,
	Test.QuickCheck.Model.TimetableAxisTriple.results,
	Test.QuickCheck.StudentView.Lesson.results,
	Test.QuickCheck.StudentView.TimetableForDay.results,
	Test.QuickCheck.StudentView.TimetableForWeek.results,
	Test.QuickCheck.StudentView.Timetable.results,
	Test.QuickCheck.Temporal.Availability.results,
	Test.QuickCheck.Temporal.Day.results,
	Test.QuickCheck.Temporal.TimeslotRequest.results,
	Test.QuickCheck.Temporal.Time.results
 ]

