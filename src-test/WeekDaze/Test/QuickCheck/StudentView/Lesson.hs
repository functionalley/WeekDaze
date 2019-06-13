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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'StudentView.Lesson.Lesson'.
-}

module WeekDaze.Test.QuickCheck.StudentView.Lesson(
-- * Types
-- ** Type-synonyms
	Lesson,
-- * Constants
	results
) where

import qualified	Data.Set
import qualified	Test.QuickCheck
import qualified	WeekDaze.Aggregate.StudentClass		as Aggregate.StudentClass
import qualified	WeekDaze.Identifiers.Level		as Identifiers.Level
import qualified	WeekDaze.Identifiers.LocationId		as Identifiers.LocationId
import qualified	WeekDaze.Identifiers.TeacherId		as Identifiers.TeacherId
import qualified	WeekDaze.LocationView.Lesson		as LocationView.Lesson
import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.Model.ResourceUser		as Model.ResourceUser
import qualified	WeekDaze.StudentView.Lesson		as StudentView.Lesson
import qualified	WeekDaze.StudentView.LessonResourceIds	as StudentView.LessonResourceIds
import qualified	WeekDaze.TeacherView.Lesson		as TeacherView.Lesson
import			Test.QuickCheck((==>))
import			ToolShed.Test.QuickCheck.Arbitrary.Set()
import			WeekDaze.Test.QuickCheck.Aggregate.StudentBody()
import			WeekDaze.Test.QuickCheck.Identifiers.LocationId()
import			WeekDaze.Test.QuickCheck.Identifiers.TeacherId()
import			WeekDaze.Test.QuickCheck.Model.Lesson()

instance (
	Test.QuickCheck.Arbitrary	locationId,
	Test.QuickCheck.Arbitrary	teacherId
 ) => Test.QuickCheck.Arbitrary (StudentView.LessonResourceIds.LessonResourceIds locationId teacherId) where
	arbitrary	= uncurry StudentView.LessonResourceIds.MkLessonResourceIds `fmap` Test.QuickCheck.arbitrary {-pair-}

-- | Defines a concrete type for testing.
type Lesson	= StudentView.Lesson.Lesson Identifiers.LocationId.LocationId Identifiers.TeacherId.TeacherId Identifiers.Level.Level

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_areMergeableWith,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_areMutuallyMergeable,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_areMergeable,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_alternativeView
 ] where
	prop_areMergeableWith :: Lesson -> Lesson -> Test.QuickCheck.Property
	prop_areMergeableWith l r = Test.QuickCheck.label "prop_areMergeableWith" $ Model.ResourceUser.areMergeableWith l [r] == Model.ResourceUser.areMergeableWith r [l]

	prop_areMutuallyMergeable :: Int -> Lesson -> Test.QuickCheck.Property
	prop_areMutuallyMergeable i	= Test.QuickCheck.label "prop_areMutuallyMergeable" . Model.ResourceUser.areMutuallyMergeable . replicate (i `mod` 10 {-arbitrarily-})

	prop_areMergeable :: [Lesson] -> Test.QuickCheck.Property
	prop_areMergeable l	= Test.QuickCheck.label "prop_areMergeable" $ if Model.ResourceUser.areMutuallyMergeable l'
		then all (`Model.ResourceUser.areMergeableWith` l') l'
		else any (not . (`Model.ResourceUser.areMergeableWith` l')) l'
		where
			l'	= take 3 {-arbitrarily-} l

	prop_alternativeView :: Aggregate.StudentClass.StudentClass -> Lesson -> Test.QuickCheck.Property
	prop_alternativeView studentClass lesson	= not (Data.Set.null studentClass) ==> Test.QuickCheck.label "prop_alternativeView" $ LocationView.Lesson.toStudentView (
		StudentView.LessonResourceIds.getLocationId $ Model.Lesson.getResourceIds lesson
	 ) (
		LocationView.Lesson.fromStudentView studentClass lesson
	 ) == TeacherView.Lesson.toStudentView (
		StudentView.LessonResourceIds.getTeacherId $ Model.Lesson.getResourceIds lesson
	 ) (
		TeacherView.Lesson.fromStudentView lesson studentClass
	 )

