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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Aggregate.StudentBodyRegister.StudentBodyRegister'.
-}

module WeekDaze.Test.QuickCheck.Aggregate.StudentBodyRegister(
-- * Types
-- ** Type-synonyms
	StudentBodyRegister,
-- * Constants
	results
) where

import qualified	Data.Map
import qualified	Test.QuickCheck
import qualified	WeekDaze.Aggregate.StudentBodyRegister	as Aggregate.StudentBodyRegister
import qualified	WeekDaze.Aggregate.StudentClass		as Aggregate.StudentClass
import qualified	WeekDaze.Data.Student			as Data.Student
import qualified	WeekDaze.Identifiers.Level		as Identifiers.Level
import qualified	WeekDaze.Identifiers.Stream		as Identifiers.Stream
import			WeekDaze.Test.QuickCheck.Aggregate.StudentBody()
import qualified	WeekDaze.Test.QuickCheck.Data.Student	as Test.Data.Student

#if !MIN_VERSION_QuickCheck(2,8,2)
import			ToolShed.Test.QuickCheck.Arbitrary.Map()
#endif

-- | Defines a concrete type for testing.
type StudentBodyRegister	= Aggregate.StudentBodyRegister.StudentBodyRegister Identifiers.Level.Level Identifiers.Stream.Stream Test.Data.Student.TeachingRatio

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [prop_getSize, prop_countAvailableStudentDays, prop_countSubjectsRequired] where
	mnemonicSeparator :: Aggregate.StudentClass.MnemonicSeparator
	mnemonicSeparator	= " / "

	prop_getSize, prop_countAvailableStudentDays, prop_countSubjectsRequired :: StudentBodyRegister -> Test.QuickCheck.Property
	prop_getSize studentBodyRegister	= Test.QuickCheck.label "prop_getSize" $ Data.Map.size (Aggregate.StudentBodyRegister.reduce' mnemonicSeparator studentBodyRegister) <= Data.Map.size studentBodyRegister

	prop_countAvailableStudentDays studentBodyRegister	= Test.QuickCheck.label "prop_countAvailableStudentDays" $ Aggregate.StudentBodyRegister.countAvailableStudentDays (
		Aggregate.StudentBodyRegister.reduce' mnemonicSeparator studentBodyRegister
	 ) == Aggregate.StudentBodyRegister.countAvailableStudentDays studentBodyRegister

	prop_countSubjectsRequired studentBodyRegister	= Test.QuickCheck.label "prop_countSubjectsRequired" $ Aggregate.StudentBodyRegister.countSubjectsRequired (
		Data.Map.map Data.Student.deriveAmalgamatedKnowledgeRequirement $ Aggregate.StudentBodyRegister.reduce' mnemonicSeparator studentBodyRegister
	 ) == Aggregate.StudentBodyRegister.countSubjectsRequired (
		Data.Map.map Data.Student.deriveAmalgamatedKnowledgeRequirement studentBodyRegister
	 )

