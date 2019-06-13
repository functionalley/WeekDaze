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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Aggregate.StudentClass.StudentClass'.
-}

module WeekDaze.Test.QuickCheck.Aggregate.StudentClass(
-- * Constants
	results
) where

import qualified	Data.Set
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))
import			ToolShed.Test.QuickCheck.Arbitrary.Set()
import qualified	WeekDaze.Aggregate.StudentBody	as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentClass	as Aggregate.StudentClass
import			WeekDaze.Test.QuickCheck.Aggregate.StudentBody()

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [prop_merge] where
	prop_merge :: Aggregate.StudentClass.StudentClass -> Test.QuickCheck.Property
	prop_merge studentClass		= not (Data.Set.null studentClass) ==> Test.QuickCheck.label "prop_merge" $ Aggregate.StudentBody.getSize (Aggregate.StudentClass.merge " / " studentClass) <= Aggregate.StudentClass.getSize studentClass -- The test-data could contain one student in more than one student-body.

