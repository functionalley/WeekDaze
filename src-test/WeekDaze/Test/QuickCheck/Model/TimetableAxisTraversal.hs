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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'Model.TimetableAxisTraversal.TimetableAxisTraversal'.
-}

module WeekDaze.Test.QuickCheck.Model.TimetableAxisTraversal(
-- * Constants
	results
) where

import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO
import qualified	WeekDaze.Model.TimetableAxisTraversal	as Model.TimetableAxisTraversal
import			WeekDaze.Test.QuickCheck.Model.TimetableAxis()

instance Test.QuickCheck.Arbitrary Model.TimetableAxisTraversal.AxisTraversal where
	arbitrary	= uncurry Model.TimetableAxisTraversal.MkAxisTraversal `fmap` Test.QuickCheck.arbitrary {-pair-}

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_readPrependedWhiteSpace,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_read
--	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_readTrailingGarbage	-- CAVEAT: the derived implementation of "Read" for "Model.TimetableAxis.TimetableAxis" fails.
 ] where
	prop_readPrependedWhiteSpace :: Model.TimetableAxisTraversal.AxisTraversal -> Test.QuickCheck.Property
	prop_readPrependedWhiteSpace	= Test.QuickCheck.label "prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace

	prop_read :: String -> Test.QuickCheck.Property
	prop_read garbage	= Test.QuickCheck.label "prop_read" $ case (reads garbage :: [(Model.TimetableAxisTraversal.AxisTraversal, String)]) of
		[_]	-> True
		_	-> True	-- Unless the read-implementation throws an exception.

	prop_readTrailingGarbage :: Model.TimetableAxisTraversal.AxisTraversal -> String -> Test.QuickCheck.Property
	prop_readTrailingGarbage axisTraversal	= Test.QuickCheck.label "prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (`elem` "+-ODT") axisTraversal

