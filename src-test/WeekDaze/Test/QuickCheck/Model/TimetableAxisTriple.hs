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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'Model.TimetableAxisTriple.TimetableAxisTriple'.
-}

module WeekDaze.Test.QuickCheck.Model.TimetableAxisTriple(
-- * Constants
--	range1,
--	range2,
	results
) where

import qualified	Data.List
import qualified	Test.QuickCheck
import qualified	ToolShed.Data.List
import qualified	WeekDaze.Model.TimetableAxisTraversal	as Model.TimetableAxisTraversal
import qualified	WeekDaze.Model.TimetableAxisTriple	as Model.TimetableAxisTriple
import qualified	WeekDaze.Model.Traverse			as Model.Traverse
import			Test.QuickCheck((==>))
import			WeekDaze.Test.QuickCheck.Model.TimetableAxisTraversal()

instance Test.QuickCheck.Arbitrary Model.TimetableAxisTriple.Axes where
	arbitrary	= do
		(x, y, z)	<- Test.QuickCheck.arbitrary

		if (/= 3) . length $ Data.List.nubBy (ToolShed.Data.List.equalityBy Model.TimetableAxisTraversal.getAxis) [x, y, z]
			then Test.QuickCheck.arbitrary	-- Recurse.
			else return {-to Gen-monad-} $ Model.TimetableAxisTriple.mkAxes (x, y, z)

-- | A range of test coordinates.
range1 :: [Int]
range1	= [0 .. 1]

-- | A range of test coordinates.
range2 :: [Bool]
range2	= [minBound .. maxBound]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_generateRasterCoordinates,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_generateRasterCoordinatesSense,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_toList
 ] where
	prop_generateRasterCoordinates :: Model.TimetableAxisTriple.Axes -> Model.TimetableAxisTriple.Axes -> Test.QuickCheck.Property
	prop_generateRasterCoordinates triple1 triple2	= triple1 /= triple2 && not (
		any Model.TimetableAxisTriple.hasWildSense [triple1, triple2]
	 ) ==> Test.QuickCheck.label "prop_generateRasterCoordinates" $ Data.List.sort (
		Model.Traverse.generateRasterCoordinates triple1 range1 range2
	 ) == Data.List.sort (
		Model.Traverse.generateRasterCoordinates triple2 range1 range2
	 )

	prop_generateRasterCoordinatesSense, prop_toList :: Model.TimetableAxisTriple.Axes -> Test.QuickCheck.Property
	prop_generateRasterCoordinatesSense triple	= not (
		Model.TimetableAxisTriple.hasWildSense triple
	 ) ==> Test.QuickCheck.label "prop_generateRasterCoordinatesSense" $ Model.Traverse.generateRasterCoordinates triple range1 range2 == reverse (
		Model.Traverse.generateRasterCoordinates (Model.TimetableAxisTriple.invertSense triple) range1 range2
	 )
	prop_toList axes				= Test.QuickCheck.label "prop_toList" $ Model.TimetableAxisTriple.fromList (Model.TimetableAxisTriple.toList axes) == axes

