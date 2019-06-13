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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Temporal.Day.Day'.
-}

module WeekDaze.Test.QuickCheck.Temporal.Day(
-- * Constants
	results
) where

import qualified	Test.QuickCheck
import qualified	WeekDaze.Temporal.Day	as Temporal.Day

instance Test.QuickCheck.Arbitrary Temporal.Day.Day where
	arbitrary	= Test.QuickCheck.elements Temporal.Day.range

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [prop_getAdjacentDays] where
	prop_getAdjacentDays :: Temporal.Day.Day -> Test.QuickCheck.Property
	prop_getAdjacentDays day	= Test.QuickCheck.label "prop_getAdjacentDays" $ Temporal.Day.getTomorrow (Temporal.Day.getYesterday day) == day

