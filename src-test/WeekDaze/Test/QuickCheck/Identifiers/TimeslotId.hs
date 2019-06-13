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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Identifiers.TimeslotId.TimeslotId'.
-}

module WeekDaze.Test.QuickCheck.Identifiers.TimeslotId(
-- * Constants
	results
) where

import qualified	Data.Char
import			Control.Arrow((&&&))
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO
import qualified	WeekDaze.Identifiers.TimeslotId	as Identifiers.TimeslotId

instance Bounded Identifiers.TimeslotId.TimeslotId where
	minBound	= Identifiers.TimeslotId.mkTimeslotId 0
	maxBound	= Identifiers.TimeslotId.mkTimeslotId 4

instance Test.QuickCheck.Arbitrary Identifiers.TimeslotId.TimeslotId where
	arbitrary	= Test.QuickCheck.elements [minBound .. maxBound]

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_readPrependedWhiteSpace,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_read,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 1024} prop_readTrailingGarbage
 ] where
	prop_readPrependedWhiteSpace :: Identifiers.TimeslotId.TimeslotId -> Test.QuickCheck.Property
	prop_readPrependedWhiteSpace	= Test.QuickCheck.label "prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace

	prop_read :: String -> Test.QuickCheck.Property
	prop_read garbage	= Test.QuickCheck.label "prop_read" $ case (reads garbage :: [(Identifiers.TimeslotId.TimeslotId, String)]) of
		[_]	-> True
		_	-> True	-- Unless the read-implementation throws an exception.

	prop_readTrailingGarbage :: Identifiers.TimeslotId.TimeslotId -> String -> Test.QuickCheck.Property
	prop_readTrailingGarbage timeslotId	= Test.QuickCheck.label "prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (
		uncurry (||) . (
			Data.Char.isDigit &&& (
				`elem` ".eox"	-- Avoid accidental specification of a decimal or exponential suffix, or erroneous interpretation as octal or hexadecimal.
			) . Data.Char.toLower
		)
	 ) timeslotId

