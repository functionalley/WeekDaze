{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for module 'Colour.RGB'.
-}

module WeekDaze.Test.QuickCheck.Colour.RGB(
-- * Constants
	results
) where

import qualified	Test.QuickCheck
import qualified	ToolShed.Data.Triple
import qualified	WeekDaze.Colour.RGB	as Colour.RGB

instance (
	Test.QuickCheck.Arbitrary	primaryColour
 ) => Test.QuickCheck.Arbitrary (Colour.RGB.RGB primaryColour) where
	arbitrary	= ToolShed.Data.Triple.uncurry3 Colour.RGB.mkRGB `fmap` Test.QuickCheck.arbitrary {-triple-}

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_absDifference,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_rgbUnitInterval
 ] where
	prop_absDifference :: Colour.RGB.RGB Integer -> Colour.RGB.RGB Integer -> Test.QuickCheck.Property
	prop_absDifference rgbL rgbR	= Test.QuickCheck.label "prop_absDifference" $ Colour.RGB.absDifference rgbL rgbR == Colour.RGB.absDifference rgbR rgbL

	prop_rgbUnitInterval :: Colour.RGB.RGB Integer -> Test.QuickCheck.Property
	prop_rgbUnitInterval rgb	= Test.QuickCheck.label "prop_rgbUnitInterval" $ Colour.RGB.fromRGBUnitInterval (Colour.RGB.toRGBUnitInterval rgb :: Colour.RGB.RGB Rational) == rgb

