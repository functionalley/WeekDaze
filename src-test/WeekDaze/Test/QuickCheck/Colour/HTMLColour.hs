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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for module 'Colour.HTMLColour'.
-}

module WeekDaze.Test.QuickCheck.Colour.HTMLColour(
-- * Constants
	results,
-- * Functions
--	normalise
) where

import qualified	Test.QuickCheck
import qualified	WeekDaze.Colour.HTMLColour	as Colour.HTMLColour
import qualified	WeekDaze.Colour.HTMLColourCode	as Colour.HTMLColourCode
import qualified	WeekDaze.Colour.RGB		as Colour.RGB
import			WeekDaze.Colour.RGB((>+<))
import			WeekDaze.Test.QuickCheck.Colour.RGB()

-- | Map the integral magnitudes of each component colour onto the required range.
normalise :: Integral i => Colour.RGB.RGB i -> Colour.RGB.RGB i
normalise	= fmap $ (`mod` Colour.RGB.radix) . abs

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_deriveComplementaryHTMLColourCode,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_rgbUnitIntervalToHTMLColourCode,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_unitIntervalToRGB
 ] where
	prop_deriveComplementaryHTMLColourCode :: Colour.RGB.RGB Integer -> Test.QuickCheck.Property
	prop_deriveComplementaryHTMLColourCode rgb	= Test.QuickCheck.label "prop_deriveComplementaryHTMLColourCode" $ (
		== Colour.RGB.white24Bit
	 ) . (
		rgb' >+<
	 ) . Colour.HTMLColour.htmlColourCodeToRGB . Colour.HTMLColourCode.deriveComplementaryHTMLColourCode $ Colour.HTMLColour.rgbUnitIntervalToHTMLColourCode (
		Colour.RGB.toRGBUnitInterval rgb' :: Colour.RGB.RGB Rational
	 ) where
		rgb'	= normalise rgb

	prop_rgbUnitIntervalToHTMLColourCode :: Colour.RGB.RGB Integer -> Test.QuickCheck.Property
	prop_rgbUnitIntervalToHTMLColourCode rgb	= Test.QuickCheck.label "prop_rgbUnitIntervalToHTMLColourCode" . (== rgb') . Colour.HTMLColour.htmlColourCodeToRGB . Colour.HTMLColour.rgbUnitIntervalToHTMLColourCode $ (Colour.RGB.toRGBUnitInterval rgb' :: Colour.RGB.RGB Rational) where
		rgb'	= normalise rgb

	prop_unitIntervalToRGB :: Int -> Double -> Test.QuickCheck.Property
	prop_unitIntervalToRGB i angularOffset	= Test.QuickCheck.label "prop_unitIntervalToRGB" . (
		== (Colour.RGB.white24Bit :: Colour.RGB.RGB Int)
	 ) . Colour.RGB.fromRGBUnitInterval {-round off errors-} . fmap (
		(/ 3) . (* 2)	-- Each phasor averages 0, but after mapping to the closed unit interval, averages 1/2. Three such phasors were added to total 3/2, which we then correct to 1.
	 ) $ x >+< y >+< z where
		pi', i', saturation, angularSpan :: Double
		pi'		= pi
		i'		= fromIntegral (i `mod` Colour.RGB.radix) / fromInteger Colour.RGB.radix	-- Normalise to the unit interval.
		saturation	= 1
		angularSpan	= 2 * pi'

		x	= Colour.HTMLColour.unitIntervalToRGB angularSpan angularOffset saturation i'
		y	= Colour.HTMLColour.unitIntervalToRGB angularSpan (angularOffset + 2 * pi' / 3) saturation i'
		z	= Colour.HTMLColour.unitIntervalToRGB angularSpan (angularOffset + 4 * pi' / 3) saturation i'

