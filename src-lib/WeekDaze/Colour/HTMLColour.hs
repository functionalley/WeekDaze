{-
	Copyright (C) 2014-2015 Dr. Alistair Ward

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

 [@DESCRIPTION@] Exports some colour-composition functions for HTML.
-}

module WeekDaze.Colour.HTMLColour(
-- * Functions
	unitIntervalToHTMLColourCode,
	unitIntervalToRGB,
-- ** Translation
	htmlColourCodeToRGB,
	rgbUnitIntervalToHTMLColourCode
) where

import qualified	Text.Printf
import qualified	ToolShed.Data.Triple
import qualified	WeekDaze.Colour.HTMLColourCode	as Colour.HTMLColourCode
import qualified	WeekDaze.Colour.RGB		as Colour.RGB

-- | Represents the specified HTML colour-code as RGB 24-bit.
htmlColourCodeToRGB :: (Integral i, Show i) => Colour.HTMLColourCode.HTMLColourCode -> Colour.RGB.RGB i
htmlColourCodeToRGB	= Colour.RGB.fromIntegral' . Colour.HTMLColourCode.readHex . tail

-- | Represents the specified RGB as an HTML colour-code.
rgbUnitIntervalToHTMLColourCode
	:: RealFrac unitInterval
	=> Colour.RGB.RGB unitInterval	-- ^ Red, green & blue component-colours, each in the unit-interval.
	-> Colour.HTMLColourCode.HTMLColourCode
rgbUnitIntervalToHTMLColourCode rgb	= ToolShed.Data.Triple.uncurry3 (Text.Printf.printf $ Colour.HTMLColourCode.htmlColourCodePrefix : "%02x%02x%02x") $ Colour.RGB.toTriple (Colour.RGB.fromRGBUnitInterval rgb :: Colour.RGB.RGB Int)

{- |
	* Maps the specified value from the unit-interval to the corresponding spectral colour represented as RGB.

	* The mapping from the spectrum to RGB is performed by modeling the spectrum as a circular arc drawn on the colour triangle,
	of radius equal to the specified saturation, & centered on green.
	CAVEAT: this algorithm has no solid basis.

	* One may specify the angular span & angular offset of this arc, to define the values to which /0/ & /1/ map.
-}
unitIntervalToRGB :: (
	Fractional	alpha,
	Fractional	unitInterval,
	Real		alpha,
	Real		unitInterval,
	Show		alpha,
	Show		unitInterval
 )
	=> alpha	-- ^ The angular span in the range [0 .. 2 * pi radians], to which the unit-interval is mapped.
	-> alpha	-- ^ The angular offset in radians.
	-> unitInterval	-- ^ The saturation in the unit-interval; /0/ will always result in black.
	-> unitInterval	-- ^ The value in the unit-interval which is to be represented by a colour.
	-> Colour.RGB.RGB unitInterval
unitIntervalToRGB _ _ 0 _	= Colour.RGB.black
unitIntervalToRGB angularSpan angularOffset saturation x
	| angularSpan < 0 || realToFrac angularSpan > 2 * pi'	= error $ "WeekDaze.Colour.HTMLColour.unitIntervalToRGB:\tthe specified angular span must be in the range [0 .. 2 * Pi]; " ++ show angularSpan
	| saturation < 0 || saturation > 1			= error $ "WeekDaze.Colour.HTMLColour.unitIntervalToRGB:\tthe specified saturation must be in the unit-interval; " ++ show saturation
	| x < 0 || x > 1					= error $ "WeekDaze.Colour.HTMLColour.unitIntervalToRGB:\tthe specified value must be in the unit-interval; " ++ show x
	| otherwise						= let
		alpha	= realToFrac $ angularSpan * realToFrac x + angularOffset	-- Map the specified value onto a circular arc spanning the specified range of angles, offset by the specified angle.
	in (
		realToFrac . (* realToFrac saturation) . (/ 2) . (+ 1)	-- Translate & scale the result from [-1 .. 1] to [0 .. saturation].
	) `fmap` Colour.RGB.mkRGB (
		cos $ alpha + 5 * pi' / 6
	) (
		sin alpha
	) (
		cos $ alpha + pi' / 6
	)
	where
		pi' :: Double	-- Choose a working floating-point type.
		pi'	= pi

{- |
	* Calls 'unitIntervalToRGB', with default parameters to make /0/ map to /blue/ & /1/ to /red/, & with a moderately high saturation.

	* Calls 'rgbUnitIntervalToHTMLColourCode' to convert to an HTML colour-code.
-}
unitIntervalToHTMLColourCode :: (
	RealFrac	unitInterval,
	Show		unitInterval
 ) => unitInterval -> Colour.HTMLColourCode.HTMLColourCode
unitIntervalToHTMLColourCode	= rgbUnitIntervalToHTMLColourCode . unitIntervalToRGB (4 * pi / 3 :: Double) (negate pi / 6) (3 / 4) {-rather arbitrary-}

