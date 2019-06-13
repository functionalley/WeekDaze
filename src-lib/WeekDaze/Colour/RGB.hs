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

 [@DESCRIPTION@] Exports a representation of a colour as primary components.
-}

module WeekDaze.Colour.RGB(
-- * Types
-- ** Data-types
	RGB(),
-- * Constants
	black,
	white24Bit,
	radix,
--	saturated,
-- * Functions
	absDifference,
--	combine,
	toTriple,
-- ** Constructors
	mkRGB,
--	mkRGBUnitInterval,
	mkRGB24Bit,
	fromIntegral',
-- ** Operators
--	(>-<),
	(>+<),
-- ** Translation
	toRGBUnitInterval,
	fromRGBUnitInterval
) where

import qualified	Control.Arrow

infixl 6 >+<	-- Like (+).
infixl 6 >-<	-- Like (-).

-- | The representation of a colour as three primary components.
data RGB primaryColour	= MkRGB primaryColour primaryColour primaryColour	deriving (Eq, Show)

-- | Constructor.
mkRGB
	:: a	-- ^ Red.
	-> a	-- ^ Green.
	-> a	-- ^ Blue.
	-> RGB a
mkRGB = MkRGB

instance Functor RGB where
	f `fmap` MkRGB red green blue	= mkRGB (f red) (f green) (f blue)	-- CAVEAT: the bounds are neither checked nor even known.

-- | Smart-constructor.
mkRGBUnitInterval
	:: (Fractional a, Ord a, Show a)
	=> a	-- ^ Red.
	-> a	-- ^ Green.
	-> a	-- ^ Blue.
	-> RGB a
mkRGBUnitInterval red green blue
	| any ($ [red, green, blue]) [any (< 0), any (> 1)]	= error $ "WeekDaze.Colour.RGB.mkRGBUnitInterval:\teach of the specified fractional component-colours must be in the unit-interval; " ++ show (red, green, blue)
	| otherwise						= mkRGB red green blue

-- | Smart-constructor.
mkRGB24Bit
	:: (Integral a, Show a)
	=> a	-- ^ Red.
	-> a	-- ^ Green.
	-> a	-- ^ Blue.
	-> RGB a
mkRGB24Bit red green blue
	| any ($ [red, green, blue]) [any (< 0), any (> saturated)]	= error $ "WeekDaze.Colour.RGB.mkRGB24Bit:\teach of the specified integral component-colours must be in the closed interval [0," ++ show (saturated :: Int) ++ "]; " ++ show (red, green, blue)
	| otherwise							= mkRGB red green blue

-- | Constant base of number-system used to represent 24-bit colours in HTML.
radix :: Integral i => i
radix	= 2 ^ (8 :: Int)

-- | Represents the specified positive integral value as RGB 24-bit.
fromIntegral' :: (Integral i, Show i) => i -> RGB i
fromIntegral' i
	| i < 0				= error $ "WeekDaze.Colour.RGB.fromIntegral':\tthe specified value can't be negative; " ++ show i
	| i > radix ^ (3 :: Int)	= error $ "WeekDaze.Colour.RGB.fromIntegral':\tthe specified value is too large; " ++ show i
	| otherwise		= (
		\((red, green), blue)	-> mkRGB24Bit red green blue
	) . Control.Arrow.first (`divMod` radix) $ i `divMod` radix

-- | Deconstructor.
toTriple :: RGB a -> (a, a, a)
toTriple (MkRGB red green blue)	= (red, green, blue)

-- | The constant base of number-system used to represent colours in HTML.
saturated :: Integral i => i
saturated	= pred radix

-- | The constant representation of /black/ in RGB.
black :: Num n => RGB n
black	= mkRGB 0 0 0

-- | The constant representation of /white/ in RGB.
white24Bit :: (Integral n, Show n) => RGB n
white24Bit	= mkRGB24Bit saturated saturated saturated

-- | Combine two RGB-representations using the specified function to combine each primary colour.
combine
	:: (a -> a -> a)
	-> RGB a
	-> RGB a
	-> RGB a
combine f (MkRGB rL gL bL) (MkRGB rR gR bR)	= mkRGB (rL `f` rR) (gL `f` gR) (bL `f` bR)

{- |
	* Add the matching component colours.

	* CAVEAT: this may result in overflow.
-}
(>+<) :: Num n => RGB n -> RGB n -> RGB n
(>+<)	= combine (+)

{- |
	* Find the difference between the matching component colours.

	* CAVEAT: negative values may result.
-}
(>-<) :: Num n => RGB n -> RGB n -> RGB n
(>-<)	= combine (-)

-- | Find the absolute difference between the matching component colours.
absDifference :: Num n => RGB n -> RGB n -> RGB n
absDifference x y	= abs `fmap` (x >-< y)

-- | Convert RGB 24-bit to RGB UnitInterval, i.e. map from the closed interval [0,255] to the semi-closed interval [0,1).
toRGBUnitInterval :: (Integral i, Fractional f) => RGB i -> RGB f
toRGBUnitInterval	= fmap $ (/ fromInteger radix) . fromIntegral

-- | Convert RGB UnitInterval to RGB 24-bit, i.e. map from the semi-closed interval [0,1) to the closed interval [0,255].
fromRGBUnitInterval :: (Integral i, RealFrac f) => RGB f -> RGB i
fromRGBUnitInterval	= fmap $ min saturated {-CAVEAT: regrettably, we actually have to work with the closed interval [0,1] and 1 must still map to 255 not 256 (i.e. 0 in modulo-256 arithmetic-} . floor . (* fromInteger radix)

