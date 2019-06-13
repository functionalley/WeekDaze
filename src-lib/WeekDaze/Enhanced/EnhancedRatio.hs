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

 [@DESCRIPTION@]

	* Defines class-instances for 'Data.Ratio.Ratio'.

	* CAVEAT: rational numbers are converted imprecisely to floating-point for representation as XML.
	To remedy the loss of precision on reading from XML, they're rounded to the /epsilon/ for IEEE double-precision <https://en.wikipedia.org/wiki/Machine_epsilon>.
-}

module WeekDaze.Enhanced.EnhancedRatio(
--	round'
) where

import qualified	Control.Arrow
import			Control.Arrow((***))
import qualified	Data.Ratio
import qualified	System.Random
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Truncate the rounding-error introduced by conversion to a rational number.
round' :: RealFrac r => r -> Rational
round'	= (`Data.Ratio.approxRational` doublePrecisionEpsilon) where
	doublePrecisionEpsilon	= recip 2 ^ floatDigits (undefined :: Double)

instance Integral i => HXT.XmlPickler (Data.Ratio.Ratio i) where
	xpickle	= HXT.xpWrap (
		fromRational . round',		-- Construct from a Double.
		\x -> realToFrac x :: Double	-- Deconstruct to a Double.
	 ) HXT.xpPrim

instance (Integral r, System.Random.Random r) => System.Random.Random (Data.Ratio.Ratio r) where
	random		= System.Random.randomR (0, 1)
	randomR bounds	= Control.Arrow.first (\x -> realToFrac (x :: Double)) . System.Random.randomR (realToFrac *** realToFrac $ bounds)
