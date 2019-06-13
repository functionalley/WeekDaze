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

-}

module WeekDaze.Temporal.TimeAxes(
-- * Types
-- ** Data-types
	TimeAxes(
--		MkTimeAxes,
		getByDay,
		getByTimeslotId
	),
-- * Constants
--	tag,
--	dayTag,
--	byTimeslotIdTag,
-- * Functions
-- ** Constructor
	mkTimeAxes
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

-- | Defines a concept over the two Cartesian axes used to graph it by /time/.
data TimeAxes a = MkTimeAxes {
	getByDay	:: a,	-- ^ Accessor.
	getByTimeslotId	:: a	-- ^ Accessor.
} deriving (Eq, Show)

-- | Constructor.
mkTimeAxes :: a -> a -> TimeAxes a
mkTimeAxes	= MkTimeAxes

-- | Used to qualify XML.
tag :: String
tag		= "timeAxes"

-- | Used to qualify XML.
byDayTag :: String
byDayTag	= "byDay"

-- | Used to qualify XML.
byTimeslotIdTag :: String
byTimeslotIdTag	= "byTimeslotId"

instance HXT.XmlPickler a => HXT.XmlPickler (TimeAxes a) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		uncurry MkTimeAxes,		-- Construct from a Pair.
		getByDay &&& getByTimeslotId	-- Deconstruct to a Pair.
	 ) $ HXT.xpAttr byDayTag HXT.xpickle {-Bool-} `HXT.xpPair` HXT.xpAttr byTimeslotIdTag HXT.xpickle {-Bool-}

instance Control.DeepSeq.NFData	a => Control.DeepSeq.NFData (TimeAxes a) where
	rnf	= Control.DeepSeq.rnf . (getByDay &&& getByTimeslotId)

