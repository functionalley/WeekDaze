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

 [@DESCRIPTION@]	Defines a specific axis (& the sense in which that axis is traversed), in the three-dimensional coordinate-system used to describe a /timetable/.

-}

module WeekDaze.Model.TimetableAxisTraversal(
-- * Types
-- ** Type-synonyms
	Sense,
-- ** Data-types
	AxisTraversal(..),
-- * Constants
--	tag,
	senseTag,
-- * Functions
	maybeSenseToList,
	invertSense,
-- ** Predicates
	hasWildSense
) where

import			Control.Arrow((&&&))
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Char
import qualified	Data.Maybe
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	WeekDaze.Model.TimetableAxis	as Model.TimetableAxis
import			WeekDaze.Enhanced.EnhancedBool()

-- | Used to qualify SQL & XML.
tag :: String
tag		= "timetableAxisTraversal"

-- | Used to qualify SQL & XML.
senseTag :: String
senseTag	= "sense"

-- | Defines the direction of travel along an axis; 'True' being interpreted as positive & 'False' negative.
type Sense	= Bool

-- | Defines an axis & the direction of travel along it.
data AxisTraversal	= MkAxisTraversal {
	getMaybeSense	:: Maybe Sense,			-- ^ Optionally defines the direction of travel along the axis.
	getAxis		:: Model.TimetableAxis.Axis	-- ^ Defines which of the axes to traverse.
} deriving Eq

instance Read AxisTraversal where
	readsPrec _ []	= []
	readsPrec _ s@(c : remainder)
		| Data.Char.isSpace c	= reads remainder	-- Consume.
		| otherwise		= case c of
			'+'	-> Control.Arrow.first (MkAxisTraversal $ Just True) `map` reads remainder
			'-'	-> Control.Arrow.first (MkAxisTraversal $ Just False) `map` reads remainder
			_	-> Control.Arrow.first (MkAxisTraversal Nothing) `map` reads s	-- Wild sense.

instance Show AxisTraversal where
	showsPrec _ axisTraversal	= showString (
		Data.Maybe.maybe "" (\sense -> if sense then "+" else "-") $ getMaybeSense axisTraversal
	 ) . shows (getAxis axisTraversal)

instance HXT.XmlPickler AxisTraversal where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		uncurry MkAxisTraversal,	-- Construct from a Pair.
		getMaybeSense &&& getAxis	-- Deconstruct to a Pair.
	 ) $ HXT.xpAttrImplied senseTag HXT.xpickle {-Maybe Sense-} `HXT.xpPair` HXT.xpickle {-TimetableAxis-}

instance Control.DeepSeq.NFData AxisTraversal where
	rnf	= Control.DeepSeq.rnf . (getMaybeSense &&& getAxis)

-- | Interpret a sense-specification.
maybeSenseToList :: Maybe Sense -> [Sense]
maybeSenseToList	= Data.Maybe.maybe [minBound .. maxBound] return {-to List-monad-}

-- | Construct a traversal which goes the other way along the same axis.
invertSense :: AxisTraversal -> AxisTraversal
invertSense (MkAxisTraversal (Just sense) axis)	= MkAxisTraversal (Just $ not sense) axis
invertSense x@(MkAxisTraversal Nothing _)	= x

-- | True if the sense is ill-defined.
hasWildSense :: AxisTraversal -> Bool
hasWildSense	= Data.Maybe.isNothing . getMaybeSense

