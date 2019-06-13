{-
	Copyright (C) 2013 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Bounds the minimum & maximum workload of an individual, measured in terms of a number of /time-slot/s.
-}

module WeekDaze.Temporal.Workload(
-- * Types
-- ** Type-synonyms
	Bounds,
-- * Constants
	unloaded,
-- * Functions
-- ** Accessors
	getMinimum,
	getMaximum,
-- ** Operators
	(<+>)
) where

import			Control.Arrow((***))
import qualified	WeekDaze.Size	as Size

infixl 6 <+>	-- Same as (+).

-- | The minimum & maximum duration, in /time-slot/s.
type Bounds	= (Size.NTimeslots, Size.NTimeslots)

-- | The origin.
unloaded :: Bounds
unloaded	= (0, 0)

-- | Accessor.
getMinimum :: Bounds -> Size.NTimeslots
getMinimum	= fst

-- | Accessor.
getMaximum :: Bounds -> Size.NTimeslots
getMaximum	= snd

-- | Add the specified /bounds/.
(<+>) :: Bounds -> Bounds -> Bounds
(<+>)	= uncurry (***) . ((+) *** (+))	-- Independently add the two minima, & the two maxima.

