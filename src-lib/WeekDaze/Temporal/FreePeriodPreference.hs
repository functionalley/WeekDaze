{-# LANGUAGE CPP, MultiParamTypeClasses #-}
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

	* Defines a preference, for the position within in a day, of any free-periods.

	* This might allow a /teacher/ to perform non-teaching tasks at home, prior to arrival at, or after departure from, the school.
-}

module WeekDaze.Temporal.FreePeriodPreference(
-- * Types
-- ** Data-types
	FreePeriodPreference(..),
-- * Constants
	tag
) where

import qualified	Control.DeepSeq
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

#ifdef USE_HDBC
import qualified	Data.Convertible
import qualified	Database.HDBC

instance Data.Convertible.Convertible Database.HDBC.SqlValue FreePeriodPreference {-multi-parameter type-class-} where
	safeConvert	= fmap read . Data.Convertible.safeConvert
#endif /* USE_HDBC */

-- | Used to qualify SQL & XML.
tag :: String
tag	= "freePeriodPreference"

-- | The human-resource's preference for the position of unallocated /time-slot/s, within any working /day/.
data FreePeriodPreference
	= Pre		-- ^ A preference for unallocated /time-slot/s to precede the first allocated /lesson/ of the day.
	| Post		-- ^ A preference for unallocated /time-slot/s to follow the last allocated /lesson/ of the day.
	| Terminal	-- ^ A preference for unallocated /time-slot/s to either precede the first or follow the last, /lesson/ of the day.
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance HXT.XmlPickler FreePeriodPreference where
--	xpickle	= HXT.xpAttr tag HXT.xpPrim
	xpickle	= HXT.xpAttr tag . HXT.xpWrap (read, show) . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show [minBound :: FreePeriodPreference .. maxBound]	-- There can only ever be one preference, so it can reasonably be assumed to be an attribute.

instance Control.DeepSeq.NFData FreePeriodPreference where
	rnf _	= ()

