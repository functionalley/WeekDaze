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

 [@DESCRIPTION@]	Names the axes of the conceptually /3-D/ data-structure of the /timetable/.
-}

module WeekDaze.Model.TimetableAxis(
-- * Types
-- ** Data-types
	Axis(..),
-- * Constants
	tag,
	range,
-- * Functions
	getOthers,
	getPerpendicular
) where

import qualified	Control.DeepSeq
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "axis"

-- | Defines the three orthogonal axes of a /timetable/.
data Axis
	= ObserverId	-- ^ This axis spans the coordinates defined by the /observerId/, which is one of /student-body/, /location-id/, or /teacher-id/.
	| Day		-- ^ This axis spans the /day/s of the week.
	| TimeslotId	-- ^ This axis spans the identifier of a /time-slot/, within a /day/.
	deriving (Bounded, Enum, Eq, Read, Show)

instance HXT.XmlPickler Axis where
	xpickle	= HXT.xpAttr tag . HXT.xpWrap (read, show) . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

instance Control.DeepSeq.NFData Axis where
	rnf _	= ()

-- | The constant complete list of all values.
range :: [Axis]
range	= [minBound .. maxBound]

-- | Returns those axes other than that specified.
getOthers :: Axis -> [Axis]
getOthers ObserverId	= [Day, TimeslotId]
getOthers Day		= [ObserverId, TimeslotId]
getOthers TimeslotId	= [ObserverId, Day]

-- | Returns the axis perpendicular to the orthogonal pair specified.
getPerpendicular :: Axis -> Axis -> Axis
getPerpendicular ObserverId Day		= TimeslotId
getPerpendicular ObserverId TimeslotId	= Day
getPerpendicular Day ObserverId		= TimeslotId
getPerpendicular Day TimeslotId		= ObserverId
getPerpendicular TimeslotId ObserverId	= Day
getPerpendicular TimeslotId Day		= ObserverId
getPerpendicular x y			= error $ "WeekDaze.Model.TimetableAxis.getPerpendicular:\tspecified axes must be othogonal; " ++ show (x, y) ++ "."

