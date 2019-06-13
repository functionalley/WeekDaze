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

 [@DESCRIPTION@]	Enumerates the various perspectives from which a /timetable/ can be viewed.
-}

module WeekDaze.OutputConfiguration.View(
-- * Types
-- ** Data-types
	View(..),
-- * Constants
	tag,
	range
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

-- | Used to qualify XML.
tag :: String
tag	= "view"

-- | The perspective from which a timetable is intended to be viewed.
data View
	= LocationView
	| StudentView
	| TeacherView
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Data.Default.Default View where
	def	= StudentView

instance HXT.XmlPickler View where
	xpickle	= HXT.xpAttr tag . HXT.xpWrap (read, show) . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

instance Control.DeepSeq.NFData View where
	rnf _	= ()

-- | The constant complete range of values.
range :: [View]
range	= [minBound .. maxBound]

