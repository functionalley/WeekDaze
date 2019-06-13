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

 [@DESCRIPTION@]	Defines the possible output-formats available.
-}

module WeekDaze.OutputConfiguration.Format(
-- * Types
-- ** Data-types
	Format(..),
-- * Constants
--	xhtmlTag,
--	xmlTag,
-- * Functions
-- ** Accessors
	getMaybeStyle
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	WeekDaze.OutputConfiguration.Style	as OutputConfiguration.Style
import qualified	WeekDaze.OutputConfiguration.View	as OutputConfiguration.View

-- | Used to qualify XML.
xhtmlTag :: String
xhtmlTag	= "xhtml"

-- | Used to qualify XML.
xmlTag :: String
xmlTag		= "xml"

-- | Defines the alternative output-formats of the /timetable/.
data Format minimumContrastRatio
	= XHTML (OutputConfiguration.Style.Style minimumContrastRatio)
	| XML OutputConfiguration.View.View
	deriving (Eq, Show)

instance Data.Default.Default (Format minimumContrastRatio) where
	def	= XHTML Data.Default.def

instance (
	Fractional	minimumContrastRatio,
	HXT.XmlPickler	minimumContrastRatio,
	Ord		minimumContrastRatio
 ) => HXT.XmlPickler (Format minimumContrastRatio) where
	xpickle	= HXT.xpDefault Data.Default.def $ HXT.xpAlt (
		\format -> case format of
			XHTML _	-> 0
			XML _	-> 1
	 ) [
		HXT.xpElem xhtmlTag $ HXT.xpWrap (XHTML, \(XHTML style) -> style) HXT.xpickle {-Style-},
		HXT.xpElem xmlTag . HXT.xpWrap (XML, \(XML view) -> view) $ HXT.xpDefault Data.Default.def HXT.xpickle {-View-}
	 ]

instance Control.DeepSeq.NFData	minimumContrastRatio => Control.DeepSeq.NFData (Format minimumContrastRatio) where
	rnf (XHTML style)	= Control.DeepSeq.rnf style
	rnf (XML view)		= Control.DeepSeq.rnf view

-- | Accessor.
getMaybeStyle :: Format minimumContrastRatio -> Maybe (OutputConfiguration.Style.Style minimumContrastRatio)
getMaybeStyle (XHTML style)	= Just style
getMaybeStyle _			= Nothing

