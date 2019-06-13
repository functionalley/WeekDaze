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

 [@DESCRIPTION@]	Defines a suitable type for its polymorphic namesake.

 [@CAVEAT@]	Exporting nothing but the type-constructor forces this to be used only as an interface, & therefore to be easily replaced; though a data-constructor is also exported for testing.
-}

module WeekDaze.Identifiers.TimeslotId(
-- * Types
-- ** Data-types
	TimeslotId(),
-- * Constants
--	tag,
-- * Functions
-- ** Constructor
	mkTimeslotId
) where

import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Text.XHtml.Strict
import			Text.XHtml.Strict((<<))
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	WeekDaze.Text.XHTML		as Text.XHTML

#ifdef USE_HDBC
import qualified	Data.Convertible
import qualified	Database.HDBC

instance Data.Convertible.Convertible Database.HDBC.SqlValue TimeslotId {-multi-parameter type-class-} where
	safeConvert	= fmap mkTimeslotId . Data.Convertible.safeConvert
#endif /* USE_HDBC */

-- | Used to qualify XML & CSS.
tag :: String
tag	= "timeslotId"

-- | Define a concrete type for the identifier of a time-slot in any /day/.
newtype TimeslotId	= MkTimeslotId {
	deconstruct	:: Int
} deriving (Data.Array.IArray.Ix, Eq, Ord)

-- | Constructor.
mkTimeslotId :: Int -> TimeslotId
mkTimeslotId	= MkTimeslotId

instance Show TimeslotId where
	showsPrec _	= shows . deconstruct

instance Read TimeslotId where
	readsPrec _	= map (Control.Arrow.first mkTimeslotId) . reads

instance Enum TimeslotId where
	toEnum		= mkTimeslotId
	fromEnum	= deconstruct

instance HXT.XmlPickler TimeslotId where
	xpickle	= HXT.xpWrap (
		mkTimeslotId,	-- Construct from an Int.
		deconstruct	-- Deconstruct to an Int.
	 ) $ HXT.xpAttr tag HXT.xpInt

instance Control.DeepSeq.NFData TimeslotId where
	rnf	= Control.DeepSeq.rnf . deconstruct

instance Text.XHtml.Strict.HTML TimeslotId where
	toHtml (MkTimeslotId i)	= Text.XHTML.mkXHTMLSpan tag << i

