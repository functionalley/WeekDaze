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

module WeekDaze.Identifiers.SynchronisationId(
-- * Types
-- ** Data-types
	SynchronisationId(),
-- * Constants
--	tag,
-- * Functions
-- ** Constructor
	mkSynchronisationId
) where

import qualified	Control.DeepSeq
import qualified	Text.XHtml.Strict
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	WeekDaze.Text.XHTML		as Text.XHTML
import			Text.XHtml.Strict((<<))

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible

instance Data.Convertible.Convertible Database.HDBC.SqlValue SynchronisationId {-multi-parameter type-class-} where
	safeConvert	= fmap mkSynchronisationId . Data.Convertible.safeConvert
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag	= "synchronisationId"

-- | Define a concrete type for the identifier of a set of /course/s, whose heterogeneous /lesson/s must be synchronised.
newtype SynchronisationId	= MkSynchronisationId {
	deconstruct	:: String	-- Could also be an Int.
} deriving (Eq, Ord)

-- | Constructor.
mkSynchronisationId :: String -> SynchronisationId
mkSynchronisationId	= MkSynchronisationId

instance Show SynchronisationId where
	showsPrec _	= shows . deconstruct

instance HXT.XmlPickler SynchronisationId where
	xpickle	= HXT.xpWrap (
		mkSynchronisationId,	-- Construct from a String.
		deconstruct		-- Deconstruct to a String.
	 ) $ HXT.xpTextAttr tag {-can't be null-}

instance Control.DeepSeq.NFData SynchronisationId where
	rnf	= Control.DeepSeq.rnf . deconstruct

instance Text.XHtml.Strict.HTML SynchronisationId where
	toHtml (MkSynchronisationId s)	= Text.XHTML.mkXHTMLDiv tag << s

