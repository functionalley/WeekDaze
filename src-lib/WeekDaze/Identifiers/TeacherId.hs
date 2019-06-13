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

module WeekDaze.Identifiers.TeacherId(
-- * Types
-- ** Data-types
	TeacherId(),
-- * Constants
--	tag,
-- * Functions
-- ** Constructor
	mkTeacherId
) where

import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Text.XHtml.Strict
import			Text.XHtml.Strict((<<))
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	WeekDaze.Text.XHTML		as Text.XHTML

#ifdef USE_HDBC
import qualified	Data.Convertible
import qualified	Database.HDBC

instance Data.Convertible.Convertible Database.HDBC.SqlValue TeacherId {-multi-parameter type-class-} where
	safeConvert	= fmap mkTeacherId . Data.Convertible.safeConvert
#endif /* USE_HDBC */

-- | Used to qualify XML & CSS.
tag :: String
tag	= "teacherId"

-- | Define a concrete type for the identifier for a /teacher/.
newtype TeacherId	= MkTeacherId {
	deconstruct	:: String	-- Could also be an Int.
} deriving (Eq, Ord)

-- | Constructor.
mkTeacherId :: String -> TeacherId
mkTeacherId	= MkTeacherId

instance Show TeacherId where
	showsPrec _	= shows . deconstruct

instance Read TeacherId where
	readsPrec _	= map (Control.Arrow.first mkTeacherId) . reads

instance HXT.XmlPickler TeacherId where
	xpickle	= HXT.xpWrap (
		MkTeacherId,	-- Construct from a String.
		deconstruct	-- Deconstruct to a String.
	 ) $ HXT.xpTextAttr tag {-can't be null-}

instance Control.DeepSeq.NFData TeacherId where
	rnf	= Control.DeepSeq.rnf . deconstruct

instance Text.XHtml.Strict.HTML TeacherId where
	toHtml (MkTeacherId s)	= Text.XHTML.mkXHTMLSpan tag << s

