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

	* Defines a suitable type for its polymorphic namesake.

	* Defines a concrete type used to describe the /campus/ in which a /location/ exists.

 [@CAVEAT@]	Exporting nothing but the type-constructor forces this to be used only as an interface, & therefore to be easily replaced; though a data-constructor is also exported for testing.
-}

module WeekDaze.Identifiers.Campus(
-- * Types
-- ** Data-types
	Campus(),
-- * Constants
--	tag,
-- * Functions
-- ** Constructor
	mkCampus
) where

import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XHtml.Strict
import			Text.XHtml.Strict((<<))
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	WeekDaze.Text.XHTML		as Text.XHTML

#ifdef USE_HDBC
import qualified	Data.Convertible
import qualified	Database.HDBC

instance Data.Convertible.Convertible Database.HDBC.SqlValue Campus {-multi-parameter type-class-} where
	safeConvert	= fmap mkCampus . Data.Convertible.safeConvert
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag	= "campus"

-- | Define a concrete type for the identifier for a /campus/.
newtype Campus	= MkCampus {
	deconstruct	:: String
} deriving (Eq, Ord)

-- | Constructor.
mkCampus :: String -> Campus
mkCampus	= MkCampus

instance Data.Default.Default Campus where
	def	= mkCampus ""

instance Show Campus where
	showsPrec _	= shows . deconstruct

instance Read Campus where
	readsPrec _	= map (Control.Arrow.first mkCampus) . reads

instance HXT.XmlPickler Campus where
	xpickle	= HXT.xpWrap (
		mkCampus,	-- Construct from a String.
		deconstruct	-- Deconstruct to a String.
	 ) $ HXT.xpAttr tag HXT.xpText0 {-can be null-}

instance Control.DeepSeq.NFData Campus where
	rnf	= Control.DeepSeq.rnf . deconstruct

instance Text.XHtml.Strict.HTML Campus where
	toHtml (MkCampus "")	= Text.XHtml.Strict.noHtml
	toHtml (MkCampus s)	= Text.XHTML.mkXHTMLSpan tag << s

