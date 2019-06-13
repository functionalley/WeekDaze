{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{-
	Copyright (C) 2015 Dr. Alistair Ward

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

	* This value permits one to distinguish between the academic year of a /student/ & optionally also their /stream/ within the year.

 [@CAVEAT@]	Exporting nothing but the type-constructor forces this to be used only as an interface, & therefore to be easily replaced; though a data-constructor is also exported for testing.
-}

module WeekDaze.Identifiers.Stream(
-- * Types
-- ** Data-types
	Stream(),
-- * Constants
--	tag,
-- * Functions
-- ** Constructor
	mkStream
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

instance Data.Convertible.Convertible Database.HDBC.SqlValue Stream {-multi-parameter type-class-} where
	safeConvert	= fmap mkStream . Data.Convertible.safeConvert
#endif /* USE_HDBC */

-- | Used to qualify XML & CSS.
tag :: String
tag	= "stream"

-- | Define a concrete type for the stream of a /student/.
newtype Stream	= MkStream {
	deconstruct	:: String	-- Could also be an Int.
} deriving (Eq, Ord)

-- | Constructor.
mkStream :: String -> Stream
mkStream	= MkStream

instance Data.Default.Default Stream where
	def	= mkStream ""

instance Show Stream where
	showsPrec _	= shows . deconstruct

instance Read Stream where
	readsPrec _	= map (Control.Arrow.first mkStream) . reads

instance HXT.XmlPickler Stream where
	xpickle	= HXT.xpWrap (
		mkStream,	-- Construct from a String.
		deconstruct	-- Deconstruct to a String.
	 ) $ HXT.xpAttr tag HXT.xpText0 {-can be null-}

instance Control.DeepSeq.NFData Stream where
	rnf	= Control.DeepSeq.rnf . deconstruct

instance Text.XHtml.Strict.HTML Stream where
	toHtml (MkStream "")	= Text.XHtml.Strict.noHtml
	toHtml (MkStream s)	= Text.XHTML.mkXHTMLSpan tag << s

