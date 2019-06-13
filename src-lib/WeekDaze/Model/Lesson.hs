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

 [@DESCRIPTION@]	Defines a lesson, suitable for any /time-slot/ in the generic /timetable/.
-}

module WeekDaze.Model.Lesson(
-- * Types
-- ** Type-synonyms
	GeneralisedLesson,
-- ** Data-types
	Lesson(..)
-- * Constants
--	resourceIdsCSSIdentifier,
--	tag
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Text.XHtml.Strict
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	WeekDaze.Data.Subject		as Data.Subject
import qualified	WeekDaze.Text.CSS		as Text.CSS
import			Text.XHtml.Strict((+++), (<<), (!))

-- | Used to qualify output.
resourceIdsCSSIdentifier :: Text.CSS.CSSIdentifier
resourceIdsCSSIdentifier	= "resourceIds"	-- CAVEAT: coordinate with 'Data.Meeting.resourceIdsCSSIdentifier'.

-- | Used to qualify CSS & XML.
tag :: String
tag	= "lesson"

{- |
	* Defines a potential /booking/ of the /resource/s required to teach a /subject/.

	* It doesn't specify the /time-slot/ at which it's to be booked, because that's defined by it's position within a /timetable/.

	* A /timetable/ composed from these /lesson/s, can be viewed by either a /location/, /student/ or /teacher/, so 'getResourceIds' is polymorphic.
-}
data Lesson resourceIds level	= MkLesson {
	getResourceIds	:: resourceIds,			-- ^ The concrete type depends on whether viewed from the perspective of the /location/, /student/ or /teacher/.
	getSubject	:: Data.Subject.Subject level	-- ^ The /subject/ to be taught.
} deriving (Eq, Ord, Show)

instance (Text.XHtml.Strict.HTML level, Text.XHtml.Strict.HTML resourceIds) => Text.XHtml.Strict.HTML (Lesson resourceIds level) where
	toHtml lesson	= Text.XHtml.Strict.thediv ! [
		Text.XHtml.Strict.theclass tag,
		Text.XHtml.Strict.title tag
	 ] << (
		getSubject lesson +++ '.' +++ Text.XHtml.Strict.ulist ! [
			Text.XHtml.Strict.theclass resourceIdsCSSIdentifier,
			Text.XHtml.Strict.title resourceIdsCSSIdentifier
		] << getResourceIds lesson
	 )

instance (HXT.XmlPickler level, HXT.XmlPickler resourceIds, Show level) => HXT.XmlPickler (Lesson resourceIds level) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		uncurry MkLesson,		-- Construct from a Pair.
		getResourceIds &&& getSubject	-- Deconstruct to a Pair.
	 ) $ HXT.xpickle `HXT.xpPair` HXT.xpickle

instance (Control.DeepSeq.NFData level, Control.DeepSeq.NFData resourceIds) => Control.DeepSeq.NFData (Lesson resourceIds level) where
	rnf	= Control.DeepSeq.rnf . (getResourceIds &&& getSubject)

-- | A potentially undefined /lesson/.
type GeneralisedLesson resourceIds level	= Maybe (Lesson resourceIds level)

