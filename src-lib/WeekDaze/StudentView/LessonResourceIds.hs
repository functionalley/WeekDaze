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

 [@DESCRIPTION@]	Defines the /resource/s required for a /lesson/, from the perspective of a /student-body/.
-}

module WeekDaze.StudentView.LessonResourceIds(
-- * Types
-- ** Data-types
	LessonResourceIds(..)
-- * Constants
--	tag
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Text.XHtml.Strict
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	WeekDaze.Model.ResourceUser	as Model.ResourceUser
import			Text.XHtml.Strict((+++), (<<))

-- | Used to qualify XML.
tag :: String
tag	= "studentViewLessonResourceIds"

-- | Composed from both the 'locationId' & 'teacherId'.
data LessonResourceIds locationId teacherId	= MkLessonResourceIds {
	getLocationId	:: locationId,	-- ^ The identifier for the /location/ at which the /class/ will be held.
	getTeacherId	:: teacherId	-- ^ The identifier for the /teacher/ instructing this /class/.
} deriving (Eq, Ord, Show)

instance (
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	teacherId
 ) => Text.XHtml.Strict.HTML (LessonResourceIds locationId teacherId) where
	toHtml	= (
		\(locationId, teacherId) -> Text.XHtml.Strict.li << (locationId +++ '.') +++ Text.XHtml.Strict.li << (teacherId +++ '.')
	 ) . (
		getLocationId &&& getTeacherId
	 )

instance (HXT.XmlPickler locationId, HXT.XmlPickler teacherId) => HXT.XmlPickler (LessonResourceIds locationId teacherId) where
	xpickle	= HXT.xpElem tag $ HXT.xpWrap (
		uncurry MkLessonResourceIds,	-- Construct from a Pair.
		getLocationId &&& getTeacherId	-- Deconstruct to a Pair.
	 ) HXT.xpickle {-Pair-}

instance (Eq locationId, Eq teacherId) => Model.ResourceUser.ResourceUser (LessonResourceIds locationId teacherId) where
	areIndependent l r	= getLocationId l /= getLocationId r && getTeacherId l /= getTeacherId r

instance (Control.DeepSeq.NFData locationId, Control.DeepSeq.NFData teacherId) => Control.DeepSeq.NFData (LessonResourceIds locationId teacherId) where
	rnf	= Control.DeepSeq.rnf . (getLocationId &&& getTeacherId)

