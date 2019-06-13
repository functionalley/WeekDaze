{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{-
	Copyright (C) 2014-2015 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Used to specify the source of the data, used to automatically generate the colour of a /lesson/.
-}

module WeekDaze.Model.GenerateLessonColourFrom(
-- * Types
-- ** Data-types
	GenerateLessonColourFrom(..),
-- * Constants
	tag
) where

import qualified	Control.DeepSeq
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema

#ifdef USE_HDBC
import qualified	Data.Convertible
import qualified	Database.HDBC

instance Data.Convertible.Convertible Database.HDBC.SqlValue GenerateLessonColourFrom {-multi-parameter type-class-} where
	safeConvert	= fmap read . Data.Convertible.safeConvert
#endif /* USE_HDBC */

-- | Used to qualify SQL & XML.
tag :: String
tag	= "generateLessonColourFrom"

-- | The source of the string to hash, when generating the colour of a /lesson/ in the timetable.
data GenerateLessonColourFrom
	= Lesson
	| Subject
	| Topic
	| Level
	| ResourceIds
	deriving (Bounded, Enum, Eq, Read, Show)

instance HXT.XmlPickler GenerateLessonColourFrom where
--	xpickle	= HXT.xpPrim
	xpickle	= HXT.xpAttr tag . HXT.xpWrap (read, show) . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show [minBound :: GenerateLessonColourFrom .. maxBound]

instance Control.DeepSeq.NFData GenerateLessonColourFrom where
	rnf _	= ()
