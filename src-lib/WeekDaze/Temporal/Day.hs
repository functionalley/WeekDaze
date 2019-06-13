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

	* Defines the days of the week.

	* This is required to define the day of a /booking/ in the timetable.

	* Day/Time modules may already exist in the Haskell-platform's standard library,
	but since no account of; time-zones, locales, or leap-years, is required, they're undoubtedly over-engineered for the requirement.
-}

module WeekDaze.Temporal.Day(
-- * Types
-- ** Type-synonyms
	Weekend,
-- ** Data-types
	Day(),
-- * Constants
	tag,
	nDaysPerWeek,
	range,
-- * Functions
	getYesterday,
	getTomorrow,
	getAdjacentDays
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Data.Array.IArray
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	Text.XML.HXT.Arrow.Pickle.Schema
import qualified	WeekDaze.Size			as Size

#ifdef USE_HDBC
import qualified	Data.Convertible
import qualified	Database.HDBC

instance Data.Convertible.Convertible Database.HDBC.SqlValue Day {-multi-parameter type-class-} where
	safeConvert	= fmap read . Data.Convertible.safeConvert
#endif /* USE_HDBC */

-- | Used to qualify SQL & XML.
tag :: String
tag	= "day"

-- | Defines the days of the working-week.
data Day	= Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday	deriving (
	Data.Array.IArray.Ix,
	Bounded,
	Enum,
	Eq,
	Ord,
	Read,
	Show
 )

instance Text.XHtml.Strict.HTML Day where
	toHtml	= Text.XHtml.Strict.toHtml . show

instance HXT.XmlPickler Day where
	xpickle	= HXT.xpElem tag . HXT.xpAttr "value" . HXT.xpWrap (read, show) . HXT.xpTextDT . Text.XML.HXT.Arrow.Pickle.Schema.scEnum $ map show range

instance Control.DeepSeq.NFData Day where
	rnf _	= ()

-- | Like 'pred', but treats the enumerable type as a circular list.
getYesterday :: Day -> Day
getYesterday day
	| day == minBound	= maxBound
	| otherwise		= pred day

-- | Like 'succ', but treats the enumerable type as a circular list.
getTomorrow :: Day -> Day
getTomorrow day
	| day == maxBound	= minBound
	| otherwise		= succ day

-- | Returns yesterday & tomorrow.
getAdjacentDays :: Day -> (Day, Day)
getAdjacentDays	= getYesterday &&& getTomorrow

-- | The constant number of days in a week.
nDaysPerWeek :: Size.NDays
nDaysPerWeek	= succ {-fence-post-} $ fromEnum (maxBound :: Day) - fromEnum (minBound :: Day)

-- | The constant complete range of values.
range :: [Day]
range	= [minBound .. maxBound]

-- | Contains all days generally considered to be work-days rather than weekend.
type Weekend	= Data.Set.Set Day

