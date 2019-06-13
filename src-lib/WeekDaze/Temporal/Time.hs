{-# LANGUAGE CPP, FlexibleContexts #-}
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

	* The time-slot within any /day/ is identified by an abstract enumerable type, & is therefore only loosely related to time;
	i.e. in this representation, one would specify the first or perhaps the last /time-slot/, rather than booking for say exactly /9:30/.

	* The actual time can only be derived from the /time-slot/,
	using knowlege of the time at which the working-day began, & of the constant duration of each time-slot;
	but it's irrelevant to the scheduling-task.

	* Because no absolute time is associated with specific /time-slot/s within a /day/,
	/lunch-time/ merely exists invisibly between two time-slots, & plays no part in the scheduling.

	* The /day/ & the abstract identifier of the /time-slot/ within that day,
	together define the coordinates of a specific /time-slot/ in the timetable.

	* Day/Time modules may already exist in the Haskell-platform's standard library,
	but since no account of; time-zones, locales, or leap-years, is required, they're undoubtedly over-engineered for the requirement.
-}

module WeekDaze.Temporal.Time(
-- * Types
-- ** Type-synonyms
	TimeSet,
	TimesByDay,
-- ** Data-types
	Time(
--		MkTime,
		getDay,
		getTimeslotId
	),
-- * Constants
--	tag,
-- * Functions
	calculateNTimeslotsPerWeek,
	calculateDistance,
	calculateAbsoluteDistance,
	categoriseByDay,
	shift,
	shiftTime,
-- ** Constructors
#ifdef USE_HDBC
	mkTimeFromSql,
#endif
	mkTime
) where

import qualified	Control.Arrow
import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Data.Map
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import			Text.XHtml.Strict((+++))
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	WeekDaze.Size			as Size
import qualified	WeekDaze.Temporal.Day		as Temporal.Day

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	WeekDaze.Database.Selector	as Database.Selector

-- | Construct from database-values.
mkTimeFromSql
	:: Data.Convertible.Convertible Database.HDBC.SqlValue timeslotId	-- Flexible context.
	=> Database.HDBC.SqlValue	-- ^ Day.
	-> Database.HDBC.SqlValue	-- ^ timeslotId.
	-> Time timeslotId
mkTimeFromSql daySql	= mkTime (
	either (
		error . showString "WeekDaze.Temporal.Time.mkTimeFromSql:\tfailed to parse the value for " . shows Temporal.Day.tag . showString " read from the database; " . show
	) id $ Database.HDBC.safeFromSql daySql
 ) . either (
	error . showString "WeekDaze.Temporal.Time.mkTimeFromSql:\tfailed to parse the value for " . shows Database.Selector.timeslotIdColumnName . showString " read from the database; " . show
 ) id . Database.HDBC.safeFromSql
#endif /* USE_HDBC */

-- | Defines the coordinates of specific /time-slot/, within a given week.
data Time timeslotId	= MkTime {
	getDay		:: Temporal.Day.Day,	-- ^ Accessor.
	getTimeslotId	:: timeslotId		-- ^ Accessor.
} deriving (Eq, Ord)

instance Control.DeepSeq.NFData	timeslotId => Control.DeepSeq.NFData (Time timeslotId) where
	rnf	= Control.DeepSeq.rnf . (getDay &&& getTimeslotId)

-- | Constructor.
mkTime :: Temporal.Day.Day -> timeslotId -> Time timeslotId
mkTime	= MkTime

-- | Used to qualify XML.
tag :: String
tag	= "time"

instance Read timeslotId => Read (Time timeslotId) where
	readsPrec _	= map (Control.Arrow.first $ uncurry MkTime) . reads

instance Show timeslotId => Show (Time timeslotId) where
	showsPrec _	= shows . (getDay &&& getTimeslotId)	-- Hides the data-constructor & accessors.

instance Text.XHtml.Strict.HTML timeslotId => Text.XHtml.Strict.HTML (Time timeslotId) where
	toHtml time	= getDay time +++ Text.XHtml.Strict.spaceHtml +++ getTimeslotId time

instance HXT.XmlPickler timeslotId => HXT.XmlPickler (Time timeslotId) where
	xpickle	= HXT.xpElem tag $ HXT.xpWrap (
		uncurry MkTime,			-- Construct from a Pair.
		getDay &&& getTimeslotId	-- Deconstruct to a Pair.
	 ) HXT.xpickle {-Pair-}

-- | Calculates the number of /time-slot/s in a week, when provided by the number of /time-slot/s per /day/.
calculateNTimeslotsPerWeek :: Size.NTimeslots -> Size.NTimeslots
calculateNTimeslotsPerWeek	= (Temporal.Day.nDaysPerWeek *)

-- | The signed distance between /time-slots/.
calculateDistance :: Enum timeslotId => timeslotId -> timeslotId -> Size.NTimeslots
calculateDistance x y	= fromEnum x - fromEnum y

-- | The absolute distance between /time-slots/.
calculateAbsoluteDistance :: Enum timeslotId => timeslotId -> timeslotId -> Size.NTimeslots
calculateAbsoluteDistance x y	= abs $ calculateDistance x y

-- | Shift the /time-slot/ by the specified number of places.
shift :: Enum timeslotId => Size.NTimeslots -> timeslotId -> timeslotId
shift i	= toEnum . (+ i) . fromEnum

-- | Shift the /time/ by the specified number of /time-slot/s.
shiftTime :: Enum timeslotId => Size.NTimeslots -> Time timeslotId -> Time timeslotId
shiftTime i	= uncurry MkTime . (getDay &&& shift i . getTimeslotId)

-- | An arbitrary set of /Time/s.
type TimeSet timeslotId	= Data.Set.Set (Time timeslotId)

-- | Arbitrary /time/s, categorised by /day/.
type TimesByDay timeslotId	= Data.Map.Map Temporal.Day.Day (Data.Set.Set timeslotId)

{- |
	* Categorise the specified times, according to their /day/.

	* CAVEAT: some /day/s have zero meetings, & therefore don't have a key in the resulting map.
-}
categoriseByDay :: Ord timeslotId => TimeSet timeslotId -> TimesByDay timeslotId
categoriseByDay	= Data.Set.foldr (
	uncurry (Data.Map.insertWith Data.Set.union) . (getDay &&& Data.Set.singleton . getTimeslotId)
 ) Data.Map.empty

