{-# LANGUAGE CPP #-}
{-
	Copyright (C) 2013-2016 Dr. Alistair Ward

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

	* Either the ideal /time-slot/ in any /day/, or the specific set of /time/s in each week, at which to book a /lesson/ of a /course/.

	* One may for example;
	prefer to teach a demanding /course/ early, when concentration is relatively high;
	avoid swimming immediately after lunch;
	book games in a consecutive sequence of time-slots, during the afternoon.

	* If there are fewer specified /time/s, than there are /lesson/s required per week for the /course/,
	then the remainder are booked at arbitrary times.
-}

module WeekDaze.Temporal.TimeslotRequest(
-- * Types
-- ** Data-types
	TimeslotRequest(..),
-- * Constants
--	tag,
--	ideallyTag,
	specificallyTag,
-- * Functions
	countSpecifiedTimes,
	countSpecifiedDays,
	getMaybeIdealTimeslotId,
	getSpecifiedTimes,
--	groupSpecifiedTimesByDay,
	findDistinctRunlengthsOfSpecifiedTimes,
-- ** Predicates
	isIdeally,
	isSpecific,
	isNull,
	isASpecifiedTime,
	isASpecifiedDay
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	ToolShed.Data.List
import qualified	ToolShed.Data.List.Runlength
import qualified	WeekDaze.Size			as Size
import qualified	WeekDaze.Temporal.Day		as Temporal.Day
import qualified	WeekDaze.Temporal.Time		as Temporal.Time

-- | Used to qualify Output & XML.
tag :: String
tag		= "timeslotRequest"

-- | Used to qualify Output & XML.
ideallyTag :: String
ideallyTag	= "ideally"

-- | Used to qualify Output & XML.
specificallyTag :: String
specificallyTag	= "specifically"

-- | Defines the /ideal time-slot/ on any /day/, or the specific /time/s in each week, at which to book a /lesson/ of a specific /course/.
data TimeslotRequest timeslotId
	= Ideally timeslotId					-- ^ Book as close to the specified /time-slot/ as possible, but on an arbitrary /day/.
	| Specifically (Temporal.Time.TimeSet timeslotId)	-- ^ Book at the specified /time/s in each week; any remaining /lesson/ required for the /course/ are booked at arbitrary /time/s.
	deriving (Eq, Ord)

instance Show timeslotId => Show (TimeslotRequest timeslotId) where
	showsPrec _ timeslotRequest	= showString (tag ++ "={") . (
		case timeslotRequest of
			Ideally timeslotId	-> showString ideallyTag . showChar '=' . shows timeslotId
			Specifically timeSet	-> showString specificallyTag . showChar '=' . shows (Data.Set.toList timeSet)
	 ) . showChar '}'

instance Data.Default.Default (TimeslotRequest timeslotId) where
	def	= Specifically Data.Set.empty

instance (HXT.XmlPickler timeslotId, Ord timeslotId) => HXT.XmlPickler (TimeslotRequest timeslotId) where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag $ HXT.xpAlt (
		\timeslotRequest -> if isIdeally timeslotRequest then 0 else 1	-- Lookup the appropriate pickler in the list below.
	 ) [
		HXT.xpElem ideallyTag $ HXT.xpWrap (
			Ideally,				-- Construct from a timeslotId.
			\(Ideally timeslotId)	-> timeslotId	-- Deconstruct to a timeslotId.
		) HXT.xpickle,
		HXT.xpElem specificallyTag . HXT.xpWrap (
			Specifically . Data.Set.fromList,	-- Construct from a list of Times.
			Data.Set.toList . getSpecifiedTimes	-- Deconstruct to a list of Times.
		) $ HXT.xpList1 {-can't be null-} HXT.xpickle {-Time-}
	 ]

instance Control.DeepSeq.NFData timeslotId => Control.DeepSeq.NFData (TimeslotRequest timeslotId) where
	rnf (Specifically timeSet)	= Control.DeepSeq.rnf timeSet
	rnf _				= ()

-- | Accessor, which returns a default value for data constructed by 'Specifically'.
getMaybeIdealTimeslotId :: TimeslotRequest timeslotId -> Maybe timeslotId
getMaybeIdealTimeslotId (Ideally timeslotId)	= Just timeslotId
getMaybeIdealTimeslotId _			= Nothing

-- | Accessor, which returns a default value for data constructed by 'Ideally'.
getSpecifiedTimes :: TimeslotRequest timeslotId -> Temporal.Time.TimeSet timeslotId
getSpecifiedTimes (Specifically timeSet)	= timeSet
getSpecifiedTimes _				= Data.Set.empty

-- | Counts the number of specified /time/s.
countSpecifiedTimes :: TimeslotRequest timeslotId -> Size.NTimeslots
countSpecifiedTimes	= Data.Set.size . getSpecifiedTimes

-- | Counts the number of discrete /day/s specified.
countSpecifiedDays ::
#if !MIN_VERSION_containers(0,5,2)
	Ord timeslotId =>	-- Not required after "Data.Set-7.8.1".
#endif
	TimeslotRequest timeslotId -> Size.NDays
countSpecifiedDays	= Data.Set.size . Data.Set.map Temporal.Time.getDay . getSpecifiedTimes

-- | True when the /timeslot-request/ was constructed by 'Ideally'.
isIdeally :: TimeslotRequest timeslotId -> Bool
isIdeally (Ideally _)	= True
isIdeally _		= False

{- |
	* True when the /timeslot-request/ was constructed by 'Specifically'

	* CAVEAT: the set of specified /time/s may still be null.
-}
isSpecific :: TimeslotRequest timeslotId -> Bool
isSpecific (Specifically _)	= True
isSpecific _			= False

{- |
	* True if there are zero specified /time/s.

	* CAVEAT: an /ideal time-slot/ may still have been defined.
-}
isNull :: TimeslotRequest timeslotId -> Bool
isNull	= Data.Set.null . getSpecifiedTimes

-- | True if the specified /time/ is one of those requested.
isASpecifiedTime :: Ord timeslotId => Temporal.Time.Time timeslotId -> TimeslotRequest timeslotId -> Bool
isASpecifiedTime time (Specifically timeSet)	= Data.Set.member time timeSet
isASpecifiedTime _ _				= False

-- | True if one of the specified /time/s falls on the specified /day/.
isASpecifiedDay :: Temporal.Day.Day -> TimeslotRequest timeslotId -> Bool
isASpecifiedDay day (Specifically timeSet)	= Data.Foldable.any ((== day) . Temporal.Time.getDay) timeSet
isASpecifiedDay _ _				= False

{- |
	* The ascending list of specified /time/s, for each /day/.

	* CAVEAT: the specific /day/ is dropped from the results,
	& the results don't include those days on which there're zero /timeslot-requests/.
-}
groupSpecifiedTimesByDay :: TimeslotRequest timeslotId -> [[Temporal.Time.Time timeslotId]]
groupSpecifiedTimesByDay	= Data.List.groupBy (ToolShed.Data.List.equalityBy Temporal.Time.getDay) . Data.Set.toAscList . getSpecifiedTimes

-- | Find the set of distinct run-lengths of consecutive specified /time/s, occuring within any single /day/.
findDistinctRunlengthsOfSpecifiedTimes :: Enum timeslotId => TimeslotRequest timeslotId -> Data.Set.Set Size.NTimeslots
findDistinctRunlengthsOfSpecifiedTimes	= Data.Set.fromList . concatMap (
	map (
		succ {-fence-post-} . ToolShed.Data.List.Runlength.getLength
	) . filter ToolShed.Data.List.Runlength.getDatum {-Bool-} . ToolShed.Data.List.Runlength.encode . map (
		== 1	-- Only true for consecutive timeslot-requests.
	) . (
		uncurry (
			zipWith (-)
		) . (
			tail &&& id
		) -- Find the difference between adjacent timeslot-requests.
	) . map (
		fromEnum . Temporal.Time.getTimeslotId	-- Required in order to find the distance between time-slots.
	)
 ) . groupSpecifiedTimesByDay

