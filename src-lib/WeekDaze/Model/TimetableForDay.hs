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

	* Define the data-structure, required to hold a single day's bookings, for any /observer/-id.

	* It doesn't reference the parameters of the specific problem, or the means by which a solution is obtained.
-}

module WeekDaze.Model.TimetableForDay(
-- * Types
-- ** Type-synonyms
	TimetableForDay,
	Association,
	Booking,
	GeneralisedLessonRunlength,
	GeneralisedLessonRunlengthByTimeslotId,
--	GeneralisedLessonRunlengths,
	LessonRunlengths,
	RunlengthsByTimeslotIdByLesson,
-- * Constants
--	tag,
--	bookingTag,
-- * Functions
	measureRunlengthAt,
	boundRunlengthAt,
	countRunlengthAt,
	countUnallocatedTimeslots,
	bisectAt,
	extractAdjacentBookings,
	locateUnallocatedTimeslots,
	findGeneralisedLessonRunlengths,
	findGeneralisedLessonRunlengthsByTimeslotId,
--	findSeparatedEqualLessonsBy,
	findSeparatedEqualLessons,
	findSeparatedEqualSubjectLessons,
	findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLesson,
	measureFreePeriodCompliance,
	spanRunlengthAt,
-- ** Accessors
	getTimeslotId,
	getMaybeLesson,
--	getBookedTimeslotId,
	getBookedLesson,
-- ** Constructor
	mkFreeTimetableForDay,
-- ** Mutators
	defineTimeslot,
-- ** Predicates
	isSubjectBooked,
	isDefined,
-- ** Translation
	xpickle
) where

import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((//), (!))
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Factory.Data.Interval
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	ToolShed.Data.List
import qualified	ToolShed.Data.List.Runlength
import qualified	ToolShed.Data.Pair
import qualified	WeekDaze.Data.Subject			as Data.Subject
import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.Size				as Size
import qualified	WeekDaze.Temporal.FreePeriodPreference	as Temporal.FreePeriodPreference
import qualified	WeekDaze.Temporal.Time			as Temporal.Time

-- | Used to qualify XML.
tag :: String
tag		= "timetableForDay"

-- | Used to qualify XML.
bookingTag :: String
bookingTag	= "booking"

{- |
	* The ordered sequence of time-slots within a single unspecified day.

	* The sequence of time-slots are contiguous;
		/double/ /lesson/s are representated by consecutive duplicate /lesson/-definitions,
		free-/period/s are represented by 'Nothing'.

	* Each time-slot has a specific unspecified start-time & probably a constant duration; but that depends on the school, so no such assumption is made here.
-}
type TimetableForDay timeslotId resourceIds level	= Data.Array.IArray.Array timeslotId (Model.Lesson.GeneralisedLesson resourceIds level)

-- | The association on which 'TimetableForDay' is based.
type Association timeslotId resourceIds level	= (timeslotId, Model.Lesson.GeneralisedLesson resourceIds level)

-- | Accessor.
getTimeslotId :: Association timeslotId resourceIds level -> timeslotId
getTimeslotId	= fst

-- | Accessor.
getMaybeLesson :: Association timeslotId resourceIds level -> Model.Lesson.GeneralisedLesson resourceIds level
getMaybeLesson	= snd

-- | True if the /time-slot/ has been booked.
isDefined :: Association timeslotId resourceIds level -> Bool
isDefined	= Data.Maybe.isJust . getMaybeLesson

-- | Defines a pickler to convert the specified /timetable/ to, or from, XML.
xpickle :: (
	Data.Array.IArray.Ix	timeslotId,
	HXT.XmlPickler		level,
	HXT.XmlPickler		resourceIds,
	HXT.XmlPickler		timeslotId,
	Show			level
 ) => HXT.PU (TimetableForDay timeslotId resourceIds level)
xpickle	= HXT.xpElem tag . HXT.xpWrap (
	uncurry Data.Array.IArray.array . (
		(minimum &&& maximum) . map fst &&& id
	),				-- Construct from an association-list; since all time-slots are recorded in the XML, even when no lesson is booked, the timeslotId-bounds can be derived.
	Data.Array.IArray.assocs	-- Deconstruct to an association-list.
 ) . HXT.xpList1 {-can't be null-} $ HXT.xpElem bookingTag HXT.xpickle {-Pair-}

-- | A /lesson/ qualified by the /time-slot/ at which it is booked.
type Booking timeslotId resourceIds level	= (timeslotId, Model.Lesson.Lesson resourceIds level)

-- | Accessor.
getBookedTimeslotId :: Booking timeslotId resourceIds level -> timeslotId
getBookedTimeslotId	= fst

-- | Accessor.
getBookedLesson :: Booking timeslotId resourceIds level -> Model.Lesson.Lesson resourceIds level
getBookedLesson	= snd

-- | Constructor. Create an unallocated /timetable/, for any single day.
mkFreeTimetableForDay :: (Data.Array.IArray.Ix timeslotId, Enum timeslotId) => Factory.Data.Interval.Interval timeslotId -> TimetableForDay timeslotId resourceIds level
mkFreeTimetableForDay timeslotIdBounds	= Data.Array.IArray.array timeslotIdBounds . zip (Factory.Data.Interval.toList timeslotIdBounds) $ repeat Nothing

{- |
	* Bisect the association-list extracted from the 'TimetableForDay', at the specified /timeslotId/, to form two lists, those before & those after the specified /time-slot/.

	* The list of earlier /time-slot/s is reversed, as though looking backwards from the specified /time-slot/.

	* CAVEAT: the association at the specified /timeslotId/ isn't returned in either half.
-}
bisectAt
	:: Data.Array.IArray.Ix timeslotId
	=> timeslotId
	-> TimetableForDay timeslotId resourceIds level
	-> ([Association timeslotId resourceIds level], [Association timeslotId resourceIds level])
bisectAt timeslotId	= (
	dropWhile (
		(>= timeslotId) . getTimeslotId	-- Extract earlier timeslots.
	) . reverse &&& dropWhile (
		(<= timeslotId) . getTimeslotId	-- Extract later timeslots.
	)
 ) . Data.Array.IArray.assocs

{- |
	* Get any /lesson/s immediately adjacent to the specified /time-slot/.

	* The relevance is that adjacent /lesson/s can typically be replicated, to form a longer duration /lesson/,
	but duplication of a non-adjacent one, would leave an awkward gap.
-}
extractAdjacentBookings
	:: Data.Array.IArray.Ix timeslotId
	=> timeslotId
	-> TimetableForDay timeslotId resourceIds level
	-> [Booking timeslotId resourceIds level]
extractAdjacentBookings timeslotId	= uncurry (++) . ToolShed.Data.Pair.mirror (
	take 1 {-either a null or a singleton list; cf. 'head'-} . map (Control.Arrow.second Data.Maybe.fromJust) . takeWhile isDefined
 ) . bisectAt timeslotId

{- |
	* Defines the (lower & upper) bounds, & measures the span-length, of adjacent /lesson/s, equal to that referenced.

	* CAVEAT: if an undefined /time-slot/ is referenced, then the run-length of undefined /time-slot/s will be measured.
-}
measureRunlengthAt :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			resourceIds,
	Eq			level
 )
	=> timeslotId	-- ^ The identifier of the /time-slot/ at which to measure the run-length of equal /lesson/s.
	-> TimetableForDay timeslotId resourceIds level
	-> (Factory.Data.Interval.Interval timeslotId, Size.NTimeslots)
measureRunlengthAt timeslotId timetableForDay	= (
	(
		toEnum . (i -) *** toEnum . (i +)	-- Identify the minimum & maximum bounds of the runlength.
	) &&& succ {-include the specified timeslotId-} . uncurry (+)
 ) . ToolShed.Data.Pair.mirror (
	length . takeWhile ((== timetableForDay ! timeslotId) . getMaybeLesson)
 ) $ bisectAt timeslotId timetableForDay where
	i	= fromEnum timeslotId

-- | Defines the /timeslotId/-interval, delimiting the span of adjacent /lesson/s, equal to that referenced.
boundRunlengthAt :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			resourceIds,
	Eq			level
 )
	=> timeslotId	-- ^ The identifier of the /time-slot/ at which to measure the run-length of equal /lesson/s.
	-> TimetableForDay timeslotId resourceIds level
	-> Factory.Data.Interval.Interval timeslotId
boundRunlengthAt timeslotId	= fst . measureRunlengthAt timeslotId

-- | Returns the list of /timeslotId/s where /lesson/s equal to that referenced, are booked.
spanRunlengthAt :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			resourceIds,
	Eq			level
 )
	=> timeslotId	-- ^ The identifier of the /time-slot/ at which to measure the run-length of equal /lesson/s.
	-> TimetableForDay timeslotId resourceIds level
	-> [timeslotId]
spanRunlengthAt timeslotId	= uncurry enumFromTo . boundRunlengthAt timeslotId

-- | Measures the length of the span of adjacent /lesson/s, equal to that referenced.
countRunlengthAt :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			resourceIds,
	Eq			level
 )
	=> timeslotId	-- ^ The identifier of the /time-slot/ at which to measure the run-length of equal /lesson/s.
	-> TimetableForDay timeslotId resourceIds level
	-> Size.NTimeslots
countRunlengthAt timeslotId	= snd . measureRunlengthAt timeslotId

-- | True if the specified /subject/ has already been booked at any /time-slot/ in today's /timetable/.
isSubjectBooked :: (
#if !MIN_VERSION_array(0,5,2)
	Data.Array.IArray.Ix	timeslotId,
#endif /* CAVEAT: constraint unnecessary from "Data.Array.IArray-0.5.1.1" */
	Eq			level
 ) => Data.Subject.Subject level -> TimetableForDay timeslotId resourceIds level -> Bool
isSubjectBooked subject	= Data.Foldable.any . Data.Maybe.maybe False {-no lesson booked-} $ (== subject) . Model.Lesson.getSubject

{- |
	* Identifies unallocated /time-slot/s.

	* CAVEAT: doesn't account for the /availability/ of the /resource/, or whether any /time-slot/s has been reserved for /meeting/s.
-}
locateUnallocatedTimeslots :: Data.Array.IArray.Ix timeslotId => TimetableForDay timeslotId resourceIds level -> [timeslotId]
locateUnallocatedTimeslots timetableForDay	= [
	unallocatedTimeslotId |
		(unallocatedTimeslotId, Nothing {-i.e. unallocated-})	<- Data.Array.IArray.assocs timetableForDay
 ] -- List-comprehension.

{- |
	* Counts the /time-slot/s which haven't been yet booked.

	* CAVEAT: doesn't account for the /availability/ of the /resource/, or whether any /time-slot/s has been reserved for /meeting/s.
-}
countUnallocatedTimeslots :: Data.Array.IArray.Ix timeslotId => TimetableForDay timeslotId resourceIds level -> Size.NTimeslots
countUnallocatedTimeslots	= length . locateUnallocatedTimeslots

-- | Define a single /lesson/ in the /timetable/.
defineTimeslot
	:: Data.Array.IArray.Ix timeslotId
	=> Association timeslotId resourceIds level
	-> TimetableForDay timeslotId resourceIds level
	-> TimetableForDay timeslotId resourceIds level
defineTimeslot booking	= (// [booking])

-- | A runlength-encoded list of generalised (i.e. potentially undefined) /lesson/.
type GeneralisedLessonRunlength resourceIds level	= ToolShed.Data.List.Runlength.Code (Model.Lesson.GeneralisedLesson resourceIds level)

-- | A runlength-encoded list of /time-slot/s.
type GeneralisedLessonRunlengths resourceIds level	= [GeneralisedLessonRunlength resourceIds level]

{- |
	* Measures run-lengths of consecutive equal generalised (i.e. potentially undefined) /lesson/s.

	* Includes consecutive unallocated /time-slot/s & trivial sequences of length one.

	* CAVEAT: the /coordinates/ of each runlength aren't explicit; see 'findGeneralisedLessonRunlengthsByTimeslotId'.
-}
findGeneralisedLessonRunlengths :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			resourceIds,
	Eq			level
 ) => TimetableForDay timeslotId resourceIds level -> GeneralisedLessonRunlengths resourceIds level
findGeneralisedLessonRunlengths	= ToolShed.Data.List.Runlength.encode . Data.Array.IArray.elems

-- | An association-list keyed by the starting /timeslotId/, of runlength-encoded generalised (i.e. potentially undefined) /lesson/s.
type GeneralisedLessonRunlengthByTimeslotId timeslotId resourceIds level	= [(timeslotId, GeneralisedLessonRunlength resourceIds level)]

-- | Calls 'findGeneralisedLessonRunlengths' & derives the /coordinates/ of the start of each runlength.
findGeneralisedLessonRunlengthsByTimeslotId :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			resourceIds,
	Eq			level
 ) => TimetableForDay timeslotId resourceIds level -> GeneralisedLessonRunlengthByTimeslotId timeslotId resourceIds level
findGeneralisedLessonRunlengthsByTimeslotId timetableForDay	= init {-drop the initial value-} . scanr (
	\runlengthCode (timeslotId, _)	-> (`Temporal.Time.shift` timeslotId) . negate . ToolShed.Data.List.Runlength.getLength	{-derive the starting timeslotId-} &&& id $ runlengthCode
 ) (
	succ . snd {-max-} $ Data.Array.IArray.bounds timetableForDay,	-- The non-existent timeslotId immediately after the last for the day.
	undefined							-- The non-existent generalised lesson at the above time-slot.
 ) $ findGeneralisedLessonRunlengths timetableForDay

-- | A runlength-encoded list of /lesson/s.
type LessonRunlengths resourceIds level	= [ToolShed.Data.List.Runlength.Code (Model.Lesson.Lesson resourceIds level)]

{- |
	* Finds separated equal (according to the attribute returned by the specified accessor) /lesson/s; separated unallocated /time-slot/s don't qualify.

	* CAVEAT: discards the /coordinates/ at which each /lesson/ was booked.
-}
findSeparatedEqualLessonsBy :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			resourceIds,
	Eq			level,
	Ord			attribute
 )
	=> (Model.Lesson.Lesson resourceIds level -> attribute)	-- ^ Accessor.
	-> TimetableForDay timeslotId resourceIds level
	-> LessonRunlengths resourceIds level
findSeparatedEqualLessonsBy accessor	= filter (
	(/= 1) . ToolShed.Data.List.Runlength.getLength	-- Select non-unique sessions.
 ) . map (
	length &&& head	-- Construct a 'ToolShed.Data.List.Runlength.Code'.
 ) . Data.List.Extra.groupSortOn accessor {-separated sessions-} . Data.Maybe.mapMaybe head {-remove consecutive equal lessons, discounting undefined time-slots-} . Data.List.group {-consecutive equal generalised lessons-} . Data.Array.IArray.elems

{- |
	* Finds separated equal /lesson/s; separated unallocated /time-slot/s don't qualify.

	* CAVEAT: discards the /coordinates/ at which each /lesson/ was booked.
-}
findSeparatedEqualLessons :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			resourceIds,
	Ord			level
 ) => TimetableForDay timeslotId resourceIds level -> LessonRunlengths resourceIds level
findSeparatedEqualLessons	= findSeparatedEqualLessonsBy id

{- |
	* Finds separated /lesson/s of equal /subject/.

	* CAVEAT: discards the /coordinates/ at which each /lesson/ was booked.
-}
findSeparatedEqualSubjectLessons :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			resourceIds,
	Ord			level
 ) => TimetableForDay timeslotId resourceIds level -> LessonRunlengths resourceIds level
findSeparatedEqualSubjectLessons	= findSeparatedEqualLessonsBy Model.Lesson.getSubject

-- | A list associated by /lesson/, of lists associated by /timeslotId/.
type RunlengthsByTimeslotIdByLesson resourceIds level timeslotId	= [(Model.Lesson.Lesson resourceIds level, [(timeslotId {-start-}, Size.NTimeslots {-runlength-})])]

{- |
	* Finds runlengths of separated equal /lesson/s; separated unallocated /time-slot/s don't qualify.

	* Returns a list associated by the common /lesson/, of lists associated by starting /timeslotId/, of runlengths.
-}
findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLesson :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			resourceIds,
	Ord			level
 ) => TimetableForDay timeslotId resourceIds level -> RunlengthsByTimeslotIdByLesson resourceIds level timeslotId
findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLesson timetableForDay	= map (
	\l@(
		(
			_,	-- startingTimeslotId.
			(
				_,	-- Run-length.
				lesson
			) -- Pair.
		) : _
	) -> (
		lesson,
		map (
			Control.Arrow.second fst {-runlength-}	-- Drop the common lesson, which has been factored-out.
		) l
	) -- Pair.
 ) . filter (
	(/= 1) . length	-- Select non-unique sessions.
 ) $ Data.List.Extra.groupSortOn (
	snd {-lesson-} . snd {-runlength-code-}	-- Separated equals lessons.
 ) [
	(startingTimeslotId, (length l, lesson)) |
		l@((startingTimeslotId, Just lesson) : _)	<- Data.List.groupBy (
			ToolShed.Data.List.equalityBy snd {-generalised lesson-}	-- Consecutive equal lessons.
		) $ Data.Array.IArray.assocs timetableForDay
 ] -- List-comprehension.

{- |
	Measure the ratio of those free (neither reserved for a /meeting/ nor booked with a /lesson/) /time-slot/s
	which comply with the specified preference,
	to the total number of free /time-slot/s.
-}
measureFreePeriodCompliance
	:: (Data.Array.IArray.Ix timeslotId, Fractional ratio)
	=> Temporal.FreePeriodPreference.FreePeriodPreference
	-> Data.Set.Set timeslotId	-- ^ The /timeslotId/s of meetings, for the groups of which this observer is a member.
	-> TimetableForDay timeslotId resourceIds level
	-> ratio
measureFreePeriodCompliance freePeriodPreference meetingTimeslotIds timetableForDay
	| nFreePeriods `elem` [
		0,
		Data.Array.IArray.rangeSize $ Data.Array.IArray.bounds timetableForDay
	]		= 1	-- CAVEAT: debatable.
	| otherwise	= fromIntegral nCompliantFreePeriods / fromIntegral nFreePeriods
	where
		nCompliantFreePeriods, nFreePeriods :: Size.NTimeslots
		(nCompliantFreePeriods, nFreePeriods)	= (
			countConformantFreePeriods freePeriodPreference &&& length . filter id
		 ) . map (
			uncurry (&&) . ((`Data.Set.notMember` meetingTimeslotIds) *** Data.Maybe.isNothing)	-- Check it's neither reserved for a meeting nor booked with a lesson.
		 ) $ Data.Array.IArray.assocs timetableForDay

		countConformantFreePeriods :: Temporal.FreePeriodPreference.FreePeriodPreference -> [Bool] -> Size.NTimeslots
		countConformantFreePeriods freePeriodPreference'	= case freePeriodPreference' of
			Temporal.FreePeriodPreference.Pre	-> length . takeWhile id
			Temporal.FreePeriodPreference.Post	-> countConformantFreePeriods Temporal.FreePeriodPreference.Pre . reverse
			Temporal.FreePeriodPreference.Terminal	-> uncurry (+) . (countConformantFreePeriods Temporal.FreePeriodPreference.Pre &&& countConformantFreePeriods Temporal.FreePeriodPreference.Post)

