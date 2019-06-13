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

	* Describes a 'Data.Map.Map' of 'Model.TimetableForWeek'.

	* This is the shape of a generic /timetable/, from which specific instances;
	indexed by either /student-bodies/, /teacher/-ids or /location/-ids; can be built.

	* It doesn't reference the parameters of the specific problem, or the means by which a solution is obtained.
-}

module WeekDaze.Model.Timetable(
-- * Types
-- ** Type-synonyms
	Timetable,
	AugmentMarkup,
	GenericTimetableToMarkup,
	Booking,
	Wrapper(
		MkWrapper,
		deconstruct
	),
-- * Constants
	tag,
--	observerIdTag,
--	observerIdToTimetableAssociationTag,
-- * Functions
	calculateAverageAbsoluteDeviationOfFreeLessonsPerDay,
	calculateUtilisationRatioByObserverId,
--	locateUnallocatedAvailableCoordinates,
	locateUnallocatedAvailableUnreservedCoordinates,
	findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId,
	findGeneralisedLessonRunlengthsByCoordinates,
	findSeparatedEqualLessonsWithinAnyDayByObserverId,
	findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId,
	countLessons,
	countUnallocatedAvailableTimeslots,
	extractLessons,
	extractCoordinates,
	extractDistinctLessons,
	extractSynchronousLessonsAt,
	calculateMeanFreePeriodCompliance,
-- ** Constructor
	mkFreeTimetable,
-- ** Accessors
	getMaybeLesson,
	getBookedCoordinates,
	getBookedLesson,
-- ** Mutators
	defineTimeslot,
	undefineTimeslot,
	undefineTimeslots,
	undefineTimeslotsFor,
	purge,
-- ** Predicates
	isDefinedTimeslot,
	areMergeableWith,
	hasMatchingLessonAt,
	isBookedWith,
-- ** Translation
	toGeneralisedBooking
) where

import			Control.Arrow((&&&))
import			Data.Map((!))
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Factory.Data.Interval
import qualified	Factory.Math.Statistics
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	WeekDaze.Data.HumanResource		as Data.HumanResource
import qualified	WeekDaze.Data.Resource			as Data.Resource
import qualified	WeekDaze.Model.GeneralisedBooking	as Model.GeneralisedBooking
import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.Model.ResourceUser		as Model.ResourceUser
import qualified	WeekDaze.Model.TimetableCoordinates	as Model.TimetableCoordinates
import qualified	WeekDaze.Model.TimetableForDay		as Model.TimetableForDay
import qualified	WeekDaze.Model.TimetableForWeek		as Model.TimetableForWeek
import qualified	WeekDaze.Size				as Size
import qualified	WeekDaze.Temporal.Time			as Temporal.Time

-- | Used to qualify XML.
tag :: String
tag					= "timetable"

-- | Used to qualify XML.
observerIdTag :: String
observerIdTag				= "observerId"

-- | Used to qualify XML.
observerIdToTimetableAssociationTag :: String
observerIdToTimetableAssociationTag	= "observerIdToTimetableAssociation"

{- |
	* A map of weekly timetables, indexed by the /observer-Id/ for whom it is intended.

	* One can visualise this as a /3-D/ structure, indexed by /observer-Id/, /day/, & /timeslot-Id/, optionally defining a /lesson/ at each coordinate.

	* The type of the /observer-Id/ & of the /resource-Id/s referenced in each /lesson/,
	depend on the perspective from which the polymorphic /timetable/ was designed to be viewed:
	if viewed by a /student/, then the /observer-Id/ is a /student-body/, & the /resource-Ids/ are /location-Id/ & /teacher-Id/;
	if viewed by a /teacher/, then the /observer-Id/ is a /teacher-Id/, & the /resource-Ids/ are /location-Id/ & /student-body/;
	if viewed by a /location/, then the /observer-Id/ is a /location-Id/, & the /resource-Ids/ are /student-body/ & /teacher-Id/.
-}
type Timetable observerId timeslotId resourceIds level	= Data.Map.Map observerId (Model.TimetableForWeek.TimetableForWeek timeslotId resourceIds level)

-- | Whether to augment the marked-up output with supplementary information.
type AugmentMarkup	= Bool

-- | For use when converting any /timetable/-view to XHTML.
type GenericTimetableToMarkup locationId minimumContrastRatio teacherId timeslotId timetable	= AugmentMarkup -> Model.TimetableForWeek.GenericTimetableToMarkup' locationId minimumContrastRatio teacherId timeslotId timetable

{- |
	* Constructor.

	* Creates an unallocated weekly /timetable/,
	for each of the specified list of generic /observer-Id/s; i.e. /location/s, /student-bodies/ or /teacher-ids/.

	* Each daily /timetable/ has the specified list of /timeslots-Id/s, into which a /lesson/ can be booked.
-}
mkFreeTimetable :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			observerId
 )
	=> [observerId]
	-> Factory.Data.Interval.Interval timeslotId
	-> Timetable observerId timeslotId resourceIds level
mkFreeTimetable observerIds	= Data.Map.fromList . zip observerIds . repeat . Model.TimetableForWeek.mkFreeTimetableForWeek

-- | Get any /lesson/ booked at the specified /coordinates/.
getMaybeLesson
	:: (Data.Array.IArray.Ix timeslotId, Ord observerId)
	=> Model.TimetableCoordinates.Coordinates observerId timeslotId
	-> Timetable observerId timeslotId resourceIds level
	-> Model.Lesson.GeneralisedLesson resourceIds level
getMaybeLesson (observerId, time)	= Model.TimetableForWeek.getMaybeLesson time . (! observerId)

-- | True if a /booking/ has been made at the specified /coordinates/ in the /timetable/.
isDefinedTimeslot
	:: (Data.Array.IArray.Ix timeslotId, Ord observerId)
	=> Model.TimetableCoordinates.Coordinates observerId timeslotId
	-> Timetable observerId timeslotId resourceIds level
	-> Bool
isDefinedTimeslot coordinates	= Data.Maybe.isJust . getMaybeLesson coordinates

{- |
	* Extracts a /core/ of synchronous events, by drilling through the /timetable/ for all /observer-Id/s, at the specified /time/.

	* The /observer-id/s are discarded, rendering the results anonymous.

	* CAVEAT: performance-hotspot.
-}
extractSynchronousLessonsAt
	:: Data.Array.IArray.Ix timeslotId
	=> Temporal.Time.Time timeslotId
	-> Timetable observerId timeslotId resourceIds level
	-> [Model.Lesson.Lesson resourceIds level]
-- extractSynchronousLessonsAt time	= Data.Maybe.mapMaybe (Model.TimetableForWeek.getMaybeLesson time) . Data.Map.elems	-- CAVEAT: too slow.
-- extractSynchronousLessonsAt time	= Data.Foldable.concatMap (Data.Maybe.maybeToList . Model.TimetableForWeek.getMaybeLesson time)	-- CAVEAT: too slow.
extractSynchronousLessonsAt time	= Data.Map.foldr (Data.Maybe.maybe id (:) . Model.TimetableForWeek.getMaybeLesson time) [] {-initial value-}

-- | Extracts the /lesson/s from the specified /timetable/, by discarding the /coordinates/ at which each was booked.
extractLessons :: Data.Array.IArray.Ix timeslotId => Timetable observerId timeslotId resourceIds level -> [Model.Lesson.Lesson resourceIds level]
extractLessons	= Data.Foldable.concatMap Model.TimetableForWeek.extractLessons	-- Implementation by list-comprehension is no faster.

-- | Extract the list of all /coordinates/ in the specified /timetable/.
extractCoordinates :: Data.Array.IArray.Ix timeslotId => Timetable observerId timeslotId resourceIds level -> [Model.TimetableCoordinates.Coordinates observerId timeslotId]
extractCoordinates timetable	= [
	(observerId, time) |
		(observerId, timetableForWeek)	<- Data.Map.toList timetable,
		time				<- Model.TimetableForWeek.extractTimes timetableForWeek	-- In practice, these are consistent across all observer-Ids.
 ] -- List-comprehension.

-- | Extracts the set of distinct /lesson/s from the specified /timetable/.
extractDistinctLessons :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			resourceIds,
	Ord			level
 ) => Timetable observerId timeslotId resourceIds level -> Data.Set.Set (Model.Lesson.Lesson resourceIds level)
extractDistinctLessons	= Data.Set.fromList . extractLessons

-- | Overwrites any /lesson/ defined at the specified /coordinates/.
defineTimeslot
	:: (Data.Array.IArray.Ix timeslotId, Ord observerId)
	=> Model.GeneralisedBooking.GeneralisedBooking observerId timeslotId resourceIds level
	-> Timetable observerId timeslotId resourceIds level
	-> Timetable observerId timeslotId resourceIds level
defineTimeslot ((observerId, time), maybeLesson) timetable	= Data.Map.insert observerId (Model.TimetableForWeek.defineTimeslot (time, maybeLesson) $ timetable ! observerId) timetable

-- | Undefine the /lesson/, at the specified /coordinates/.
undefineTimeslot
	:: (Data.Array.IArray.Ix timeslotId, Ord observerId)
	=> Model.TimetableCoordinates.Coordinates observerId timeslotId
	-> Timetable observerId timeslotId resourceIds level
	-> Timetable observerId timeslotId resourceIds level
undefineTimeslot coordinates	= defineTimeslot (coordinates, Nothing)

{- |
	* Undefines each /lesson/, at the specified list of /coordinates/.

	* CAVEAT: if all the /coordinates/ have the same /observerId/, then 'undefineTimeslotsFor' should be more efficient.
-}
undefineTimeslots :: (
	Data.Array.IArray.Ix		timeslotId,
	Data.Foldable.Foldable		foldable,
	Ord				observerId
 )
	=> Timetable observerId timeslotId resourceIds level
	-> foldable (Model.TimetableCoordinates.Coordinates observerId timeslotId)	-- ^ The /coordinates/ to undefine.
	-> Timetable observerId timeslotId resourceIds level
undefineTimeslots	= Data.Foldable.foldr $ defineTimeslot . flip (,) Nothing

-- | Undefines each /lesson/, at the specified list of /time/s, within the /timetableForWeek/ for the specified /observerId/.
undefineTimeslotsFor :: (
	Data.Array.IArray.Ix	timeslotId,
	Data.Foldable.Foldable	foldable,
	Ord			observerId
 )
	=> observerId					-- ^ Identifies the single observer, in whose /timetableForWeek/ all unwanted /lesson/s exist.
	-> Timetable observerId timeslotId resourceIds level
	-> foldable (Temporal.Time.Time timeslotId)	-- ^ The /time/s of /lesson/s to undefine.
	-> Timetable observerId timeslotId resourceIds level
undefineTimeslotsFor observerId timetable	= ($ timetable) . Data.Map.insert observerId {-overwrite original-} . Model.TimetableForWeek.undefineTimeslots (timetable ! observerId)

-- | Undefine the whole /timetable/.
purge :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			observerId
 ) => Timetable observerId timeslotId resourceIds level -> Timetable observerId timeslotId resourceIds level
purge timetable
	| Data.Map.null timetable	= timetable
	| otherwise			= mkFreeTimetable (
		Data.Map.keys timetable	-- The observerIds.
	) . Data.Array.IArray.bounds {-timeslotIds-} . (
		Data.Array.IArray.! minBound	-- Arbitrarily use the first timetableForDay.
	) . snd {-timetableForWeek-} $ Data.Map.findMin timetable

-- | Finds runlengths of consecutive equal generalised (i.e. potentially undefined) /lesson/s, & the run-length of each sequence, indexed by /observerId/, /day/ & /timeslotId/.
findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			resourceIds,
	Eq			level
 ) => Timetable observerId timeslotId resourceIds level -> Data.Map.Map observerId (Model.TimetableForWeek.GeneralisedLessonRunlengthByTimeslotIdByDay timeslotId resourceIds level)
findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId	= Data.Map.map Model.TimetableForWeek.findGeneralisedLessonRunlengthsByTimeslotIdByDay

-- | Calls 'findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId', & flattens the results.
findGeneralisedLessonRunlengthsByCoordinates :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			resourceIds,
	Eq			level
 ) => Timetable observerId timeslotId resourceIds level -> [(Model.TimetableCoordinates.Coordinates observerId timeslotId, Model.TimetableForDay.GeneralisedLessonRunlength resourceIds level)]
findGeneralisedLessonRunlengthsByCoordinates timetable	= [
	((observerId, Temporal.Time.mkTime day startingTimeslotId), runlengthCode) |
		(observerId, generalisedLessonRunlengthsByTimeslotIdByDay)	<- Data.Map.toList $ findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId timetable,
		(day, generalisedLessonRunlengthsByTimeslotId)			<- Data.Array.IArray.assocs generalisedLessonRunlengthsByTimeslotIdByDay,
		(startingTimeslotId, runlengthCode)				<- generalisedLessonRunlengthsByTimeslotId
 ] -- List-comprehension.

-- | Finds runlengths of separated equal /lesson/s, within the /timetable/ for any single /day/.
findSeparatedEqualLessonsWithinAnyDayByObserverId :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			resourceIds
 ) => Timetable observerId timeslotId resourceIds level -> Data.Map.Map observerId (Model.TimetableForDay.LessonRunlengths resourceIds level)
findSeparatedEqualLessonsWithinAnyDayByObserverId	= Data.Map.map Model.TimetableForWeek.findSeparatedEqualLessonsWithinAnyDay

{- |
	* Finds runlengths of separated equal /lesson/s, within the /timetable/ for any single /day/; separated unallocated /time-slot/s don't count.

	* Returns a map indexed by /observerId/, of arrays indexed by /day/, of lists associated by common /lesson/, of lists associated by starting /timeslotId/, of runlengths.
-}
findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			resourceIds
 ) => Timetable observerId timeslotId resourceIds level -> Data.Map.Map observerId (Model.TimetableForWeek.RunlengthsByTimeslotIdByLessonByDay resourceIds level timeslotId)
findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId	= Data.Map.map Model.TimetableForWeek.findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDay

-- | Counts the number of /lesson/s booked.
countLessons :: Data.Array.IArray.Ix timeslotId => Timetable observerId timeslotId resourceIds level -> Size.NTimeslots
countLessons	= length . extractLessons

{- |
	* Locates unallocated /coordinates/, omitting any /day/s on which the /observerId/ is un/available/.

	* CAVEAT: doesn't account for /coordinates/ reserved for /meeting/s.
-}
locateUnallocatedAvailableCoordinates :: (
	Data.Array.IArray.Ix	timeslotId,
	Data.Resource.Resource	resource,
	Ord			observerId
 )
	=> Data.Resource.ResourceMap observerId resource	-- ^ The /resource/s, from which one can determine /availability/.
	-> Timetable observerId timeslotId resourceIds level
	-> Model.TimetableCoordinates.Vector observerId timeslotId
locateUnallocatedAvailableCoordinates resourceMap timetable	= [
	(observerId, unallocatedAvailableTime) |
		(observerId, timetableForWeek)	<- Data.Map.toList timetable,
		unallocatedAvailableTime	<- Model.TimetableForWeek.locateUnallocatedAvailableTimes (resourceMap ! observerId) timetableForWeek
 ] -- List-comprehension.

{- |
	Locates unallocated /coordinates/ in the specified /timetable/,
	omitting any /day/s on which the /observerId/ is un/available/, & any /coordinates/ reserved by the /observerId/ for a /meeting/.
-}
locateUnallocatedAvailableUnreservedCoordinates :: (
	Data.Array.IArray.Ix	timeslotId,
	Data.Resource.Resource	resource,
	Ord			observerId
 )
	=> Data.Map.Map (Temporal.Time.Time timeslotId) (Data.Set.Set observerId)	-- ^ Those /observerId/s booked for a /meeting/, indexed by /time/.
	-> Data.Resource.ResourceMap observerId resource				-- ^ The /resource/s, from which one can determine /availability/.
	-> Timetable observerId timeslotId resourceIds level
	-> Model.TimetableCoordinates.Vector observerId timeslotId
locateUnallocatedAvailableUnreservedCoordinates observerIdsBookedForMeetingByTime resourceMap	= filter (
	uncurry (
		Data.Maybe.maybe True {-the time is unreserved by any observerId-}
	) . (
		Data.Set.notMember . Model.TimetableCoordinates.getObserverId &&& (`Data.Map.lookup` observerIdsBookedForMeetingByTime) . Model.TimetableCoordinates.getTime
	) -- Ensure that this observerId hasn't reserved the time for a meeting.
 ) . locateUnallocatedAvailableCoordinates resourceMap

{- |
	* Counts the total number of unallocated /time-slot/s, across all /observer/s in one week (i.e. the whle /timetable/); discounting those when the /observer/ is unavailable.

	* CAVEAT: this function takes no account of the possibility that when the /observer/ represents a /student-body/,
	some /time-slot/s may be allocated to unsupervised study,
	& the remainder must be multiplied by the number of members in the /student-body/.
-}
countUnallocatedAvailableTimeslots :: (
	Data.Array.IArray.Ix	timeslotId,
	Data.Resource.Resource	resource,
	Ord			observerId
 )
	=> Data.Resource.ResourceMap observerId resource
	-> Timetable observerId timeslotId resourceIds level
	-> Size.NTimeslots
countUnallocatedAvailableTimeslots resourceMap	= length . locateUnallocatedAvailableCoordinates resourceMap

{- |
	* Calculates the average over all /observer-Id/s,
	of the average over the /day/s in the week on which the /observer/ is actually available,
	of the /absolute deviation/ (<https://en.wikipedia.org/wiki/Absolute_deviation#Average_absolute_deviation>),
	in the number of free-/period/s.

	* CAVEAT: includes in the count, those /time-slot/s allocated for /meeting/s.
-}
calculateAverageAbsoluteDeviationOfFreeLessonsPerDay :: (
	Data.Array.IArray.Ix	timeslotId,
	Data.Resource.Resource	resource,
	Fractional		average,
	Ord			observerId
 )
	=> Data.Resource.ResourceMap observerId resource
	-> Timetable observerId timeslotId resourceIds level
	-> average
calculateAverageAbsoluteDeviationOfFreeLessonsPerDay resourceMap timetable
	| Data.Map.null timetable	= 0	-- TODO: query.
	| otherwise			= Factory.Math.Statistics.getMean {-amongst observers-} $ Data.Map.mapWithKey (
		\observerId timetableForWeek	-> Model.TimetableForWeek.calculateAverageAbsoluteDeviationOfFreeLessonsPerDay (resourceMap ! observerId) timetableForWeek :: Rational
	) timetable

{- |
	The number of /lesson/s booked for each /observer/, relative to the required limit of their teaching-time;
	based on 'Model.TimetableForWeek.calculateUtilisationRatio'.
-}
calculateUtilisationRatioByObserverId :: (
	Data.Array.IArray.Ix			timeslotId,
	Data.HumanResource.HumanResource	resource,
	Fractional				teachingRatio,
	Ord					observerId
 )
	=> Data.Resource.ResourceMap observerId resource
	-> Size.NTimeslots
	-> Timetable observerId timeslotId resourceIds level
	-> Data.Map.Map observerId teachingRatio
calculateUtilisationRatioByObserverId resourceMap nTimeslotsPerDay	= Data.Map.mapWithKey (Model.TimetableForWeek.calculateUtilisationRatio nTimeslotsPerDay . (resourceMap !))

{- |
	* Check whether all the /lesson/s currently booked for the specified time, are compatible with the specified /lesson/.

	* To pass, any synchronous /lesson/-definitions must either be identical to the proposed /lesson/, or use neither of its /resource/s.

	* CAVEAT: it doesn't check whether the /ability-stream/ of the /student-body/ matches those for the existing bookings.

	* CAVEAT: performance-hotspot.
-}
areMergeableWith :: (
	Data.Array.IArray.Ix		timeslotId,
	Eq				resourceIds,
	Eq				level,
	Model.ResourceUser.ResourceUser	resourceIds
 )
	=> Model.TimetableForWeek.Booking timeslotId resourceIds level
	-> Timetable observerId timeslotId resourceIds level
	-> Bool
areMergeableWith booking	= Model.ResourceUser.areMergeableWith (Model.TimetableForWeek.getBookedLesson booking) . extractSynchronousLessonsAt (Model.TimetableForWeek.getBookedTime booking)

{- |
	Calculates the mean, over those days on which the /resource/ is available,
	of the ratio of those free (neither booked with a /lesson/, not reserved for a /meeting/) /time-slot/s which comply with the specified preference,
	to the total number of free /time-slot/s.
-}
calculateMeanFreePeriodCompliance :: (
	Data.Array.IArray.Ix			timeslotId,
	Data.HumanResource.HumanResource	humanResource,
	Data.Resource.Resource			humanResource,
	Fractional				ratio,
	Ord					observerId
 )
	=> Data.Resource.ResourceMap observerId humanResource	-- ^ A map of /resource/s by /observerId/.
	-> (humanResource -> Temporal.Time.TimeSet timeslotId)	-- ^ A function which returns the times of the /meeting/s, for all /group/s of which this /human-resource/ is a member.
	-> Timetable observerId timeslotId resourceIds level
	-> ratio
calculateMeanFreePeriodCompliance humanResourceMap getMeetingTimes timetable
	| null ratios	= 1	-- TODO: query.
	| otherwise	= Factory.Math.Statistics.getMean ratios
	where
		ratios = [
			compliantPortionOfDay :: Rational |
				(observerId, timetableForWeek)	<- Data.Map.toList timetable,
				let profile	= humanResourceMap ! observerId,
				compliantPortionOfDay		<- Data.Maybe.maybe [] (
					\freePeriodPreference -> Model.TimetableForWeek.getFreePeriodCompliance freePeriodPreference (
						Temporal.Time.categoriseByDay $ getMeetingTimes profile
					) profile timetableForWeek
				) $ Data.HumanResource.getMaybeFreePeriodPreference profile
		 ] -- List-comprehension.

-- | A dummy type on which to hang instance-definitions.
newtype Wrapper observerId timeslotId resourceIds level	= MkWrapper {
	deconstruct	:: Timetable observerId timeslotId resourceIds level
}

instance (
	Data.Array.IArray.Ix	timeslotId,
	HXT.XmlPickler		level,
	HXT.XmlPickler		observerId,
	HXT.XmlPickler		resourceIds,
	HXT.XmlPickler		timeslotId,
	Ord			observerId,
	Show			level
 ) => HXT.XmlPickler (Wrapper observerId timeslotId resourceIds level) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		MkWrapper . Data.Map.fromList,	-- Construct from an association-list.
		Data.Map.toList . deconstruct	-- Deconstruct to an association-list.
	 ) . HXT.xpList1 {-can't be null-} . HXT.xpElem observerIdToTimetableAssociationTag $ HXT.xpElem observerIdTag HXT.xpickle `HXT.xpPair` Model.TimetableForWeek.xpickle

-- | A /lesson/ qualified by the /coordinates/ at which it is booked.
type Booking observerId timeslotId resourceIds level	= (Model.TimetableCoordinates.Coordinates observerId timeslotId, Model.Lesson.Lesson resourceIds level)

-- | Accessor.
getBookedCoordinates :: Booking observerId timeslotId resourceIds level -> Model.TimetableCoordinates.Coordinates observerId timeslotId
getBookedCoordinates	= fst

-- | Accessor.
getBookedLesson :: Booking observerId timeslotId resourceIds level -> Model.Lesson.Lesson resourceIds level
getBookedLesson	= snd

-- | Transform a /booking/, into a /generalised booking/.
toGeneralisedBooking :: Booking observerId timeslotId resourceIds level -> Model.GeneralisedBooking.GeneralisedBooking observerId timeslotId resourceIds level
toGeneralisedBooking	= Control.Arrow.second Just

-- | True if a /booking/ already exists in the /timetable/ which matches the specified /predicate/.
hasMatchingLessonAt
	:: (Data.Array.IArray.Ix timeslotId, Ord observerId)
	=> (Model.Lesson.Lesson resourceIds level -> Bool)		-- ^ Determines the suitability of any /lesson/ at the specified /coordinates/.
	-> Model.TimetableCoordinates.Coordinates observerId timeslotId	-- ^ The /coordinates/ at which to look for a /lesson/.
	-> Timetable observerId timeslotId resourceIds level
	-> Bool
hasMatchingLessonAt lessonPredicate coordinates	= Data.Maybe.maybe False {-there isn't a lesson, so there can't be a match-} lessonPredicate . getMaybeLesson coordinates

-- | True if the specified /booking/ already exists in the /timetable/.
isBookedWith :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			level,
	Eq			resourceIds,
	Ord			observerId
 )
	=> Booking observerId timeslotId resourceIds level	-- ^ The /coordinates/ & /lesson/ to match.
	-> Timetable observerId timeslotId resourceIds level
	-> Bool
isBookedWith booking	= hasMatchingLessonAt (== getBookedLesson booking) (getBookedCoordinates booking)

