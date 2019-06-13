{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wall -O1 #-}	-- CAVEAT: 'O2' once resulted in a SegV on calling the bound function 'toXHtmlLesson' with @ Temporal.TimeAxes.getByTimeslotId mergeDuplicateTimeslots = False @.
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

	* Define the data-structure, required to hold a single week's bookings, for any /observer/-id.

	* It forms a complete /timetable/, when combined with similar data-structures, for all observers.
-}

module WeekDaze.Model.TimetableForWeek(
-- * Types
-- ** Type-synonyms
	TimetableForWeek,
--	Association,
	Booking,
	GenericTimetableToMarkup,
	GenericTimetableToMarkup',
	GeneralisedLessonRunlengthByTimeslotIdByDay,
	RunlengthsByTimeslotIdByLessonByDay,
-- * Constants
--	meetingsCSSIdentifier,
	observerViewTerminatorCSSIdentifier,
--	originCSSIdentifier,
--	unallocatedTimeslotCSSIdentifier,
	unavailableCSSIdentifier,
--	weekendCSSIdentifier,
--	workdayCSSIdentifier,
--	tag,
--	dayToTimetableAssociationTag,
-- * Functions
	calculateAverageAbsoluteDeviationOfFreeLessonsPerDay,
	calculateUtilisationRatio,
	locateUnallocatedAvailableTimes,
	findGeneralisedLessonRunlengthsByTimeslotIdByDay,
	findSeparatedEqualLessonsWithinAnyDay,
	findSeparatedEqualSubjectLessonsWithinAnyDay,
	findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDay,
--	countWorkload,
	countSubjectWorkload,
	countWorkloadByLesson,
	countUnallocatedAvailableTimeslots,
	extractLessons,
	extractTimes,
	extractDistinctLessons,
	extractMaybeLessonsAt,
	getFreePeriodCompliance,
-- ** Constructor
	mkFreeTimetableForWeek,
-- ** Accessors
--	getDay,
--	getTimetableForDay,
	getMaybeLesson,
	getBookedTime,
	getBookedLesson,
-- ** Mutators
	defineTimeslot,
	undefineTimeslots,
-- ** Predicates
	hasMatchingLessonAt,
	isBookedWith,
	areAllSpecifiedTimesBookable,
	isBookedOnAdjacentDay,
	isDefinedTimeslot,
	isRunlengthReducibleAt,
-- ** Translation
	toXHtml,
	xpickle
) where

import			Control.Arrow((&&&), (***))
import			Data.Array.IArray((!), (//))
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Factory.Data.Interval
import qualified	Factory.Math.Statistics
import qualified	Text.XHtml.Strict
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT
import qualified	ToolShed.Data.List
import qualified	ToolShed.Data.Pair
import qualified	WeekDaze.Colour.HTMLColour			as Colour.HTMLColour
import qualified	WeekDaze.Colour.HTMLColourCode			as Colour.HTMLColourCode
import qualified	WeekDaze.Colour.RGB				as Colour.RGB
import qualified	WeekDaze.Data.Course				as Data.Course
import qualified	WeekDaze.Data.HumanResource			as Data.HumanResource
import qualified	WeekDaze.Data.Resource				as Data.Resource
import qualified	WeekDaze.Data.Subject				as Data.Subject
import qualified	WeekDaze.Model.GenerateLessonColourFrom		as Model.GenerateLessonColourFrom
import qualified	WeekDaze.Model.Lesson				as Model.Lesson
import qualified	WeekDaze.Model.Meeting				as Model.Meeting
import qualified	WeekDaze.Model.TimetableForDay			as Model.TimetableForDay
import qualified	WeekDaze.Size					as Size
import qualified	WeekDaze.Temporal.Availability			as Temporal.Availability
import qualified	WeekDaze.Temporal.Day				as Temporal.Day
import qualified	WeekDaze.Temporal.FreePeriodPreference		as Temporal.FreePeriodPreference
import qualified	WeekDaze.Temporal.Time				as Temporal.Time
import qualified	WeekDaze.Temporal.TimeAxes			as Temporal.TimeAxes
import qualified	WeekDaze.Temporal.TimeslotRequest		as Temporal.TimeslotRequest
import qualified	WeekDaze.Text.CSS				as Text.CSS
import			Text.XHtml.Strict((+++), (<<))

-- | Used to qualify output.
meetingsCSSIdentifier :: Text.CSS.CSSIdentifier
meetingsCSSIdentifier			= "meetings"

-- | Used to qualify output.
observerViewTerminatorCSSIdentifier :: Text.CSS.CSSIdentifier
observerViewTerminatorCSSIdentifier	= "observerViewTerminator"

-- | Used to qualify output.
originCSSIdentifier :: Text.CSS.CSSIdentifier
originCSSIdentifier			= "origin"

-- | Used to qualify output.
unallocatedTimeslotCSSIdentifier :: Text.CSS.CSSIdentifier
unallocatedTimeslotCSSIdentifier	= "unallocatedTimeslot"

-- | Used to qualify output.
unavailableCSSIdentifier :: Text.CSS.CSSIdentifier
unavailableCSSIdentifier		= "unavailable"

-- | Used to qualify output.
weekendCSSIdentifier :: Text.CSS.CSSIdentifier
weekendCSSIdentifier			= "weekend"

-- | Used to qualify output.
workdayCSSIdentifier :: Text.CSS.CSSIdentifier
workdayCSSIdentifier			= "workday"

-- | Used to qualify CSS & XML.
tag :: String
tag					= "timetableForWeek"

-- | Used to qualify XML.
dayToTimetableAssociationTag :: String
dayToTimetableAssociationTag		= "dayToTimetableAssociation"

-- | A timetable for observers with identical scheduling-requirements, for any week.
type TimetableForWeek timeslotId resourceIds level	= Data.Array.IArray.Array Temporal.Day.Day (Model.TimetableForDay.TimetableForDay timeslotId resourceIds level)

-- | The association on which 'TimetableForDay' is based.
type Association timeslotId resourceIds level	= (Temporal.Day.Day, Model.TimetableForDay.TimetableForDay timeslotId resourceIds level)

-- | Accessor.
getDay :: Association timeslotId resourceIds level -> Temporal.Day.Day
getDay	= fst

-- | Accessor.
getTimetableForDay :: Association timeslotId resourceIds level -> Model.TimetableForDay.TimetableForDay timeslotId resourceIds level
getTimetableForDay	= snd

-- | Get any /lesson/ booked at the specified /time/.
getMaybeLesson
	:: Data.Array.IArray.Ix timeslotId
	=> Temporal.Time.Time timeslotId
	-> TimetableForWeek timeslotId resourceIds level
	-> Model.Lesson.GeneralisedLesson resourceIds level
getMaybeLesson time	= (! Temporal.Time.getTimeslotId time) . (! Temporal.Time.getDay time)

-- | True if a /booking/ has been made at the specified /time/ in the /timetable/.
isDefinedTimeslot
	:: Data.Array.IArray.Ix timeslotId
	=> Temporal.Time.Time timeslotId
	-> TimetableForWeek timeslotId resourceIds level
	-> Bool
isDefinedTimeslot time	= Data.Maybe.isJust . getMaybeLesson time

-- | Extracts any /lesson/-definitions, from the specified /timeslot-Id/ on each /day/.
extractMaybeLessonsAt
	:: Data.Array.IArray.Ix timeslotId
	=> timeslotId
	-> TimetableForWeek timeslotId resourceIds level
	-> Data.Array.IArray.Array Temporal.Day.Day (Model.Lesson.GeneralisedLesson resourceIds level)
extractMaybeLessonsAt timeslotId	= Data.Array.IArray.amap (! timeslotId)

-- | Extracts the set of distinct /lesson/s from the specified /timetable/.
extractDistinctLessons :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			resourceIds,
	Ord			level
 ) => TimetableForWeek timeslotId resourceIds level -> Data.Set.Set (Model.Lesson.Lesson resourceIds level)
extractDistinctLessons	= Data.Set.fromList . extractLessons

{- |
	Calculates the /average absolute deviation/ (<https://en.wikipedia.org/wiki/Absolute_deviation#Average_absolute_deviation>) in the number of free-/period/s,
	over the /day/s in the week on which the /resource/ is actually /available/.
-}
calculateAverageAbsoluteDeviationOfFreeLessonsPerDay :: (
	Data.Array.IArray.Ix	timeslotId,
	Data.Resource.Resource	resource,
	Fractional		average
 )
	=> resource
	-> TimetableForWeek timeslotId resourceIds level
	-> average
calculateAverageAbsoluteDeviationOfFreeLessonsPerDay resource	= Factory.Math.Statistics.getAverageAbsoluteDeviation . map (
	Model.TimetableForDay.countUnallocatedTimeslots . getTimetableForDay	-- Which includes those time-slots allocated for meetings.
 ) . filter (
	(`Data.Resource.isAvailableOn` resource) . getDay
 ) . Data.Array.IArray.assocs

-- | Counts the number of /lesson/s booked.
countWorkload :: Data.Array.IArray.Ix timeslotId => TimetableForWeek timeslotId resourceIds level -> Size.NTimeslots
countWorkload	= length . extractLessons

-- | Returns the total number of /lesson/-definitions currently booked for the specified /subject/.
countSubjectWorkload
	:: (Data.Array.IArray.Ix timeslotId, Ord level)
	=> Data.Subject.Subject level
	-> TimetableForWeek timeslotId resourceIds level
	-> Size.NTimeslots
countSubjectWorkload subject	= length . filter ((== subject) . Model.Lesson.getSubject) . extractLessons	-- Neither 'foldr' nor 'foldl' were faster.

{- |
	* Returns the total number of each type of /lesson/ currently booked.

	* CAVEAT: if zero of the requested /lesson/ have been booked, then there won't be any matching key in the map.

	* CAVEAT: be careful with the interpretation of these results,
	unless it can be guaranteed that all /lesson/s for a given /subject/ are identical (i.e. they use the same /resourceIds/);
	otherwise the workload returned for a /subject/ could be split amongst different /lesson/s.
	Whilst this is typically the case for the /student-view/ of a /timetableForWeek/,
	it's may not be the case for other views, since the /student-class/ may vary.
-}
countWorkloadByLesson :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			resourceIds
 ) => TimetableForWeek timeslotId resourceIds level -> Data.Map.Map (Model.Lesson.Lesson resourceIds level) Size.NTimeslots
countWorkloadByLesson	= foldr (
	flip (Data.Map.insertWith $ const succ) 1
 ) Data.Map.empty {-initial value-} . extractLessons

{- |
	* The number of /lesson/s booked for the specified /human-resource/, relative to the limit of their teaching-time.

	* This factors into the denominator, both the /availability/ of the /resource/,
	& the ratio of that working-time for which they're required to be in teaching rather than some other job-role.
-}
calculateUtilisationRatio :: (
	Data.Array.IArray.Ix			timeslotId,
	Data.HumanResource.HumanResource	humanResource,
	Fractional				teachingRatio
 )
	=> Size.NTimeslots
	-> humanResource
	-> TimetableForWeek timeslotId resourceIds level
	-> teachingRatio
calculateUtilisationRatio nTimeslotsPerDay humanResource timetableForWeek
	| denominator == 0	= if numerator == 0
		then 1	-- CAVEAT: actually indeterminate.
		else error "WeekDaze.Model.TimetableForWeek.calculateUtilisationRatio:\tattempt to divide by zero (time-slots per week of teaching)."
	| otherwise		= fromIntegral numerator / fromIntegral denominator
	where
		numerator	= countWorkload timetableForWeek
		denominator	= Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay humanResource

-- | Replace any /lesson/-definition at the specified /time-coordinate/.
defineTimeslot
	:: Data.Array.IArray.Ix timeslotId
	=> (Temporal.Time.Time timeslotId, Model.Lesson.GeneralisedLesson resourceIds level)
	-> TimetableForWeek timeslotId resourceIds level
	-> TimetableForWeek timeslotId resourceIds level
defineTimeslot (time, maybeLesson) timetableForWeek	= timetableForWeek // [
	(
		day,
		Model.TimetableForDay.defineTimeslot (Temporal.Time.getTimeslotId time, maybeLesson) $ timetableForWeek ! day
	) -- Pair.
 ] where
	day	= Temporal.Time.getDay time

-- | Undefines any /lesson/ booked at each of the specified /time-coordinate/s.
undefineTimeslots
	:: (Data.Array.IArray.Ix timeslotId, Data.Foldable.Foldable foldable)
	=> TimetableForWeek timeslotId resourceIds level
	-> foldable (Temporal.Time.Time timeslotId)
	-> TimetableForWeek timeslotId resourceIds level
undefineTimeslots	= Data.Foldable.foldr $ defineTimeslot . flip (,) Nothing

{- |
	Measure the ratio, for each /day/ on which the observer, for whom this weekly /timetable/ was intended, is /available/,
	of those free (neither reserved for a /meeting/ nor booked with a /lesson/) /time-slot/s which comply with the specified preference,
	to the total number of free /time-slot/s,
-}
getFreePeriodCompliance :: (
	Data.Array.IArray.Ix	timeslotId,
	Data.Resource.Resource	resource,
	Fractional		ratio
 )
	=> Temporal.FreePeriodPreference.FreePeriodPreference	-- ^ This observer's preferred position, within any /day/, of free /time-slot/s.
	-> Temporal.Time.TimesByDay timeslotId			-- ^ The /meeting/-times for the /group/s of which this observer is a member.
	-> resource						-- ^ The observer for whom this weekly /timetable/ was intended.
	-> TimetableForWeek timeslotId resourceIds level
	-> [ratio]						-- ^ The portion of those free /time-slot/s in each /day/, which meet the observer's preference.
getFreePeriodCompliance freePeriodPreference meetingTimesByDay resource timetableForWeek	= map (
	uncurry (
		Model.TimetableForDay.measureFreePeriodCompliance freePeriodPreference
	) . (
		Data.Maybe.fromMaybe Data.Set.empty . (`Data.Map.lookup` meetingTimesByDay) &&& (timetableForWeek !)
	)
 ) . Data.Set.elems . Temporal.Availability.deconstruct $ Data.Resource.getAvailability resource

{- |
	* A basic specification required to convert a /timetable/ to XHTML.

	* The first three parameters supply auxiliary information required for styling.
-}
type GenericTimetableToMarkup minimumContrastRatio timetable
	= Temporal.TimeAxes.TimeAxes Bool				-- ^ Whether to merge duplicate /lesson/s, between adjacent /day/s & also between consecutive /time-slot/s.
	-> Temporal.TimeAxes.TimeAxes Bool				-- ^ Whether to depict axis-labels.
	-> Temporal.Day.Weekend						-- ^ Define the /day/s which constitute the weekend.
	-> Maybe (
		Model.GenerateLessonColourFrom.GenerateLessonColourFrom,
		minimumContrastRatio
	)								-- ^ Whether to generate the colour of a /lesson/ from one of its attributes, or to delegate colouring to a CSS-file.
	-> timetable							-- ^ The /timetable/ to represent; which can be from any view, or merely a slice dedicated to one /observerId/.
	-> Text.XHtml.Strict.Html

-- | Enhance 'GenericTimetableToMarkup', with parameters for styling.
type GenericTimetableToMarkup' locationId minimumContrastRatio teacherId timeslotId timetable
	= Model.Meeting.MeetingsByTime timeslotId locationId teacherId
	-> GenericTimetableToMarkup minimumContrastRatio timetable

-- | Render in /XHTML/, as a /table/, with /day/s varying horizontally & /timeslotId/s varying vertically.
toXHtml :: forall level locationId minimumContrastRatio resource resourceIds synchronisationId teacherId timeslotId. (
	Data.Array.IArray.Ix	timeslotId,
	Data.Resource.Resource	resource,
	Eq			level,
	Eq			resourceIds,
	Fractional		minimumContrastRatio,
	Ord			minimumContrastRatio,
#if !MIN_VERSION_containers(0,5,2)
	Ord			locationId,
	Ord			teacherId,
#endif
	Show			level,
	Show			resourceIds,
	Text.XHtml.Strict.HTML	level,
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	resourceIds,
	Text.XHtml.Strict.HTML	synchronisationId,
	Text.XHtml.Strict.HTML	teacherId,
	Text.XHtml.Strict.HTML	timeslotId
 )
	=> (Model.Lesson.Lesson resourceIds level -> Data.Course.Course synchronisationId level timeslotId)	-- ^ Find the /course/ to which the specified /lesson/ belongs.
	-> resource												-- ^ The /resource/ by whom the /timetable/ was designed to be viewed.
	-> GenericTimetableToMarkup' locationId minimumContrastRatio teacherId timeslotId (TimetableForWeek timeslotId resourceIds level)
toXHtml findCourseFor resource meetingsByTime mergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom timetableForWeek = (
	Text.XHtml.Strict.table Text.XHtml.Strict.! [
		Text.XHtml.Strict.theclass tag,
		Text.XHtml.Strict.title "Timetable for Week"
	] <<
 ) . (
	if Temporal.TimeAxes.getByDay displayAxisLabels
		then (
			Text.XHtml.Strict.tr << (
				if Temporal.TimeAxes.getByTimeslotId displayAxisLabels
					then (
						(
							Text.XHtml.Strict.th Text.XHtml.Strict.! [Text.XHtml.Strict.theclass originCSSIdentifier]	-- Make this behave as a table-header element; for CSS width & height settings.
						) << Text.XHtml.Strict.noHtml :
					) -- Section.
					else id
			) (
				map (
					\day -> (
						Text.XHtml.Strict.th Text.XHtml.Strict.! if Data.Set.member day weekend
							then [
								Text.XHtml.Strict.theclass weekendCSSIdentifier,
								Text.XHtml.Strict.title weekendCSSIdentifier
							]
							else [
								Text.XHtml.Strict.theclass workdayCSSIdentifier,
								Text.XHtml.Strict.title "Work-day"
							]
					) << day
				) Temporal.Day.range
			) :
		) -- Section.
		else id
 ) . map (
	(Text.XHtml.Strict.tr <<) . (
		if Temporal.TimeAxes.getByDay mergeDuplicateTimeslots
			then map (
				uncurry (Text.XHtml.Strict.!) . (head &&& return {-to List-monad-} . Text.XHtml.Strict.colspan . length)
			) . Data.List.groupBy (ToolShed.Data.List.equalityBy show)	-- 'Text.XHtml.Strict.Html' doesn't implement Eq ?!
			else id
	)
 ) . Data.List.transpose . (
	if Temporal.TimeAxes.getByTimeslotId displayAxisLabels
		then (
			map (
				Text.XHtml.Strict.th <<
			) (
				Data.Array.IArray.indices $ timetableForWeek ! minBound {-arbitrarily-}
			) :
		) -- Section.
		else id
 ) . map (
	\(day, timetableForDay) -> let
		isAvailable :: Bool
		isAvailable	= Data.Resource.isAvailableOn day resource

		toXHtmlLesson :: Size.NTimeslots -> timeslotId -> Model.Lesson.GeneralisedLesson resourceIds level -> Text.XHtml.Strict.Html
		toXHtmlLesson rowspan timeslotId	= Data.Maybe.maybe (
			if isAvailable
				then Data.Maybe.maybe (
					Text.XHtml.Strict.td Text.XHtml.Strict.! [
						Text.XHtml.Strict.rowspan rowspan,
						Text.XHtml.Strict.theclass unallocatedTimeslotCSSIdentifier,
						Text.XHtml.Strict.title "Not booked for teaching, but possibly kept free for another activity."
					] << "Unallocated"
				) (
					(
						(
							Text.XHtml.Strict.td Text.XHtml.Strict.! [
								Text.XHtml.Strict.rowspan rowspan,
								Text.XHtml.Strict.theclass meetingsCSSIdentifier,
								Text.XHtml.Strict.title meetingsCSSIdentifier
							]
						) <<
					) . Data.List.intersperse Text.XHtml.Strict.hr . map Text.XHtml.Strict.toHtml . Data.Set.toList
				) $ Data.Map.lookup (Temporal.Time.mkTime day timeslotId) meetingsByTime
				else Text.XHtml.Strict.td Text.XHtml.Strict.! [
					Text.XHtml.Strict.rowspan rowspan,
					Text.XHtml.Strict.theclass unavailableCSSIdentifier,
					Text.XHtml.Strict.title "Not scheduled to be available today."
				] << "N/A"
		 ) $ \lesson -> Text.XHtml.Strict.td Text.XHtml.Strict.! (Text.XHtml.Strict.rowspan rowspan :) (
			Data.Maybe.maybe [
				Text.XHtml.Strict.theclass . Text.CSS.mkIdentifier . (
					(Data.Subject.topicTag ++ "_") ++	-- Avoid clash between arbitrary topic & other CSS-identifiers.
				) . Data.Subject.getTopic $ Model.Lesson.getSubject lesson
			] (
				\(dataSource, minimumContrastRatio)	-> (
					\s -> let
						htmlColourCode, complementaryHTMLColourCode :: Colour.HTMLColourCode.HTMLColourCode
						htmlColourCode			= Colour.HTMLColourCode.generateHTMLColourCodeFrom s
						complementaryHTMLColourCode	= Colour.HTMLColourCode.deriveComplementaryHTMLColourCode htmlColourCode
						(red, green, blue)		= Colour.RGB.toTriple . (
							Colour.RGB.toRGBUnitInterval	:: Colour.RGB.RGB Int -> Colour.RGB.RGB Rational
						 ) $ Colour.HTMLColour.htmlColourCodeToRGB htmlColourCode `Colour.RGB.absDifference` Colour.HTMLColour.htmlColourCodeToRGB complementaryHTMLColourCode
					in if Factory.Math.Statistics.getMean [red, green, blue] < minimumContrastRatio
						then []	-- Insufficient contrast.
						else [
							Text.XHtml.Strict.thestyle $ "color: " ++ htmlColourCode ++ "; background-color: " ++ complementaryHTMLColourCode
						]
				) $ case dataSource of
					Model.GenerateLessonColourFrom.Lesson		-> show lesson
					Model.GenerateLessonColourFrom.Subject		-> show $ Model.Lesson.getSubject lesson
					Model.GenerateLessonColourFrom.Topic		-> Data.Subject.getTopic $ Model.Lesson.getSubject lesson
					Model.GenerateLessonColourFrom.Level		-> show . Data.Subject.getLevel $ Model.Lesson.getSubject lesson
					Model.GenerateLessonColourFrom.ResourceIds	-> show $ Model.Lesson.getResourceIds lesson
			) maybeGenerateLessonColourFrom
		 ) << (
			Data.Maybe.maybe Text.XHtml.Strict.noHtml Text.XHtml.Strict.toHtml (
				Data.Course.getMaybeSynchronisationId $ findCourseFor lesson
			) +++ lesson
		 )
	in (
		if Temporal.TimeAxes.getByTimeslotId mergeDuplicateTimeslots
			then concatMap (
				\equalsByGeneralisedLesson -> let
					nDuplicates	= length equalsByGeneralisedLesson
				in uncurry (toXHtmlLesson nDuplicates {-partially apply-}) (
					head equalsByGeneralisedLesson
				) : replicate (pred nDuplicates) Text.XHtml.Strict.noHtml -- CAVEAT: bodge to prevent the column beneath rising.
			) . Data.List.groupBy (
				if isAvailable
					then ToolShed.Data.List.equalityBy (
						Control.Arrow.first {-timeslotId-} $ (
							Data.Set.map Model.Meeting.getGroupId `fmap`	-- Equality by the list of groups which is meeting.
						) . (
							`Data.Map.lookup` meetingsByTime
						) . Temporal.Time.mkTime day
					)
					else ToolShed.Data.List.equalityBy Model.TimetableForDay.getMaybeLesson	-- Which should be consistently undefined.
			)
			else map (uncurry $ toXHtmlLesson 1 {-partially apply-})
	) $ Data.Array.IArray.assocs timetableForDay
 ) $ Data.Array.IArray.assocs timetableForWeek

-- | Extracts the /lesson/s from the specified /timetable/, discarding the /booking-time/.
extractLessons :: Data.Array.IArray.Ix timeslotId => TimetableForWeek timeslotId resourceIds level -> [Model.Lesson.Lesson resourceIds level]
-- extractLessons	= Data.Foldable.concatMap (Data.Maybe.catMaybes . Data.Array.IArray.elems)	-- CAVEAT: too slow.
extractLessons timetableForWeek	= [
	lesson |
		timetableForDay	<- Data.Array.IArray.elems timetableForWeek,
		Just lesson	<- Data.Array.IArray.elems timetableForDay
 ] -- List-comprehension.

-- | Extract the list of all /time/s in the specified /timetable/, regardless of whether a /lesson/ has been booked there.
extractTimes :: Data.Array.IArray.Ix timeslotId => TimetableForWeek timeslotId resourceIds level -> [Temporal.Time.Time timeslotId]
extractTimes timetableForWeek	= [
	Temporal.Time.mkTime day timeslotId |
		(day, timetableForDay)	<- Data.Array.IArray.assocs timetableForWeek,
		timeslotId		<- Data.Array.IArray.indices timetableForDay	-- In practice, these are consistent throughout the week.
 ] -- List-comprehension.

{- |
	* Finds the /coordinate/s of all unallocated /time/s, when the specified /resource/ is /available/.

	* CAVEAT: doesn't account for /time/s reserved for /meeting/s.
-}
locateUnallocatedAvailableTimes
	:: (Data.Array.IArray.Ix timeslotId, Data.Resource.Resource resource)
	=> resource	-- ^ Used to determine /availability/.
	-> TimetableForWeek timeslotId resourceIds level
	-> [Temporal.Time.Time timeslotId]
locateUnallocatedAvailableTimes resource timetableForWeek	= [
	Temporal.Time.mkTime day unallocatedAvailableTimeslotId |
		(day, timetableForDay)		<- Data.Array.IArray.assocs timetableForWeek,
		Data.Resource.isAvailableOn day resource,
		unallocatedAvailableTimeslotId	<- Model.TimetableForDay.locateUnallocatedTimeslots timetableForDay
 ] -- List-comprehension.

{- |
	* Counts the total number of unallocated /time-slot/s, discounting those when the /observer/ is unavailable.

	* CAVEAT: this function takes no account of the possibility that when the /observer/ represents a /student-body/,
	some /time-slots/ may be allocated for unsupervised study,
	& the remainder must be multiplied by the number of members in the /student-body/.
-}
countUnallocatedAvailableTimeslots
	:: (Data.Array.IArray.Ix timeslotId, Data.Resource.Resource resource)
	=> resource
	-> TimetableForWeek timeslotId resourceIds level
	-> Size.NTimeslots
countUnallocatedAvailableTimeslots resource	= length . locateUnallocatedAvailableTimes resource

-- | Defines a pickler to convert the specified /timetable/ to, or from, XML.
xpickle :: (
	Data.Array.IArray.Ix	timeslotId,
	HXT.XmlPickler		level,
	HXT.XmlPickler		resourceIds,
	HXT.XmlPickler		timeslotId,
	Show			level
 ) => HXT.PU (TimetableForWeek timeslotId resourceIds level)
xpickle	= HXT.xpElem tag . HXT.xpWrap (
	Data.Array.IArray.array (minBound, maxBound),	-- Construct from an association-list.
	Data.Array.IArray.assocs				-- Deconstruct to an association-list.
 ) . HXT.xpList1 {-can't be null-} . HXT.xpElem dayToTimetableAssociationTag $ HXT.xpPair HXT.xpickle {-day-} Model.TimetableForDay.xpickle

-- | A /lesson/ qualified by the /time/ at which it is booked.
type Booking timeslotId resourceIds level	= (Temporal.Time.Time timeslotId, Model.Lesson.Lesson resourceIds level)

-- | Accessor.
getBookedTime :: Booking timeslotId resourceIds level -> Temporal.Time.Time timeslotId
getBookedTime	= fst

-- | Accessor.
getBookedLesson :: Booking timeslotId resourceIds level -> Model.Lesson.Lesson resourceIds level
getBookedLesson	= snd

-- | True if a /booking/ already exists in the /timetable/ which matches the specified /predicate/.
hasMatchingLessonAt
	:: Data.Array.IArray.Ix timeslotId
	=> (Model.Lesson.Lesson resourceIds level -> Bool)	-- ^ Determines the suitability of any /lesson/ at the specified /time/.
	-> Temporal.Time.Time timeslotId			-- ^ The /time/ at which to look for a /lesson/.
	-> TimetableForWeek timeslotId resourceIds level
	-> Bool
hasMatchingLessonAt lessonPredicate time	= Data.Maybe.maybe False {-there isn't a lesson, so there can't be a match-} lessonPredicate . getMaybeLesson time

-- | True if the specified /booking/ already exists in the /timetable/.
isBookedWith :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			level,
	Eq			resourceIds
 )
	=> Booking timeslotId resourceIds level	-- ^ The /time/ & /lesson/ to match.
	-> TimetableForWeek timeslotId resourceIds level
	-> Bool
isBookedWith booking	= hasMatchingLessonAt (== getBookedLesson booking) (getBookedTime booking)

-- | Constructor. Create an unallocated /timetable/, for a whole week.
mkFreeTimetableForWeek :: (Data.Array.IArray.Ix timeslotId, Enum timeslotId) => Factory.Data.Interval.Interval timeslotId -> TimetableForWeek timeslotId resourceIds level
mkFreeTimetableForWeek	= Data.Array.IArray.array (minBound, maxBound) . zip [minBound .. maxBound] . repeat . Model.TimetableForDay.mkFreeTimetableForDay

-- | A runlength-encoded list of generalised (i.e. potentially undefined) /lesson/s, indexed by the /day/ & then /timeslotId/, at which each runlength begins.
type GeneralisedLessonRunlengthByTimeslotIdByDay timeslotId resourceIds level	= Data.Array.IArray.Array Temporal.Day.Day (Model.TimetableForDay.GeneralisedLessonRunlengthByTimeslotId timeslotId resourceIds level)

-- | Finds consecutive equal generalised (i.e. potentially undefined) /lesson/s, & the run-length of each sequence, indexed by /day/ & then /timeslotId/.
findGeneralisedLessonRunlengthsByTimeslotIdByDay :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			resourceIds,
	Eq			level
 ) => TimetableForWeek timeslotId resourceIds level -> GeneralisedLessonRunlengthByTimeslotIdByDay timeslotId resourceIds level
findGeneralisedLessonRunlengthsByTimeslotIdByDay	= Data.Array.IArray.amap Model.TimetableForDay.findGeneralisedLessonRunlengthsByTimeslotId

-- | Finds separated equal /lesson/s, within the /timetable/ for any single /day/.
findSeparatedEqualLessonsWithinAnyDay :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			resourceIds,
	Ord			level
 ) => TimetableForWeek timeslotId resourceIds level -> Model.TimetableForDay.LessonRunlengths resourceIds level
findSeparatedEqualLessonsWithinAnyDay	= Data.Foldable.concatMap Model.TimetableForDay.findSeparatedEqualLessons

-- | Finds separated /lesson/s of equal /subject/, within the /timetable/ for any single /day/.
findSeparatedEqualSubjectLessonsWithinAnyDay :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			resourceIds,
	Ord			level
 ) => TimetableForWeek timeslotId resourceIds level -> Model.TimetableForDay.LessonRunlengths resourceIds level
findSeparatedEqualSubjectLessonsWithinAnyDay	= Data.Foldable.concatMap Model.TimetableForDay.findSeparatedEqualSubjectLessons

-- | A runlength-encoded list of generalised (i.e. potentially undefined) /lesson/s, indexed by the /day/ & then /timeslotId/, at which each runlength begins.
type RunlengthsByTimeslotIdByLessonByDay resourceIds level timeslotId	= Data.Array.IArray.Array Temporal.Day.Day (Model.TimetableForDay.RunlengthsByTimeslotIdByLesson resourceIds level timeslotId)

{- |
	* Finds runlengths of separated equal /lesson/s, within the /timetable/ for any single /day/; separated unallocated /time-slot/s don't qualify.

	* Returns an array indexed by /day/, of lists associated by common /lesson/, of lists associated by starting /timeslotId/, of runlengths.
-}
findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDay :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			resourceIds,
	Ord			level
 ) => TimetableForWeek timeslotId resourceIds level -> RunlengthsByTimeslotIdByLessonByDay resourceIds level timeslotId
findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDay	= Data.Array.IArray.amap Model.TimetableForDay.findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLesson

{- |
	'True' if all the /time/s specified by a /course/, are currently unbooked,
	& the /resource/ is regularly /available/ on each requested /day/.
-}
areAllSpecifiedTimesBookable
	:: (Data.Resource.Resource resource, Data.Array.IArray.Ix timeslotId)
	=> resource
	-> TimetableForWeek timeslotId resourceIds level		-- ^ The /timetable/ whose bookings to inspect.
	-> Data.Course.Course synchronisationId level timeslotId	-- ^ The /course/ whose specified /time/s, to query.
	-> Bool
areAllSpecifiedTimesBookable resource timetableForWeek	= Data.Foldable.all (
	uncurry (&&) . (
		not . (`isDefinedTimeslot` timetableForWeek) &&& (`Data.Resource.isAvailableOn` resource) . Temporal.Time.getDay
	)
 ) . Temporal.TimeslotRequest.getSpecifiedTimes . Data.Course.getTimeslotRequest

-- | True if the specified /lesson/ is booked an an adjacent /day/.
isBookedOnAdjacentDay :: (
#if !MIN_VERSION_array(0,5,2)
	Data.Array.IArray.Ix	timeslotId,
#endif /* CAVEAT: constraint unnecessary from "Data.Array.IArray-0.5.1.1" */
	Eq			level,
	Eq			resourceIds
 )
	=> TimetableForWeek timeslotId resourceIds level	-- ^ The /timetable/ whose bookings to inspect.
	-> Model.Lesson.Lesson resourceIds level		-- ^ The /lesson/ to match.
	-> Temporal.Day.Day					-- ^ Today.
	-> Bool
isBookedOnAdjacentDay timetableForWeek lesson	= uncurry (||) . ToolShed.Data.Pair.mirror (
	Data.Foldable.elem (Just lesson) . (timetableForWeek !)
 ) . Temporal.Day.getAdjacentDays

{- |
	* True if the /course/ requires only a trivial /minimumConsecutiveLessons/,
	or the referenced /lesson/ is at either end of a runlength which exceeds /minimumConsecutiveLessons/.

	* If True, then /minimumConsecutiveLessons/ doesn't preclude the referenced /lesson/ from being unbooked; though there may be other reasons why it shouldn't.

	* CAVEAT: if @ minimumConsecutiveLessons==1 @, but a runlength of three or more have been booked, then unbooking the middle one will result in a split session.
-}
isRunlengthReducibleAt :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			resourceIds
 )
	=> TimetableForWeek timeslotId resourceIds level	-- ^ The /timetable/ whose bookings to inspect.
	-> Temporal.Time.Time timeslotId
	-> Data.Course.Course synchronisationId level timeslotId
	-> Bool
isRunlengthReducibleAt timetableForWeek time course
	| minimumConsecutiveLessons == 1	= True
	| otherwise				= uncurry (&&) . (
		uncurry (||) . ToolShed.Data.Pair.mirror (
			== timeslotId	-- Check that this timeslot terminates the runlength; rather than splits it.
		) *** (
			> minimumConsecutiveLessons	-- Check that the runlength is excessive.
		)
	) . Model.TimetableForDay.measureRunlengthAt timeslotId {-which returns a pair-} . (
		timetableForWeek !	-- Lookup the timetableForDay.
	) $ Temporal.Time.getDay time
	where
		timeslotId			= Temporal.Time.getTimeslotId time
		minimumConsecutiveLessons	= Data.Course.getMinimumConsecutiveLessons course

