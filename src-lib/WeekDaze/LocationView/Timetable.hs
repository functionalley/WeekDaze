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

	* Defines the personal /timetable/ for any week, for each /location/.

	* It contains identical information to 'StudentView.Timetable.Timetable',
	but the data has been re-indexed to present it in a form relevant to a /location/ rather than a /student/.
-}

module WeekDaze.LocationView.Timetable(
-- * Types
-- ** Type-synonyms
	Timetable,
--	Booking,
-- * Functions
-- ** Mutators
--	bookStudentViewLesson,
--	bookLocationViewLesson,
-- ** Translation
	fromStudentViewTimetable,
	toStudentViewTimetable,
	toXHtml
) where

import			Data.Map((!))
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import qualified	WeekDaze.Aggregate.LocationCatalogue	as Aggregate.LocationCatalogue
import qualified	WeekDaze.Data.Course			as Data.Course
import qualified	WeekDaze.Data.Location			as Data.Location
import qualified	WeekDaze.LocationView.Lesson		as LocationView.Lesson
import qualified	WeekDaze.LocationView.LessonResourceIds	as LocationView.LessonResourceIds
import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.Model.Meeting			as Model.Meeting
import qualified	WeekDaze.Model.Timetable		as Model.Timetable
import qualified	WeekDaze.Model.TimetableForWeek		as Model.TimetableForWeek
import qualified	WeekDaze.StudentView.LessonResourceIds	as StudentView.LessonResourceIds
import qualified	WeekDaze.StudentView.Timetable		as StudentView.Timetable
import qualified	WeekDaze.Temporal.Time			as Temporal.Time
import qualified	WeekDaze.Text.CSS			as Text.CSS
import			Text.XHtml.Strict((+++), (<<))
import			WeekDaze.Text.XHTML()

-- | An association-list of the timetables for all /locations/.
type Timetable locationId timeslotId teacherId level	= Model.Timetable.Timetable locationId timeslotId (LocationView.LessonResourceIds.LessonResourceIds teacherId) level

{- |
	* Build a 'Timetable' by inverting a 'StudentView.Timetable.Timetable'.

	* CAVEAT: in the original 'StudentView.Timetable.Timetable', different /student-bodies/ may simultaneously reference a single /location/.
	This is normal behaviour, provided the /location/ has sufficient capacity,
	& is implemented seemlessly, as a /map/-structure with one or more /key/s referencing a /value/, containing the same /location/.

	* When inverting such a /timetable/, this many-to-one relationship becomes a one-to-many, & a single /location-id/ must now reference many /student-bodies/.
	The /map/-structure (cf. /multi-map/), can't directly accommodate multiple identical keys, with different values,
	so this is achieved by merging the /student-bodies/ into a class of /student/s.
-}
fromStudentViewTimetable :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			teacherId,
	Ord			locationId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> Bool							-- ^ Permit a temporary /student-body/ merger.
	-> Timetable locationId timeslotId teacherId level	-- ^ An unallocated /timetable/, supplied for efficiency.
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> Timetable locationId timeslotId teacherId level
fromStudentViewTimetable permitTemporaryStudentBodyMerger	= Data.Map.foldrWithKey (
	\studentBody studentViewTimetableForWeek locationViewTimetable	-> snd {-timetable-} $ Data.Foldable.foldr (
		\studentViewTimetableForDay (day, locationViewTimetable')	-> (
			pred day,	-- Because of the use of 'foldr'.
			snd {-timetable-} $ Data.Foldable.foldr (
				\maybeStudentViewLesson (timeslotId, locationViewTimetable'')	-> (
					pred timeslotId,	-- Because of the use of 'foldr'.
					Data.Maybe.maybe locationViewTimetable'' (
						($ locationViewTimetable'') . bookStudentViewLesson permitTemporaryStudentBodyMerger {-partially apply-} . (,) (
							studentBody,
							Temporal.Time.mkTime day timeslotId
						) {-construct a booking-}
					) maybeStudentViewLesson
				)
			) (
				snd {-maximum timeslotId-} $ Data.Array.IArray.bounds studentViewTimetableForDay,
				locationViewTimetable'
			) studentViewTimetableForDay
		)
	) (
		snd {-maximum day-} $ Data.Array.IArray.bounds studentViewTimetableForWeek,
		locationViewTimetable
	) studentViewTimetableForWeek
 )

{- |
	* Build a 'StudentView.Timetable.Timetable', by inverting a 'Timetable'.

	* Each /lesson/ in the /timetable/ seen from the /location/'s perspective, contains a /student-class/.
	This /student-class/ must be broken into /student-bodies/, before inserting repeatedly into the required /timetable/ as seen from the /student-body/'s perpective.
-}
toStudentViewTimetable
	:: (Data.Array.IArray.Ix timeslotId, Enum timeslotId)
	=> StudentView.Timetable.Timetable timeslotId locationId teacherId level	-- ^ An unallocated /timetable/, supplied for efficiency.
	-> Timetable locationId timeslotId teacherId level
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
toStudentViewTimetable	= Data.Map.foldrWithKey (
	\locationId locationViewTimetableForWeek studentViewTimetable	-> snd {-timetable-} $ Data.Foldable.foldr (
		\locationViewTimetableForDay (day, studentViewTimetable')	-> (
			pred day,	-- Because of the use of 'foldr'.
			snd {-timetable-} $ Data.Foldable.foldr (
				\maybeLocationViewLesson (timeslotId, studentViewTimetable'')	-> (
					pred timeslotId,	-- Because of the use of 'foldr'.
					Data.Maybe.maybe studentViewTimetable'' (
						($ studentViewTimetable'') . bookLocationViewLesson {-partially apply-} . (,) (
							locationId,
							Temporal.Time.mkTime day timeslotId
						) {-construct a booking-}
					) maybeLocationViewLesson
				)
			) (
				snd {-maximum timeslotId-} $ Data.Array.IArray.bounds locationViewTimetableForDay,
				studentViewTimetable'
			) locationViewTimetableForDay
		)
	) (
		snd {-maximum day-} $ Data.Array.IArray.bounds locationViewTimetableForWeek,
		studentViewTimetable
	) locationViewTimetableForWeek
 )

-- | Make a 'StudentView.Timetable.Booking' in a /LocationView-timetable/.
bookStudentViewLesson :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			level,
	Eq			teacherId,
	Ord			locationId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> Bool									-- ^ Permit a temporary /student-body/ merger.
	-> StudentView.Timetable.Booking timeslotId locationId teacherId level	-- ^ The /coordinates/ & /lesson/ from the perspective of the /student/-view of the /timetable/.
	-> Timetable locationId timeslotId teacherId level			-- ^ The /timetable/ as seen from the /location/'s perspective.
	-> Timetable locationId timeslotId teacherId level
bookStudentViewLesson permitTemporaryStudentBodyMerger ((studentBody, time), studentViewLesson) locationViewTimetable	= Model.Timetable.defineTimeslot (
	locationViewTimetableCoordinates,
	Just studentViewLesson {
		Model.Lesson.getResourceIds	= LocationView.LessonResourceIds.mkLessonResourceIds (
			Data.Set.insert studentBody $ case Model.Timetable.getMaybeLesson locationViewTimetableCoordinates locationViewTimetable of
				Nothing	-> Data.Set.empty	-- Start a new student-class.
				Just locationViewLesson
					| and [
						permitTemporaryStudentBodyMerger,
						LocationView.LessonResourceIds.getTeacherId locationViewResourceIds == teacherId,
						Model.Lesson.getSubject locationViewLesson == Model.Lesson.getSubject studentViewLesson
					]		-> LocationView.LessonResourceIds.getStudentClass locationViewResourceIds
					| otherwise	-> error $ "WeekDaze.LocationView.Timetable.bookStudentViewLesson:\tlesson at coordinates=" ++ show locationViewTimetableCoordinates ++ " has already been booked in either a different subject or location; " ++ show (locationViewLesson, studentViewLesson) ++ "."
					where
						locationViewResourceIds	= Model.Lesson.getResourceIds locationViewLesson
		 ) teacherId
	} -- Mutate.
 ) locationViewTimetable where
	studentViewResourceIds			= Model.Lesson.getResourceIds studentViewLesson
	teacherId				= StudentView.LessonResourceIds.getTeacherId studentViewResourceIds
	locationViewTimetableCoordinates	= (StudentView.LessonResourceIds.getLocationId studentViewResourceIds, time)

-- | A /lesson/ qualified by the /coordinates/ at which it is booked.
type Booking locationId timeslotId teacherId level	= Model.Timetable.Booking locationId timeslotId (LocationView.LessonResourceIds.LessonResourceIds teacherId) level

-- | Books a /LocationView-lesson/ in a /StudentView-timetable/.
bookLocationViewLesson
	:: Data.Array.IArray.Ix timeslotId
	=> Booking locationId timeslotId teacherId level				-- ^ The /coordinates/ & /lesson/ from the perspective of the /location/-view of the /timetable/.
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level	-- ^ The /timetable/ as seen from the /student/'s perspective.
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
bookLocationViewLesson ((locationId, time), locationViewLesson) studentViewTimetable	= Data.Set.foldr (
	Model.Timetable.defineTimeslot . flip (,) (
		Just $ LocationView.Lesson.toStudentView locationId locationViewLesson
	) . flip (,) time
 ) studentViewTimetable $ LocationView.LessonResourceIds.getStudentClass locationViewResourceIds where
	locationViewResourceIds	= Model.Lesson.getResourceIds locationViewLesson

-- | Render in /XHTML/, as a /definition-list/.
toXHtml :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		minimumContrastRatio,
	Ord			level,
	Ord			locationId,
	Ord			minimumContrastRatio,
	Ord			teacherId,
	Show			level,
	Show			teacherId,
	Text.XHtml.Strict.HTML	campus,
	Text.XHtml.Strict.HTML	level,
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	synchronisationId,
	Text.XHtml.Strict.HTML	teacherId,
	Text.XHtml.Strict.HTML	timeslotId
 )
	=> (locationId -> LocationView.Lesson.Lesson teacherId level -> Data.Course.Course synchronisationId level timeslotId)	-- ^ Find the /course/ to which the specified /lesson/ belongs.
	-> Aggregate.LocationCatalogue.LocationCatalogue locationId campus
	-> Model.Timetable.GenericTimetableToMarkup locationId minimumContrastRatio teacherId timeslotId (Timetable locationId timeslotId teacherId level)
toXHtml findCourseForLocationViewLesson locationCatalogue displaySupplementaryInformation meetingsByTime htmlMergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom	= Text.XHtml.Strict.defList . map (
	\(locationId, timetableForWeek)	-> let
		locationProfile			= locationCatalogue ! locationId
		nUnallocatedAvailableTimeslots	= Model.TimetableForWeek.countUnallocatedAvailableTimeslots locationProfile timetableForWeek
	in (
		Text.XHtml.Strict.unordList (
			(
				Data.Location.getCampus locationProfile +++ ' ' +++ locationId
			) : if displaySupplementaryInformation
				then Data.Maybe.catMaybes [
					Just $ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
						Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier,
						Text.XHtml.Strict.title "The total capacity to accommodate students at this location."
					] << (
						"Capacity" +++ Text.XHtml.Strict.spaceHtml +++ '=' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
						] << Data.Location.getCapacity (locationCatalogue ! locationId)
					),
					if nUnallocatedAvailableTimeslots == 0
						then Nothing
						else Just $ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier,
							Text.XHtml.Strict.title "Available but unallocated time-slots, at this location."
						] << (
							"Unallocated time-slots" +++ Text.XHtml.Strict.spaceHtml +++ '=' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
							] << nUnallocatedAvailableTimeslots
						),
					let
						facilityNames	= Data.Location.getFacilityNames locationProfile
					in if Data.Set.null facilityNames
						then Nothing
						else Just $ Text.XHtml.Strict.unordList (map show $ Data.Set.toList facilityNames) Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier,
							Text.XHtml.Strict.title "Facilities available at this location."
						]
				]
				else []
		) Text.XHtml.Strict.! [Text.XHtml.Strict.theclass Text.CSS.observerSummaryCSSIdentifier],	-- CAVEAT: xhtml only permits inline elements to be nested inside a 'dt' element, & 'ul' is a block element.
		Model.TimetableForWeek.toXHtml (
			findCourseForLocationViewLesson locationId	-- Partially apply.
		) locationProfile (
			Model.Meeting.deleteLocationId locationId meetingsByTime
		) htmlMergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom timetableForWeek
	)
 ) . Data.Map.toList

