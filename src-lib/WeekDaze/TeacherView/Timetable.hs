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

	* Defines the /timetable/ for any week, for all /teacher/s.

	* It contains identical information to 'StudentView.Timetable.Timetable',
	but the data has been re-indexed to present it in a form relevant to a /teacher/ rather than a /student/.
-}

module WeekDaze.TeacherView.Timetable(
-- * Types
-- ** Type-synonyms
	Timetable,
--	Booking,
	InterCampusMigrationsByTeacherId,
	TimesByTeacherId,
-- * Functions
	findStudentClassesByLocationId,
	calculateMeanStudentClassSize,
	countLocationChangesByTeacherId,
-- ** Mutators
	bookStudentViewLesson,
--	bookTeacherViewLesson,
-- ** Translation
	fromStudentViewTimetable,
	toStudentViewTimetable,
	toXHtml
) where

import			Control.Arrow((&&&))
import			Data.Map((!))
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Data.Tuple
import qualified	Factory.Math.Statistics
import qualified	Text.XHtml.Strict
import qualified	WeekDaze.Aggregate.StudentClass		as Aggregate.StudentClass
import qualified	WeekDaze.Aggregate.TeacherRegister	as Aggregate.TeacherRegister
import qualified	WeekDaze.Data.Course			as Data.Course
import qualified	WeekDaze.Data.HumanResource		as Data.HumanResource
import qualified	WeekDaze.Model.Lesson			as Model.Lesson
import qualified	WeekDaze.Model.Meeting			as Model.Meeting
import qualified	WeekDaze.Model.Timetable		as Model.Timetable
import qualified	WeekDaze.Model.TimetableForWeek		as Model.TimetableForWeek
import qualified	WeekDaze.Size				as Size
import qualified	WeekDaze.StudentView.LessonResourceIds	as StudentView.LessonResourceIds
import qualified	WeekDaze.StudentView.Timetable		as StudentView.Timetable
import qualified	WeekDaze.TeacherView.Lesson		as TeacherView.Lesson
import qualified	WeekDaze.TeacherView.LessonResourceIds	as TeacherView.LessonResourceIds
import qualified	WeekDaze.TeacherView.TimetableForWeek	as TeacherView.TimetableForWeek
import qualified	WeekDaze.Temporal.Time			as Temporal.Time
import qualified	WeekDaze.Text.CSS			as Text.CSS
import			Text.XHtml.Strict((+++), (<<))
import			WeekDaze.Text.XHTML()

-- | An association-list of the timetables for all /teachers/.
type Timetable teacherId timeslotId locationId level	= Model.Timetable.Timetable teacherId timeslotId (TeacherView.LessonResourceIds.LessonResourceIds locationId) level

{- |
	* Builds a 'Timetable' by inverting a 'StudentView.Timetable.Timetable'.

	* CAVEAT: in the original 'StudentView.Timetable.Timetable', different /student-bodies/ may simultaneously reference a single /teacher/.
	This is normal behaviour, provided the /teacher/ can cope with the class-size,
	& is implemented seemlessly, as a /map/-structure with one or more /key/s referencing a /value/, containing the same /teacher/.

	* When inverting such a /timetable/, this many-to-one relationship becomes a one-to-many, & a single /teacher-id/ must now reference many /student-bodies/.
	The simple /map/-structure, unlike a /multi-map/, can't directly accommodate multiple identical keys, with different values,
	so this is achieved by merging /student-bodies/ into /student-class/es.
-}
fromStudentViewTimetable :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			teacherId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> Bool							-- ^ Permit a temporary /student-body/ merger.
	-> Timetable teacherId timeslotId locationId level	-- ^ An unallocated /timetable/, supplied for efficiency.
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> Timetable teacherId timeslotId locationId level
fromStudentViewTimetable permitTemporaryStudentBodyMerger	= Data.Map.foldrWithKey (
	\studentBody studentViewTimetableForWeek teacherViewTimetable	-> snd {-timetable-} $ Data.Foldable.foldr (
		\studentViewTimetableForDay (day, teacherViewTimetable')	-> (
			pred day,	-- Because of the use of 'foldr'.
			snd {-timetable-} $ Data.Foldable.foldr (
				\maybeStudentViewLesson (timeslotId, teacherViewTimetable'')	-> (
					pred timeslotId,	-- Because of the use of 'foldr'.
					Data.Maybe.maybe teacherViewTimetable'' (
						(
							$ teacherViewTimetable''
						) . bookStudentViewLesson permitTemporaryStudentBodyMerger {-partially apply-} . (,) (
							studentBody,
							Temporal.Time.mkTime day timeslotId
						) {-construct a booking-}
					) maybeStudentViewLesson
				)
			) (
				snd {-maximum timeslotId-} $ Data.Array.IArray.bounds studentViewTimetableForDay,
				teacherViewTimetable'
			) studentViewTimetableForDay
		) {-CAVEAT: performance-hotspot-}
	) (
		snd {-maximum day-} $ Data.Array.IArray.bounds studentViewTimetableForWeek,
		teacherViewTimetable
	) studentViewTimetableForWeek
 )

{- |
	* Build a 'StudentView.Timetable.Timetable', by inverting a 'Timetable'.

	* Each /lesson/ in the /timetable/ seen from the /teacher/'s perspective, contains a /student-class/.
	This /student-class/ must be broken into /student-bodies/, before inserting repeatedly into the required /timetable/ as seen from the /student-body/'s perpective.
-}
toStudentViewTimetable
	:: (Data.Array.IArray.Ix timeslotId, Enum timeslotId)
	=> StudentView.Timetable.Timetable timeslotId locationId teacherId level	-- ^ An unallocated /timetable/, supplied for efficiency.
	-> Timetable teacherId timeslotId locationId level
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
toStudentViewTimetable	= Data.Map.foldrWithKey (
	\teacherId teacherViewTimetableForWeek studentViewTimetable	-> snd {-timetable-} $ Data.Foldable.foldr (
		\teacherViewTimetableForDay (day, studentViewTimetable')	-> (
			pred day,	-- Because of the use of 'foldr'.
			snd {-timetable-} $ Data.Foldable.foldr (
				\maybeTeacherViewLesson (timeslotId, studentViewTimetable'')	-> (
					pred timeslotId,	-- Because of the use of 'foldr'.
					Data.Maybe.maybe studentViewTimetable'' (
						(
							$ studentViewTimetable''
						) . bookTeacherViewLesson {-partially apply-} . (,) (
							teacherId,
							Temporal.Time.mkTime day timeslotId
						) {-construct a booking-}
					) maybeTeacherViewLesson
				)
			) (
				snd {-maximum timeslotId-} $ Data.Array.IArray.bounds teacherViewTimetableForDay,
				studentViewTimetable'
			) teacherViewTimetableForDay
		)
	) (
		snd {-maximum day-} $ Data.Array.IArray.bounds teacherViewTimetableForWeek,
		studentViewTimetable
	) teacherViewTimetableForWeek
 )

-- | Apply a 'StudentView.Timetable.Booking' to a /TeacherView-timetable/.
bookStudentViewLesson :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			locationId,
	Eq			level,
	Ord			teacherId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> Bool									-- ^ Permit a temporary /student-body/ merger.
	-> StudentView.Timetable.Booking timeslotId locationId teacherId level	-- ^ The /coordinates/ & /lesson/ from the perspective of the /student/-view of the /timetable/.
	-> Timetable teacherId timeslotId locationId level			-- ^ The /timetable/ as seen from the /teacher/'s perspective.
	-> Timetable teacherId timeslotId locationId level
bookStudentViewLesson permitTemporaryStudentBodyMerger ((studentBody, time), studentViewLesson) teacherViewTimetable	= Model.Timetable.defineTimeslot (
	teacherViewTimetableCoordinates,
	Just studentViewLesson {
		Model.Lesson.getResourceIds	= TeacherView.LessonResourceIds.mkLessonResourceIds locationId . Data.Set.insert studentBody $ case Model.Timetable.getMaybeLesson teacherViewTimetableCoordinates teacherViewTimetable of
			Nothing	-> Data.Set.empty	-- Start a new student-class.
			Just teacherViewLesson
				| and [
					permitTemporaryStudentBodyMerger,
					TeacherView.LessonResourceIds.getLocationId teacherViewResourceIds == locationId,
					Model.Lesson.getSubject teacherViewLesson == Model.Lesson.getSubject studentViewLesson
				]		-> TeacherView.LessonResourceIds.getStudentClass teacherViewResourceIds
				| otherwise	-> error $ "WeekDaze.TeacherView.Timetable.bookStudentViewLesson:\tlesson at coordinates=" ++ show teacherViewTimetableCoordinates ++ " has already been booked in either a different subject or location; " ++ show (studentViewLesson, teacherViewLesson) ++ "."
				where
					teacherViewResourceIds	= Model.Lesson.getResourceIds teacherViewLesson
	} -- Mutate.
 ) teacherViewTimetable where
	studentViewResourceIds		= Model.Lesson.getResourceIds studentViewLesson
	locationId			= StudentView.LessonResourceIds.getLocationId studentViewResourceIds
	teacherViewTimetableCoordinates	= (StudentView.LessonResourceIds.getTeacherId studentViewResourceIds, time)

-- | Catalogue the /student-class/es booked in the specified /timetable/, indexed by /location-Id/.
findStudentClassesByLocationId
	:: (Data.Array.IArray.Ix timeslotId, Ord locationId)
	=> Timetable teacherId timeslotId locationId level
	-> Data.Map.Map locationId [Aggregate.StudentClass.StudentClass]
findStudentClassesByLocationId	= Data.Map.fromListWith (++) . map (
	(TeacherView.LessonResourceIds.getLocationId &&& return {-to List-monad-} . TeacherView.LessonResourceIds.getStudentClass) . Model.Lesson.getResourceIds
 ) . Model.Timetable.extractLessons

-- | Calculates the mean over all /booking/s, of the size of the /student-class/.
calculateMeanStudentClassSize :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		meanValue
 ) => Timetable teacherId timeslotId locationId level -> meanValue
calculateMeanStudentClassSize teacherViewTimetable
	| null lessons	= 0	-- The timetable is completely unpopulated.
	| otherwise	= Factory.Math.Statistics.getMean $ map (
		Aggregate.StudentClass.getSize . TeacherView.LessonResourceIds.getStudentClass . Model.Lesson.getResourceIds
	) lessons
	where
		lessons	= Model.Timetable.extractLessons teacherViewTimetable

{- |
	* Counts the /location/-changes made by each /teacher/.

	* The minimum value of zero can be achieved either by a completely unbooked /timetable/,
	or one completely booked by the /lesson/s in the same /location/.

	* CAVEAT: doesn't account for the /location/ of any /meeting/s.
-}
countLocationChangesByTeacherId :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			locationId
 ) => Timetable teacherId timeslotId locationId level -> Data.Map.Map teacherId Int
countLocationChangesByTeacherId	= Data.Map.map TeacherView.TimetableForWeek.countLocationChanges

-- | A /lesson/ qualified by the /coordinates/ at which it is booked.
type Booking teacherId timeslotId locationId level	= Model.Timetable.Booking teacherId timeslotId (TeacherView.LessonResourceIds.LessonResourceIds locationId) level

-- | Books a /TeacherView-booking/ in a /StudentView-timetable/.
bookTeacherViewLesson
	:: Data.Array.IArray.Ix timeslotId
	=> Booking teacherId timeslotId locationId level				-- ^ The /coordinates/ & /lesson/ from the perspective of the /teacher/-view of the /timetable/.
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level	-- ^ The /timetable/ as seen from the /student/'s perspective.
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
bookTeacherViewLesson ((teacherId, time), teacherViewLesson) studentViewTimetable	= Data.Set.foldr (
	Model.Timetable.defineTimeslot . flip (,) (
		Just $ TeacherView.Lesson.toStudentView teacherId teacherViewLesson
	) . flip (,) time
 ) studentViewTimetable $ TeacherView.LessonResourceIds.getStudentClass teacherViewResourceIds where
	teacherViewResourceIds	= Model.Lesson.getResourceIds teacherViewLesson

-- | The type of a map passed to 'toXHtml'.
type InterCampusMigrationsByTeacherId teacherId	= Data.Map.Map teacherId Size.NTimeslots

-- | A map indexed by /teacherId/, of sets of /times/.
type TimesByTeacherId teacherId timeslotId	= Data.Map.Map teacherId (Temporal.Time.TimeSet timeslotId)

-- | Render in /XHTML/, as a /definition-list/.
toXHtml :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		minimumContrastRatio,
	Ord			level,
	Ord			locationId,
	Ord			minimumContrastRatio,
	Ord			teacherId,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Text.XHtml.Strict.HTML	level,
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	synchronisationId,
	Text.XHtml.Strict.HTML	teacherId,
	Text.XHtml.Strict.HTML	timeslotId
 )
	=> InterCampusMigrationsByTeacherId teacherId										-- ^ The number of inter-/campus/ /teacher/-migrations, indexed by /teacherId/.
	-> TimesByTeacherId teacherId timeslotId										-- ^ Unbooked but specified times, by /teacherId/.
	-> Size.NTimeslots													-- ^ The number of /time-slot/s per /day/.
	-> (teacherId -> TeacherView.Lesson.Lesson locationId level -> Data.Course.Course synchronisationId level timeslotId)	-- ^ Find the /course/ to which the specified /lesson/ belongs.
	-> Aggregate.TeacherRegister.TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio
	-> Model.Timetable.GenericTimetableToMarkup locationId minimumContrastRatio teacherId timeslotId (Timetable teacherId timeslotId locationId level)
toXHtml interCampusMigrationsByTeacherId unbookedSpecifiedTimesByTeacherId nTimeslotsPerDay findCourseFor teacherRegister displaySupplementaryInformation meetingsByTime htmlMergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom	= Text.XHtml.Strict.defList . map (
	\(teacherId, timetableForWeek)	-> let
		teacherProfile	= teacherRegister ! teacherId
	in (
		Text.XHtml.Strict.unordList (
			Text.XHtml.Strict.toHtml teacherId : if displaySupplementaryInformation
				then Data.Maybe.catMaybes [
					let
						nInterCampusMigrations	= interCampusMigrationsByTeacherId ! teacherId
					in if nInterCampusMigrations == 0
						then Nothing
						else Just $ Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "The number of inter-campus migrations required of this teacher."
						] << (
							"Inter-campus migrations" +++ Text.XHtml.Strict.spaceHtml +++ '=' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
							] << nInterCampusMigrations
						),
					if Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay teacherProfile == 0
						then Nothing
						else Just $ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier,
							Text.XHtml.Strict.title "Time allocated to teaching / time available for teaching."
						] << (
							"Utilisation-ratio" +++ Text.XHtml.Strict.spaceHtml +++ '=' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
							] << (
								round $ 100 * (
									Model.TimetableForWeek.calculateUtilisationRatio nTimeslotsPerDay teacherProfile timetableForWeek ::Rational
								) :: Int
							) +++ '%'
						),
					let
						nFreePeriods	= Model.TimetableForWeek.countUnallocatedAvailableTimeslots teacherProfile timetableForWeek - Data.HumanResource.getNTimeslotsPerWeekOfNonTeaching nTimeslotsPerDay teacherProfile {-which includes the time for meetings-}
					in if nFreePeriods == 0
						then Nothing
						else Just $ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "The number of available but unallocated time-slots, excluding those allocated to administration & meetings."
						] << (
							"Free-periods" +++ Text.XHtml.Strict.spaceHtml +++ '=' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
							] << nFreePeriods
						),
					let
						separatedEqualLessons	= Model.TimetableForWeek.findSeparatedEqualLessonsWithinAnyDay timetableForWeek
					in if null separatedEqualLessons
						then Nothing
						else Just $ Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "The number of instances of separated sessions of identical lessons, in any single day."
						] << (
							"Split sessions:" +++ Text.XHtml.Strict.defList (
								map (
									Data.Tuple.swap . Control.Arrow.first (
										(
											Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
												Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
											] <<
										) . pred {-count those exceeding one-}
									)
								) separatedEqualLessons
							)
						),
					let
						unbookedSpecifiedTimes	= unbookedSpecifiedTimesByTeacherId ! teacherId
					in if Data.Set.null unbookedSpecifiedTimes
						then Nothing
						else Just $ Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "Times specified by courses, which haven't been booked, for courses which have."
						] << (
							"Unbooked times:" +++ (
								Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
									Text.XHtml.Strict.theclass Text.CSS.dataCSSIdentifier
								] << Text.XHtml.Strict.unordList (
									Data.Set.toAscList unbookedSpecifiedTimes
								)
							)
						)
				]
				else []
		) Text.XHtml.Strict.! [Text.XHtml.Strict.theclass Text.CSS.observerSummaryCSSIdentifier],	-- CAVEAT: xhtml only permits inline elements to be nested inside a 'dt' element, & 'ul' is a block element.
		Model.TimetableForWeek.toXHtml (
			findCourseFor teacherId	-- Partially apply.
		) teacherProfile (
			Model.Meeting.deleteTeacherId teacherId meetingsByTime
		) htmlMergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom timetableForWeek
	) -- Pair.
 ) . Data.Map.toList

