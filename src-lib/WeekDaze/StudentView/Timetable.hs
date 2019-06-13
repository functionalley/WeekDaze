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

	* Defines the specific instance of the underyling generic 'Model.Timetable.Timetable', required to define the weekly /timetable/ for all /student/s, i.e. one indexed by 'Aggregate.StudentBody'.

	* This form of the /timetable/ can be re-indexed to form one appropropriate to either /teacher/s or to /location/s,
	but is more significant, since it is from this form that the other two are derived.
-}

module WeekDaze.StudentView.Timetable(
-- * Types
-- ** Type-synonyms
	InterCampusMigrationsByStudentBody,
	LessonRunlengthByStudentBody,
	Timetable,
	StudentClassesByLesson,
	Booking,
-- * Functions
	calculateMeanLocusOperandiOfTeachers,
	calculateWeightedMeanStudentBodyCombinationsPerLesson,
--	extractBookedResourceIdsAt,
	extractStudentClassAt,
--	extractRoutineForStudentBodyBySubject,
	findDistinctLocationIdsFor,
	findDistinctTeacherIdsFor,
	findStudentClassesByLessonFor,
	findStudentClassByTimeByLesson,
	findStudentClassesByLesson,
-- ** Accessors
	getStudentBodies,
-- ** Predicates
	areBookedResourcesAt,
	breaksRoutine,
-- ** Translation
	toXHtml
) where

import			Control.Arrow((&&&), (***))
import			Data.Map((!))
import qualified	Data.Array.IArray
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Data.Tuple
import qualified	Factory.Math.Statistics
import qualified	Text.XHtml.Strict
import qualified	ToolShed.Data.List
import qualified	ToolShed.Data.List.Runlength
import qualified	ToolShed.Data.Pair
import qualified	WeekDaze.Aggregate.StudentBody			as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentBodyRegister		as Aggregate.StudentBodyRegister
import qualified	WeekDaze.Aggregate.StudentClass			as Aggregate.StudentClass
import qualified	WeekDaze.Data.Course				as Data.Course
import qualified	WeekDaze.Data.HumanResource			as Data.HumanResource
import qualified	WeekDaze.Data.Location				as Data.Location
import qualified	WeekDaze.Data.Requirements			as Data.Requirements
import qualified	WeekDaze.Data.Student				as Data.Student
import qualified	WeekDaze.Data.Subject				as Data.Subject
import qualified	WeekDaze.LinearModel.Timetable			as LinearModel.Timetable
import qualified	WeekDaze.LinearModel.TimetableForWeek		as LinearModel.TimetableForWeek
import qualified	WeekDaze.Model.Lesson				as Model.Lesson
import qualified	WeekDaze.Model.Meeting				as Model.Meeting
import qualified	WeekDaze.Model.Timetable			as Model.Timetable
import qualified	WeekDaze.Model.TimetableCoordinates		as Model.TimetableCoordinates
import qualified	WeekDaze.Model.TimetableForWeek			as Model.TimetableForWeek
import qualified	WeekDaze.Size					as Size
import qualified	WeekDaze.StudentView.Lesson			as StudentView.Lesson
import qualified	WeekDaze.StudentView.LessonResourceIds		as StudentView.LessonResourceIds
import qualified	WeekDaze.StudentView.TimetableForWeek		as StudentView.TimetableForWeek
import qualified	WeekDaze.Temporal.Time				as Temporal.Time
import qualified	WeekDaze.Text.CSS				as Text.CSS
import			Text.XHtml.Strict((+++), (<<))
import			WeekDaze.Text.XHTML()

{- |
	* An association-list of the timetables for all /students/.

	* Since /body of students/ will have identical requirements, this is indexed by /body/ rather than an individual student.
-}
type Timetable timeslotId locationId teacherId level	= Model.Timetable.Timetable Aggregate.StudentBody.StudentBody timeslotId (StudentView.LessonResourceIds.LessonResourceIds locationId teacherId) level

-- | Accessor.
getStudentBodies :: Timetable timeslotId locationId teacherId level -> [Aggregate.StudentBody.StudentBody] {-conceptually different from a student-class-}
getStudentBodies	= Data.Map.keys

{- |
	* Identifies the lists of both /locationId/ & /teacherId/, that have been booked in the 'timetable', for the specified /time/.

	* CAVEAT: whilst the booked /resources/ could reasonably be returned as sets, facilitating rapid query,
	the number of resources is typically too small for this approach to be competitive.
-}
extractBookedResourceIdsAt
	:: Data.Array.IArray.Ix timeslotId
	=> Temporal.Time.Time timeslotId
	-> Timetable timeslotId locationId teacherId level
	-> ([locationId], [teacherId])
extractBookedResourceIdsAt time	= foldr (
	\lesson (l, t) -> (: l) . StudentView.LessonResourceIds.getLocationId &&& (: t) . StudentView.LessonResourceIds.getTeacherId $ Model.Lesson.getResourceIds lesson
 ) ([], []) . Model.Timetable.extractSynchronousLessonsAt time

{- |
	* True if either of the /resource/s referenced in the specified /booking/, are already booked at the same /time/ in the /timetable/.

	* CAVEAT: it is assumed that the required /lesson/ hasn't already been defined in the /timetable/, since otherwise both /location/ & /teacher/ would clearly be booked.

	* CAVEAT: performance-hotspot.
-}
areBookedResourcesAt :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			locationId,
	Eq			teacherId
 )
	=> StudentView.TimetableForWeek.Booking timeslotId locationId teacherId level
	-> Timetable timeslotId locationId teacherId level
	-> Bool
areBookedResourcesAt studentViewBooking	= uncurry (||) . (
	elem (StudentView.LessonResourceIds.getLocationId resourceIds) *** elem (StudentView.LessonResourceIds.getTeacherId resourceIds)
 ) . extractBookedResourceIdsAt (Model.TimetableForWeek.getBookedTime studentViewBooking) where
	resourceIds	= Model.Lesson.getResourceIds $ Model.TimetableForWeek.getBookedLesson studentViewBooking

{- |
	* Get the /student-bodies/ who are simultaneously booked to study the same /subject/, at the same /location/, with the same /teacher/.

	* CAVEAT: performance-hotspot.
-}
extractStudentClassAt :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			level,
	Eq			locationId,
	Eq			teacherId
 )
	=> StudentView.TimetableForWeek.Booking timeslotId locationId teacherId level
	-> Timetable timeslotId locationId teacherId level
	-> Aggregate.StudentClass.StudentClass
{-
extractStudentClassAt studentViewBooking	= Data.Map.keysSet . Data.Map.filter (
	== Just (Model.TimetableForWeek.getBookedLesson studentViewBooking)
 ) . Data.Map.map (
	Model.TimetableForWeek.getMaybeLesson $ Model.TimetableForWeek.getBookedTime studentViewBooking
 ) {-extract synchronous events-}	-- CAVEAT: too slow.
-}
extractStudentClassAt studentViewBooking = Data.Map.foldrWithKey (
	\studentBody studentViewTimetableForWeek	-> case Model.TimetableForWeek.getMaybeLesson (Model.TimetableForWeek.getBookedTime studentViewBooking) studentViewTimetableForWeek of
		Just studentViewLesson'
			| studentViewLesson' == Model.TimetableForWeek.getBookedLesson studentViewBooking	-> Data.Set.insert studentBody
			| otherwise										-> id	-- Unequal lesson.
		_												-> id	-- Unallocated timeslot.
 ) Data.Set.empty

{- |
	* Once a /teacher/ has been assigned to teach a /student-body/, in a /subject/, at a /location/, a routine has been established;
	the /student-body/ wouldn't normally be taught that /subject/, by any other /teacher/, or in any other /location/.
	If the routine is adhered to, then each /subject/ maps to a singleton set of /resource-Id/s.

	* CAVEAT: this concept doesn't apply to other views of the /timetable/:
		/teacher/-view;		the same /teacher/ can teach other /student-bodies/, in that /subject/, & at that /location/.
		/location/-view;	the same /location/ can host other /student-bodies/, for tuition of that /subject/, & by that /teacher/.
-}
extractRoutineForStudentBodyBySubject :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> Timetable timeslotId locationId teacherId level	-- ^ The /timetable/.
	-> Aggregate.StudentBody.StudentBody			-- ^ The /student-body/ whose routine is required.
	-> Data.Map.Map (Data.Subject.Subject level) (Data.Set.Set (StudentView.LessonResourceIds.LessonResourceIds locationId teacherId))
extractRoutineForStudentBodyBySubject timetable	= StudentView.TimetableForWeek.extractRoutineBySubject . (timetable !)

{- |
	* True if the specified /lesson/, employs either a different /teacher/, or uses a different /location/, as other /booking/s in the same /subject/.

	* CAVEAT: the routine could include mergers with other /student-bodies/ to form a consistent /student-class/,
	though currently this a soft constraint addressed by both /lesson-criteria/ & /timetable-criteria/.
-}
breaksRoutine :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> Timetable timeslotId locationId teacherId level	-- ^ The current /timetable/.
	-> Aggregate.StudentBody.StudentBody			-- ^ The /student-body/ whose /routine/ to which the new /lesson/ should conform.
	-> StudentView.Lesson.Lesson locationId teacherId level	-- ^ The new /lesson/ to be booked.
	-> Bool
breaksRoutine timetable studentBody	= uncurry (
	Data.Maybe.maybe False {-there's no precedent to break-}
 ) . (
	Data.Set.notMember . Model.Lesson.getResourceIds &&& (`Data.Map.lookup` extractRoutineForStudentBodyBySubject timetable studentBody) . Model.Lesson.getSubject
 )

{- |
	* Find the distinct /locationId/s booked by the referenced /teacher/.

	* CAVEAT: doesn't account for the /location/ of any /meeting/s.
-}
findDistinctLocationIdsFor :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			teacherId,
	Ord			locationId
 )
	=> teacherId
	-> Timetable timeslotId locationId teacherId level
	-> Data.Location.Locus locationId
findDistinctLocationIdsFor teacherId	= foldr (
	Data.Set.insert . StudentView.LessonResourceIds.getLocationId
 ) Data.Set.empty . filter (
	(== teacherId) . StudentView.LessonResourceIds.getTeacherId
 ) . map Model.Lesson.getResourceIds . Model.Timetable.extractLessons

-- | Find the distinct /teacherId/s booked at the referenced /location/.
findDistinctTeacherIdsFor :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			locationId,
	Ord			teacherId
 )
	=> locationId
	-> Timetable timeslotId locationId teacherId level
	-> Data.Set.Set teacherId
findDistinctTeacherIdsFor locationId	= foldr (
	Data.Set.insert . StudentView.LessonResourceIds.getTeacherId
 ) Data.Set.empty . filter (
	(== locationId) . StudentView.LessonResourceIds.getLocationId
 ) . map Model.Lesson.getResourceIds . Model.Timetable.extractLessons

{- |
	* Gets the average number of /location/s visited by /teacher/s.

	* Because only /teacher/s who've at least one /booking/ are included, the result is only less than one if there're zero bookings.
-}
calculateMeanLocusOperandiOfTeachers :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		mean,
	Ord			locationId,
	Ord			teacherId
 ) => Timetable timeslotId locationId teacherId level -> mean
calculateMeanLocusOperandiOfTeachers timetable
	| Data.Map.null distinctLocationsByTeacherId	= 0	-- Zero bookings have been made.
	| otherwise					= Factory.Math.Statistics.getMean $ Data.Map.map Data.Set.size distinctLocationsByTeacherId
	where
		distinctLocationsByTeacherId	= foldr (
			uncurry (Data.Map.insertWith Data.Set.union) . (
				StudentView.LessonResourceIds.getTeacherId &&& Data.Set.singleton . StudentView.LessonResourceIds.getLocationId
			) . Model.Lesson.getResourceIds
		 ) Data.Map.empty $ Model.Timetable.extractLessons timetable

-- | Sets of /student-classes/ indexed by the /lesson/ for which they've been booked.
type StudentClassesByLesson locationId teacherId level	= Data.Map.Map (StudentView.Lesson.Lesson locationId teacherId level) (Data.Set.Set Aggregate.StudentClass.StudentClass)

{- |
	* Because (given sufficient /location/-capacity) one may simultaneously book more than one /student-body/, at a single /location/, taught by a single /teacher/, in a single /subject/,
	the specified /timetable/ may already contain /booking/s formed from a variety of such combinations, each having the specified /student-body/ as a member.
	Since one may reasonably wish to minimise these combinations, this function aims to enumerate distinct /student-class/es per type of /lesson/.

	* For each distinct /lesson/ of which the specified /student-body/ is a member, it identifies the set of /student-class/es already booked.

	* Any set of size greater than one, represents a /student-body combination/ for that /lesson/.
-}
findStudentClassesByLessonFor :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> Timetable timeslotId locationId teacherId level
	-> Aggregate.StudentBody.StudentBody	-- ^ The /student-body/, whose /student-class/-membership one requires.
	-> StudentClassesByLesson locationId teacherId level
findStudentClassesByLessonFor timetable	= foldr (
	\booking -> Data.Map.insertWith Data.Set.union (
		Model.TimetableForWeek.getBookedLesson booking
	) . Data.Set.singleton $ extractStudentClassAt booking timetable
 ) Data.Map.empty . LinearModel.TimetableForWeek.fromTimetableForWeek {-ignore undefined timeslots-} . (timetable !)

{- |
	* Catalogues by /lesson/, the single /student-class/, at each booking-/time/.

	* A /student-body combination/ exists for a specific /lesson/, if within the /booking-time/s identified,
	any /student/ exists in more than one of the distinct /student-class/es;
	a necessary but insufficient prerequisit for which is, that within those /time/s, more than one distinct /student-class/ must exist.
-}
findStudentClassByTimeByLesson :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 ) => Timetable timeslotId locationId teacherId level -> Data.Map.Map (StudentView.Lesson.Lesson locationId teacherId level) (Data.Map.Map (Temporal.Time.Time timeslotId) Aggregate.StudentClass.StudentClass)
findStudentClassByTimeByLesson	= foldr (
	uncurry (
		Data.Map.insertWith $ Data.Map.unionWith Data.Set.union
	) . (
		Model.Timetable.getBookedLesson &&& uncurry Data.Map.singleton . (
			Model.TimetableCoordinates.getTime &&& Data.Set.singleton . Model.TimetableCoordinates.getObserverId
		) . Model.Timetable.getBookedCoordinates
	)
 ) Data.Map.empty . LinearModel.Timetable.fromTimetable

{- |
	* Equivalent to calling 'findStudentClassesByLessonFor' for each /student-body/, & merging the results; but more efficient.

	* A /student-body combination/ exists for a specific /lesson/, if any /student/ exists in more than one of the distinct /student-class/es;
	a necessary but insufficient prerequisit for which is, that more than one distinct /student-class/ must exist.
	E.g. if for a particular /lesson/-type, there are two /booking/s each for either one of two /student-bodies/, then neither will be part of a /student-body combination/,
	but when the results are merged by removing the booking-/time/, the /lesson/ will map to a set of size two.
-}
findStudentClassesByLesson :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 ) => Timetable timeslotId locationId teacherId level -> StudentClassesByLesson locationId teacherId level
findStudentClassesByLesson	= Data.Map.map (
	Data.Set.fromList . Data.Map.elems {-student-classes-}	-- Discard the time.
 ) . findStudentClassByTimeByLesson

{- |
	* Each /student-body/ can be booked for a specific /lesson/ (i.e. to learn a /subject/, taught by a /teacher/, at a /location/),
	by merging with other /student-bodies/ in a variety of combinations, to form a /student-class/.

	* Gets the average number of /student-body/-combinations, over the domain of /student-bodies/ & 'StudentView.Lesson.Lesson's.
	Each number of combinations is weighted by the size of the /student-body/, in the average.
-}
calculateWeightedMeanStudentBodyCombinationsPerLesson :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		weightedMean,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 ) => Timetable timeslotId locationId teacherId level -> weightedMean
calculateWeightedMeanStudentBodyCombinationsPerLesson timetable
	| denominator == 0	= 1	-- (zero / zero) -> 1, at the limit of only one lesson-definition in the timetable.
	| otherwise		= fromIntegral numerator / fromIntegral denominator
	where
		(numerator, denominator)	= Data.Map.foldr (
			\studentClassSet pair	-> if Data.Set.size studentClassSet == 1	-- Trap the most common case, which can be addressed more efficiently.
				then ToolShed.Data.Pair.mirror (
					+ Aggregate.StudentClass.getSize (Data.Set.findMin studentClassSet)
				) pair	-- All student-bodies must be a member of the only student-class.
				else Data.Set.foldr (
					\studentBody -> let
						(memberOfNStudentClasses, weight)	= Data.Set.size . (
							`Data.Set.filter` studentClassSet	-- Select those student-classes of which this student-body is a member.
						 ) . Data.Set.member &&& Aggregate.StudentBody.getSize $ studentBody
					in (+ memberOfNStudentClasses * weight) *** (+ weight)
				) pair . Data.Set.unions $ Data.Set.elems studentClassSet	-- Consider each distinct student-body in turn.
		 ) (0, 0) $ findStudentClassesByLesson timetable

-- | The type of a map passed to 'toXHtml'.
type InterCampusMigrationsByStudentBody	= Data.Map.Map Aggregate.StudentBody.StudentBody Size.NTimeslots

-- | The type of a map passed to 'toXHtml'.
type LessonRunlengthByStudentBody timeslotId locationId teacherId level	= Data.Map.Map Aggregate.StudentBody.StudentBody [(Temporal.Time.Time timeslotId, ToolShed.Data.List.Runlength.Code (StudentView.Lesson.Lesson locationId teacherId level))]

{- |
	* Render in /XHTML/, as a /definition-list/.

	* CAVEAT: the extra parameters make it difficult to express this as an instance of 'Text.XHtml.Strict.HTML'.
-}
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
	Show			stream,
	Show			teacherId,
	Text.XHtml.Strict.HTML	level,
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	stream,
	Text.XHtml.Strict.HTML	synchronisationId,
	Text.XHtml.Strict.HTML	teacherId,
	Text.XHtml.Strict.HTML	timeslotId
 )
	=> InterCampusMigrationsByStudentBody											-- ^ The number of inter-/campus/ /student/-migrations, indexed by /student-body/.
	-> Data.Requirements.Requirements (Aggregate.StudentBodyRegister.KnowledgeByStudentBody level)				-- ^ The unbooked /knowledge-requirements/ by student-body, partitioned into core & optional /subject/s.
	-> LessonRunlengthByStudentBody timeslotId locationId teacherId level							-- ^ Excessively long subject-sessions.
	-> LessonRunlengthByStudentBody timeslotId locationId teacherId level							-- ^ Short subject-sessions.
	-> Size.NTimeslots													-- ^ The number of /time-slot/s per /day/.
	-> (StudentView.Lesson.Lesson locationId teacherId level -> Data.Course.Course synchronisationId level timeslotId)	-- ^ Find the /course/ to which the specified /lesson/ belongs.
	-> Aggregate.StudentBodyRegister.StudentBodyRegister level stream teachingRatio
	-> Model.Timetable.GenericTimetableToMarkup locationId minimumContrastRatio teacherId timeslotId (Timetable timeslotId locationId teacherId level)
toXHtml interCampusMigrationsByStudentBody unbookedKnowledgeRequirementsByStudentBody excessivelyLongSessionsByStudentBody shortSessionsByStudentBody nTimeslotsPerDay findCourseFor studentBodyRegister displaySupplementaryInformation meetingsByTime htmlMergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom timetable	= Text.XHtml.Strict.defList . map (
	\(studentBody, timetableForWeek)	-> let
		(studentProfile, (excessivelyLongSessions, shortSessions))	= (studentBodyRegister !) &&& ((excessivelyLongSessionsByStudentBody !) &&& (shortSessionsByStudentBody !)) $ studentBody	-- De-reference.
	in (
		Text.XHtml.Strict.unordList (
			Text.XHtml.Strict.toHtml studentBody : if displaySupplementaryInformation
				then Data.Maybe.catMaybes [
					let
						nInterCampusMigrations	= interCampusMigrationsByStudentBody ! studentBody
					in if nInterCampusMigrations == 0
						then Nothing
						else Just $ Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "The number of inter-campus migrations required by these students."
						] << (
							"Inter-campus migrations" +++ Text.XHtml.Strict.spaceHtml +++ '=' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
							] << nInterCampusMigrations
						),
					let
						stream	= Data.Student.getStream studentProfile
					in if any ($ show stream) [null, (== "\"\"")]	-- The string-representation is uncertain, since 'stream' is polymorphic.
						then Nothing
						else Just $ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier,
							Text.XHtml.Strict.title "The year/stream for this student-body."
						] << (
							"Stream" +++ Text.XHtml.Strict.spaceHtml +++ '=' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.dataCSSIdentifier
							] << stream
						),
					(
						(
							Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
								Text.XHtml.Strict.title "Any core knowledge-requirements for this student-body, which are incompletely booked."
							] <<
						) . (
							"Incomplete core knowledge-requirements" +++
						) . (
							Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.dataCSSIdentifier
							] <<
						) . Text.XHtml.Strict.unordList . Data.Set.toAscList
					) `fmap` Data.Map.lookup studentBody (
						Data.Requirements.getCore unbookedKnowledgeRequirementsByStudentBody
					),
					(
						(
							Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
								Text.XHtml.Strict.title "Any optional knowledge-requirements for this student-body, which are incompletely booked."
							] <<
						) . (
							"Incomplete optional knowledge-requirements:" +++
						) . (
							Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.dataCSSIdentifier
							] <<
						) . Text.XHtml.Strict.unordList . Data.Set.toAscList
					) `fmap` Data.Map.lookup studentBody (
						Data.Requirements.getOptional unbookedKnowledgeRequirementsByStudentBody
					),
					Just $ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
						Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier,
						Text.XHtml.Strict.title "Time allocated to teaching / time available for teaching."
					] << (
						"Utilisation-ratio" +++ Text.XHtml.Strict.spaceHtml +++ '=' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
						] << (
							round $ 100 * (
								Model.TimetableForWeek.calculateUtilisationRatio nTimeslotsPerDay studentProfile timetableForWeek ::Rational
							) :: Int
						) +++ '%'
					),
					let
						nFreePeriods :: Size.NTimeslots
						nFreePeriods	= Model.TimetableForWeek.countUnallocatedAvailableTimeslots studentProfile timetableForWeek - Data.HumanResource.getNTimeslotsPerWeekOfNonTeaching nTimeslotsPerDay studentProfile {-which includes the time for meetings-}
					in if nFreePeriods == 0
						then Nothing
						else Just $ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "The number of available but unallocated time-slots, excluding those allocated to free study & meetings, multiplied by the size of the student-body."
						] << (
							"Free-periods" +++ Text.XHtml.Strict.spaceHtml +++ '=' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
							] << nFreePeriods +++ Text.XHtml.Strict.primHtmlChar "times" +++ Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
								Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
							] << Aggregate.StudentBody.getSize studentBody
						),
					let
						separatedEqualSubjectLessons	= Model.TimetableForWeek.findSeparatedEqualSubjectLessonsWithinAnyDay timetableForWeek
					in if null separatedEqualSubjectLessons
						then Nothing
						else Just $ Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "The number of instances of separated sessions in the same subject, in any single day."
						] << (
							"Split sessions:" +++ Text.XHtml.Strict.unordList (
								map (
									uncurry (+++) . Data.Tuple.swap . (
										(
											((Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.primHtmlChar "Implies" +++ Text.XHtml.Strict.spaceHtml) +++) . Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
												Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
											] <<
										) . pred {-count those exceeding one-} *** Model.Lesson.getSubject
									)
								) separatedEqualSubjectLessons
							)
						),
					if null excessivelyLongSessions
						then Nothing
						else Just $ Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "Some subjects have been booked in longer sessions than necessary (and therefore also longer than requested)."
						] << (
							"Long sessions:" +++ (
								Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
									Text.XHtml.Strict.theclass Text.CSS.dataCSSIdentifier
								] << Text.XHtml.Strict.unordList (
									ToolShed.Data.List.nub' $ map (
										Model.Lesson.getSubject . ToolShed.Data.List.Runlength.getDatum . snd {-runlength-code-}
									) excessivelyLongSessions
								)
							)
						),
					if null shortSessions
						then Nothing
						else Just $ Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "Some subjects have been booked in shorter sessions than requested."
						] << (
							"Short sessions:" +++ (
								Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
									Text.XHtml.Strict.theclass Text.CSS.dataCSSIdentifier
								] << Text.XHtml.Strict.unordList (
									ToolShed.Data.List.nub' $ map (
										Model.Lesson.getSubject . ToolShed.Data.List.Runlength.getDatum . snd {-runlength-code-}
									) shortSessions
								)
							)
						),
					let
						studentBodyCombinationsByLesson	= Data.Map.filter ((> 1) . Data.Set.size) $ findStudentClassesByLessonFor timetable studentBody
					in if Data.Map.null studentBodyCombinationsByLesson
						then Nothing
						else Just $ Text.XHtml.Strict.thediv Text.XHtml.Strict.! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
							Text.XHtml.Strict.title "Those subjects studied with different combinations of student-bodies."
						] << (
							"Student-body combinations:" +++ Text.XHtml.Strict.unordList (
								map (
									uncurry (+++) . (
										Model.Lesson.getSubject *** (
											((Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.primHtmlChar "Implies" +++ Text.XHtml.Strict.spaceHtml) +++) . Text.XHtml.Strict.thespan Text.XHtml.Strict.! [
												Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier,
												Text.XHtml.Strict.title "Number of combinations."
											] <<
										) . Data.Set.size
									)
								) $ Data.Map.toList studentBodyCombinationsByLesson
							)
						)
				]
				else []
		) Text.XHtml.Strict.! [Text.XHtml.Strict.theclass Text.CSS.observerSummaryCSSIdentifier],	-- CAVEAT: xhtml only permits inline elements to be nested inside a 'dt' element, & 'ul' is a block element.
		Model.TimetableForWeek.toXHtml findCourseFor studentProfile (Model.Meeting.deleteStudentBody studentBody meetingsByTime) htmlMergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom timetableForWeek
	) -- Pair.
 ) $ Data.Map.toList timetable

-- | A /lesson/ qualified by the /coordinates/ at which it is booked.
type Booking timeslotId locationId teacherId level	= Model.Timetable.Booking Aggregate.StudentBody.StudentBody timeslotId (StudentView.LessonResourceIds.LessonResourceIds locationId teacherId) level
