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

	Contains utilities dependent on both the static data derived from either configuration or command-line options,
	& on the dynamic state of the current 'StudentView.Timetable.Timetable'.
-}

module WeekDaze.Dynamic.StudentViewTimetableUtilities(
-- * Types
-- ** Type-synonyms
	BookAtomically,
-- * Functions
	calculateAverageAbsoluteDeviationFromIdealTimeslotRequest,
--	calculateRemainingPlaces,
--	calculateUtilisationRatioByStudentBody,
	calculateWeightedMeanUtilisationRatio,
	countFreeLessons,
	countInterCampusMigrations,
	countInterCampusMigrationsByStudentBody,
	findMeanRatioOfIncompletelyBookedKnowledgeRequirements,
	findIncompletelyBookedKnowledgeRequirementsByStudentBody,
	calculateMeanFreePeriodCompliance,
	findExcessiveLessonRunlengthsByTimeByStudentBody,
	findShortLessonRunlengthsByTimeByStudentBody,
	locateLessonsBySynchronisationId,
	locateUnallocatedAvailableUnreservedCoordinates,
-- ** Mutators
	bookAtomicallyNothing,
--	bookAtomicallyMinimumConsecutiveLessons,
	bookAtomicallyWithRamifications,
-- ** Predicates
	isSuitableBooking,
	isTimetableValid
) where

import			Control.Arrow((&&&), (***))
import			Data.Map((!))
import			Data.Set((\\))
import qualified	Control.Arrow
import qualified	Data.Array.Base
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	Data.Set
import qualified	Data.Tuple
import qualified	Factory.Data.Interval
import qualified	Factory.Math.Statistics
import qualified	ToolShed.Data.List.Runlength
import qualified	ToolShed.Data.Pair
import qualified	ToolShed.Data.Triple
import qualified	WeekDaze.Aggregate.GroupCatalogue				as Aggregate.GroupCatalogue
import qualified	WeekDaze.Aggregate.StudentBody					as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentClass					as Aggregate.StudentClass
import qualified	WeekDaze.Data.Course						as Data.Course
import qualified	WeekDaze.Data.HumanResource					as Data.HumanResource
import qualified	WeekDaze.Data.Location						as Data.Location
import qualified	WeekDaze.Data.Requirements					as Data.Requirements
import qualified	WeekDaze.Data.Resource						as Data.Resource
import qualified	WeekDaze.Data.Student						as Data.Student
import qualified	WeekDaze.Dynamic.HumanViewTimetable				as Dynamic.HumanViewTimetable
import qualified	WeekDaze.Dynamic.StudentViewTimetableForWeekUtilities		as Dynamic.StudentViewTimetableForWeekUtilities
import qualified	WeekDaze.Dynamic.TeacherViewTimetableForWeekUtilities		as Dynamic.TeacherViewTimetableForWeekUtilities
import qualified	WeekDaze.Dynamic.TeacherViewTimetableUtilities			as Dynamic.TeacherViewTimetableUtilities
import qualified	WeekDaze.Dynamic.TimetableForWeekUtilities			as Dynamic.TimetableForWeekUtilities
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions		as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.LinearModel.Timetable					as LinearModel.Timetable
import qualified	WeekDaze.Model.Lesson						as Model.Lesson
import qualified	WeekDaze.Model.ResourceUser					as Model.ResourceUser
import qualified	WeekDaze.Model.Timetable					as Model.Timetable
import qualified	WeekDaze.Model.TimetableCoordinates				as Model.TimetableCoordinates
import qualified	WeekDaze.Model.TimetableForDay					as Model.TimetableForDay
import qualified	WeekDaze.Model.TimetableForWeek					as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis			as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters			as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.ProblemConfiguration.TimetableValidationSwitches	as ProblemConfiguration.TimetableValidationSwitches
import qualified	WeekDaze.Size							as Size
import qualified	WeekDaze.StudentView.LessonResourceIds				as StudentView.LessonResourceIds
import qualified	WeekDaze.StudentView.Timetable					as StudentView.Timetable
import qualified	WeekDaze.StudentView.TimetableCoordinates			as StudentView.TimetableCoordinates
import qualified	WeekDaze.StudentView.TimetableForWeek				as StudentView.TimetableForWeek
import qualified	WeekDaze.TeacherView.Timetable					as TeacherView.Timetable
import qualified	WeekDaze.Temporal.Day						as Temporal.Day
import qualified	WeekDaze.Temporal.Time						as Temporal.Time
import qualified	WeekDaze.Temporal.TimeslotRequest				as Temporal.TimeslotRequest

{- |
	* Determines the remainder of the /knowledge-requirement/, of each /student-body/, yet to be booked (including ssubjects which haven't been booked at all).

	* CAVEAT: any element of the map returned, may be 'Data.Set.empty'.
-}
findIncompletelyBookedKnowledgeRequirementsByStudentBody :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			locationId,
	Ord			level,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> Data.Map.Map Aggregate.StudentBody.StudentBody (Data.Student.KnowledgeRequirements level)
findIncompletelyBookedKnowledgeRequirementsByStudentBody problemParameters timetable	= Data.Map.mapWithKey (
	\studentBody -> Dynamic.StudentViewTimetableForWeekUtilities.findIncompletelyBookedKnowledgeRequirementsFor problemParameters (timetable ! studentBody)
 ) $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters

{- |
	* Gets the average (weighted by /student-body/-size) over all /student-bodies/,
	of the ratio of core & optional /subject/s which are incompletely booked, to all /subject/-requirements.

	* CAVEAT: this calculation values all incomplelely booked /course/s equally,
	rather than preferring partially booked ones to completely unbooked ones.
	This dubious decision, is forced because of the difficulty in finding the extent to which a completely unbooked /knowledge-requirement/ falls short of the /course/'s /requiredLessonsPerWeek/,
	when the /course/ which satisfies this /knowledge-requirement/ hasn't yet been defined.

	* CAVEAT: the ratio of both missing core & missing optional /subject/s, is obtained by dividing by the total number of /subject/s required,
	since were core /subject/s normalised wrt to the total number of core /subject/s required,
	& optional /subject/s normalised wrt the total number of optional /subject/s required,
	one of these denominators could be zero (resulting in infinity), but the sum can't.
-}
findMeanRatioOfIncompletelyBookedKnowledgeRequirements :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		f,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> Data.Requirements.Requirements f
findMeanRatioOfIncompletelyBookedKnowledgeRequirements problemParameters problemAnalysis timetable
	| nStudents == 0	= error "WeekDaze.Dynamic.StudentViewTimetableUtilities.findMeanRatioOfIncompletelyBookedKnowledgeRequirements:\tattempt to divide by zero (students)."
	| otherwise		= ToolShed.Data.Pair.mirror (
		(
			/ fromIntegral nStudents	-- Normalise wrt the total number of students.
		) . sum
	) . unzip . map (
		\(studentBody, studentProfile)	-> let
			nSubjectRequirements	= Data.Set.size $ Data.Student.deriveAmalgamatedKnowledgeRequirement studentProfile
		in if nSubjectRequirements == 0
			then error "WeekDaze.Dynamic.StudentViewTimetableUtilities.findMeanRatioOfIncompletelyBookedKnowledgeRequirements:\tattempt to divide by zero (subject-requirements)."
			else ToolShed.Data.Pair.mirror (
				(
					/ fromIntegral nSubjectRequirements	-- Normalise wrt the size of the amalgamated knowledge-requirement; which can't be zero.
				) . fromIntegral . (
					* Aggregate.StudentBody.getSize studentBody	-- Each student in the student-body has the same missing subject-requirement.
				) . Data.Set.size
			) $ Dynamic.StudentViewTimetableForWeekUtilities.findIncompletelyBookedKnowledgeRequirementsFor problemParameters (timetable ! studentBody) studentProfile
	) . Data.Map.toList $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters
	where
		nStudents	= ProblemConfiguration.ProblemAnalysis.getNStudents problemAnalysis

{- |
	* Determines whether the specified /lesson/ can be booked in the /timetable/, at the specified /coordinates/.

	* Observes hard constraints; soft constraints are dealt with elsewhere, by measuring /lesson-criteria/.

	* CAVEAT: the caller should ensure that the the requested /lesson/ hasn't already been booked, since otherwise the required /resource/s will appear to be booked & 'False' will be returned.

	* CAVEAT: doesn't check whether there's already a /lesson/ booked at the specified coordinates, which may be overwritten,
	potentially disasterously if the overwritten /lesson/ had either a non-trivial /minimumConsecutiveLessons/ or was a member of a /synchronised course/.

	* CAVEAT: assumes that the /resource/s required for the proposed /lesson/, are regularly /available/ on the specified /day/;
	since this can be statically determined, it's more efficient to check externally.

	* CAVEAT: the order in which the checks are performed, significantly affects performance.
-}
isSuitableBooking :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			stream,
	Ord			teacherId,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level	-- ^ The /student/-view & /teacher/-view of the same /timetable/, prior to the proposed /booking/.
	-> StudentView.Timetable.Booking timeslotId locationId teacherId level			-- ^ The proposed /lesson/ & the /coordinates/ at which it is to be booked.
	-> Bool
isSuitableBooking problemParameters executionOptions problemAnalysis timetablePair studentViewBooking@((studentBody, time), studentViewLesson)	= and [
	not $ ProblemConfiguration.ProblemAnalysis.areAnyResourcesBookedForGroupMeeting problemAnalysis time studentBody locationId teacherId,	-- Performing this check first, significantly reduces the number of viable bookings.
	not (
		StudentView.Timetable.areBookedResourcesAt studentViewTimetableForWeekBooking studentViewTimetable	-- Check whether either the location or the teacher, is already booked at the same time.
	) || and [
		permitTemporaryStudentBodyMerger,
		Model.Timetable.areMergeableWith studentViewTimetableForWeekBooking studentViewTimetable,	-- Check whether each of the current bookings are either identical or use neither of the required resources.
		Aggregate.StudentBody.getSize studentBody <= calculateRemainingPlaces problemParameters studentViewTimetable studentViewTimetableForWeekBooking,	-- Check whether both the location & the teacher, can cope with another student-body.
		Data.Student.getStream (studentBodyRegister ! studentBody) `Data.Set.member` Data.Set.map (
			Data.Student.getStream . (studentBodyRegister !)	-- There should only be no more than one stream in a single student-class.
		) (
			StudentView.Timetable.extractStudentClassAt studentViewTimetableForWeekBooking studentViewTimetable
		) -- Check that the student-body to be booked, has the same ability as the student-class into which it's to be merged.
	],
	not $ Dynamic.StudentViewTimetableForWeekUtilities.isSufficientlyBooked problemParameters studentViewTimetableForWeek studentViewLesson,	-- Check that insufficient lessons have been booked, to satisfy the course-requirements.
	not $ StudentView.Timetable.breaksRoutine studentViewTimetable studentBody studentViewLesson,	-- Check this booking doesn't break any routine established for the student-body, with either the location or the teacher.
	Dynamic.StudentViewTimetableForWeekUtilities.calculateUtilisationRatio problemParameters problemAnalysis studentBody (
		Model.TimetableForWeek.defineTimeslot (time, Just studentViewLesson) studentViewTimetableForWeek	-- Derive the proposed student-view timetable for the week.
	) <= 1,	-- Check that the student-body won't become overloaded.
	Dynamic.TeacherViewTimetableForWeekUtilities.calculateUtilisationRatio problemParameters problemAnalysis teacherId (
		TeacherView.Timetable.bookStudentViewLesson permitTemporaryStudentBodyMerger studentViewBooking teacherViewTimetable ! teacherId	-- Derive the proposed teacher-view timetable for the week.
	) <= 1,	-- Check that the teacher won't become overloaded.
	uncurry (||) . (
		not . Data.Course.isRigid &&& Data.Course.isASpecifiedTime time	-- By definition, a rigid course can only book at one of the fully specified times.
	) $ ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson
 ] where
	studentViewTimetableForWeekBooking		= Control.Arrow.first Model.TimetableCoordinates.getTime studentViewBooking
	studentBodyRegister				= ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters
	permitTemporaryStudentBodyMerger		= ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger executionOptions
	(studentViewTimetable, teacherViewTimetable)	= Dynamic.HumanViewTimetable.getStudentViewTimetable &&& Dynamic.HumanViewTimetable.getTeacherViewTimetable $ timetablePair	-- Deconstruct.
	studentViewTimetableForWeek			= studentViewTimetable ! studentBody
	(locationId, teacherId)				= StudentView.LessonResourceIds.getLocationId &&& StudentView.LessonResourceIds.getTeacherId $ Model.Lesson.getResourceIds studentViewLesson	-- Deconstruct.

{- |
	* Validates a /timetable/, as required when it has been read from file.

	* Only the first error is returned, since it may compromise the stability of subsequent tests.
-}
isTimetableValid :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			synchronisationId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId,
	RealFrac		teachingRatio,
	Show			synchronisationId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			teachingRatio,
	Show			timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level	-- ^ The studentViewTimetable to validate.
	-> Maybe String								-- ^ Any error-message; 'Nothing' represents success.
isTimetableValid	problemParameters executionOptions problemAnalysis studentViewTimetable	= Data.Maybe.listToMaybe $ Data.Maybe.catMaybes [
-- Confirm the number of timeslots per day.
		let
			nTimeslotPerDay				= ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis
			incorrectNTimeslotsByStudentBodyByDay	= Data.Map.filter (not . null) $ Data.Map.map (
				filter (
					(/= nTimeslotPerDay) . snd {-numElements-}
				) . Data.Array.IArray.assocs . Data.Array.IArray.amap Data.Array.Base.numElements
			 ) studentViewTimetable
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckTimeslotsPerDay timetableValidationSwitches) || Data.Map.null incorrectNTimeslotsByStudentBodyByDay
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkTimeslotsPerDayTag ++ ": the days for some student-bodies, have been subdivided into a different number of time-slots to that configured (i.e. " ++ show nTimeslotPerDay ++ "); " ++ show (Control.Arrow.first Aggregate.StudentBody.getMnemonic `map` Data.Map.toList incorrectNTimeslotsByStudentBodyByDay),

-- Confirm existance of all resources.
		let
			nonExistentLocationIds	= Data.Set.fromList locationIds \\ Data.Map.keysSet locationCatalogue
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckExistenceOfLocationIds timetableValidationSwitches) || Data.Set.null nonExistentLocationIds
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkExistenceOfLocationIdsTag ++ ": some referenced location-ids don't exist; " ++ show (Data.Set.toList nonExistentLocationIds),
		let
			nonExistentStudentBodies	= Data.Set.map Aggregate.StudentBody.toPair (Data.Map.keysSet studentViewTimetable) \\ Data.Set.map Aggregate.StudentBody.toPair (Data.Map.keysSet studentBodyRegister)	-- CAVEAT: avoid 'instance Eq StudentBody', which for efficiency only uses the mnemonic.
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckExistenceOfStudentBodies timetableValidationSwitches) || Data.Set.null nonExistentStudentBodies
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkExistenceOfStudentBodiesTag ++ ": some referenced student-bodies don't exist (or have different members); " ++ show (Control.Arrow.second Data.Set.toList `map` Data.Set.toList nonExistentStudentBodies),
		let
			nonExistentTeacherIds	= Data.Set.fromList teacherIds \\ Data.Map.keysSet teacherRegister
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckExistenceOfTeacherIds timetableValidationSwitches) || Data.Set.null nonExistentTeacherIds
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkExistenceOfTeacherIdsTag ++ ": some referenced teacher-ids don't exist; " ++ show (Data.Set.toList nonExistentTeacherIds),
-- Check availability of all booked resources.
		let
			unavailableResourcesByDay	= Data.Map.fromListWith (
				\(ll, sl, tl) (lr, sr, tr)	-> (
					ll `Data.Set.union` lr,
					sl `Data.Set.union` sr,
					tl `Data.Set.union` tr
				) -- Triple.
			 ) [
				(
					day,
					(unavailableLocationIds, unavailableStudentBodies, unavailableTeacherIds) -- Triple.
				) |
					(studentBody, studentViewTimetableForWeek)	<- Data.Map.toList studentViewTimetable,
					(day, studentViewTimetableForDay)		<- Data.Array.IArray.assocs studentViewTimetableForWeek,
					let
						bookedLessons	= Data.Maybe.catMaybes $ Data.Array.IArray.elems studentViewTimetableForDay

						(unavailableLocationIds, unavailableTeacherIds)	= (
							Data.Set.fromList . filter (
								not . Data.Resource.isAvailableOn day . (locationCatalogue !)
							) *** Data.Set.fromList . filter (
								not . Data.Resource.isAvailableOn day . (teacherRegister !)
							)
						 ) . unzip $ map (
							(StudentView.LessonResourceIds.getLocationId &&& StudentView.LessonResourceIds.getTeacherId) . Model.Lesson.getResourceIds
						 ) bookedLessons

						unavailableStudentBodies
							| null bookedLessons || Data.Resource.isAvailableOn day (studentBodyRegister ! studentBody)	= Data.Set.empty
							| otherwise											= Data.Set.singleton studentBody,
					not $ Data.Set.null unavailableLocationIds && Data.Set.null unavailableStudentBodies && Data.Set.null unavailableTeacherIds
			 ] -- List-comprehension.
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckResourceAvailability timetableValidationSwitches) || Data.Map.null unavailableResourcesByDay
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkResourceAvailabilityTag ++ ": some resources have been booked on days when they're unavailable; " ++ show (
				Data.Map.toList $ Data.Map.map (
					\(l, s, t) -> (
						Data.Set.toList l,
						map Aggregate.StudentBody.getMnemonic $ Data.Set.toList s,
						Data.Set.toList t
					) -- Triple.
				) unavailableResourcesByDay
			),

-- Check that all booked subjects are supported by a service.
		let
			unsupportedSubjectsByTeacherId	= Data.Map.fromListWith Data.Set.union . map (
				StudentView.LessonResourceIds.getTeacherId . Model.Lesson.getResourceIds &&& Data.Set.singleton . Model.Lesson.getSubject
			 ) . filter (
				Data.Maybe.isNothing . ProblemConfiguration.ProblemAnalysis.lookupCourseFor problemParameters
			 ) $ map Model.Timetable.getBookedLesson linearStudentViewTimetable
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckBookedSubjectsHaveService timetableValidationSwitches) || Data.Map.null unsupportedSubjectsByTeacherId
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkBookedSubjectsHaveServiceTag ++ ": some lessons are not supported by any service; " ++ show (Data.Map.toList $ Data.Map.map Data.Set.toList unsupportedSubjectsByTeacherId),

-- Check for overloading of human-resources.
		let
			overloadedStudents	= Data.Map.filter (> 1) $ Data.Map.mapWithKey (
				Dynamic.StudentViewTimetableForWeekUtilities.calculateUtilisationRatio problemParameters problemAnalysis
			 ) studentViewTimetable
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckForOverloadedStudentBodies timetableValidationSwitches) || Data.Map.null overloadedStudents
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkForOverloadedStudentBodiesTag ++ ": some student-bodies are overloaded; " ++ show (Control.Arrow.first Aggregate.StudentBody.getMnemonic `map` Data.Map.toList overloadedStudents),
		let
			overloadedTeachers	= Data.Map.filter (> 1) $ Data.Map.mapWithKey (
				Dynamic.TeacherViewTimetableForWeekUtilities.calculateUtilisationRatio problemParameters problemAnalysis
			 ) $ Dynamic.TeacherViewTimetableUtilities.fromStudentViewTimetable executionOptions problemAnalysis studentViewTimetable
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckForOverloadedTeachers timetableValidationSwitches) || Data.Map.null overloadedTeachers
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkForOverloadedTeachersTag ++ ": some teachers are overloaded; " ++ show (Data.Map.toList overloadedTeachers),
-- Check that student-bodies have a requirement for all subjects which have been booked.
		let
			unrequiredSubjectsByStudentBody	= Data.Map.filter (not . Data.Set.null) . Data.Map.mapWithKey (
				\studentBody distinctSubjects	-> distinctSubjects \\ Data.Student.deriveAmalgamatedKnowledgeRequirement (studentBodyRegister ! studentBody)
			 ) $ Data.Map.map (
				Data.Set.map Model.Lesson.getSubject . Model.TimetableForWeek.extractDistinctLessons
			 ) studentViewTimetable
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckBookedSubjectsAreRequired timetableValidationSwitches) || Data.Map.null unrequiredSubjectsByStudentBody
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkBookedSubjectsAreRequiredTag ++ ": some student-bodies are booked for subjects which they don't require; " ++ show (
				map (Aggregate.StudentBody.getMnemonic *** Data.Set.toList) $ Data.Map.toList unrequiredSubjectsByStudentBody
			),

-- Check that student-bodies aren't overbooked in any subject.
		let
			nOverbookedBySubjectByStudentBody	= Data.Map.filter (not . Data.Map.null) $ Data.Map.map (
				\studentViewTimetableForWeek -> Data.Map.fromList . filter ((> 0) . snd) . map (
					Model.Lesson.getSubject &&& negate . Dynamic.StudentViewTimetableForWeekUtilities.countUnbookedLessons problemParameters studentViewTimetableForWeek
				) . Data.Set.toList $ Model.TimetableForWeek.extractDistinctLessons studentViewTimetableForWeek
			 ) studentViewTimetable
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckForOverbookedSubjects timetableValidationSwitches) || Data.Map.null nOverbookedBySubjectByStudentBody
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkForOverbookedSubjectsTag ++ ": some student-bodies are overbooked in some subjects; " ++ show (
				map (Aggregate.StudentBody.getMnemonic *** Data.Map.toList) $ Data.Map.toList nOverbookedBySubjectByStudentBody
			),

-- Check for clashes between bookings & meetings.
		let
			bookingsAtMeetingTime	= filter (
				\booking -> uncurry (
					uncurry (
						ProblemConfiguration.ProblemAnalysis.areAnyResourcesBookedForGroupMeeting problemAnalysis
					) . (
						Model.TimetableCoordinates.getTime &&& Model.TimetableCoordinates.getObserverId
					) $ Model.Timetable.getBookedCoordinates booking
				) . (
					StudentView.LessonResourceIds.getLocationId &&& StudentView.LessonResourceIds.getTeacherId
				) . Model.Lesson.getResourceIds $ Model.Timetable.getBookedLesson booking
			 ) linearStudentViewTimetable
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckForBookingMeetingClash timetableValidationSwitches) || null bookingsAtMeetingTime
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkForBookingMeetingClashTag ++ ": some bookings have been made when some of their resources are required for a meeting; " ++ show (
				Control.Arrow.first (Control.Arrow.first Aggregate.StudentBody.getMnemonic) `map` bookingsAtMeetingTime
			),

-- Check for resource-conflicts.
		let
			timesWithIncompatibleBookings	= [
				time |
					day		<- Temporal.Day.range,
					timeslotId	<- ProblemConfiguration.ProblemAnalysis.getTimeslotIdRange problemAnalysis,
					let time = Temporal.Time.mkTime day timeslotId,
					not . Model.ResourceUser.areMutuallyMergeable $ Model.Timetable.extractSynchronousLessonsAt time studentViewTimetable
			 ] -- List-comprehension.
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckForResourceConflicts timetableValidationSwitches) || null timesWithIncompatibleBookings
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkForResourceConflictsTag ++ ": the lessons booked at some times have incompatible resource-requirements; " ++ show timesWithIncompatibleBookings,

-- Check for lessons booked at locations lacking either adequate capacity or facilities for their course.
		let
			misplacedLessonsByLocationId	= Data.Map.fromList [
				(locationId, (teacherId, Model.Lesson.getSubject lesson)) |
					booking@(_, lesson)	<- map (Model.TimetableCoordinates.getTime . Model.Timetable.getBookedCoordinates &&& Model.Timetable.getBookedLesson) linearStudentViewTimetable,
					let (locationId, teacherId)	= StudentView.LessonResourceIds.getLocationId &&& StudentView.LessonResourceIds.getTeacherId $ Model.Lesson.getResourceIds lesson,	-- Deconstruct.
					not . Data.Location.isSuitable (
						Aggregate.StudentClass.getSize $ StudentView.Timetable.extractStudentClassAt booking studentViewTimetable
					) (
						Data.Course.getRequiredFacilityNames $ ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters lesson
					) $ locationCatalogue ! locationId
			 ] -- List-comprehension.
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckForMisplacedLessons timetableValidationSwitches) || Data.Map.null misplacedLessonsByLocationId
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkForMisplacedLessonsTag ++ ": some lessons have been booked at locations lacking either adequate capacity or the required facilities for their course; " ++ show (Data.Map.toList misplacedLessonsByLocationId),

-- Check the synchronisation of the lessons booked for synchronisedCourses.
		let
			teacherIdsBySynchronisationId			= Data.Map.map Data.Map.keysSet $ ProblemConfiguration.ProblemAnalysis.getCoursesByTeacherIdBySynchronisationId problemAnalysis
			timesSynchronisationIdAndMissingTeacherIds	= [
				(time, synchronisationId, Data.Set.toList missingTeacherIds) |
					day						<- Temporal.Day.range,
					timeslotId					<- ProblemConfiguration.ProblemAnalysis.getTimeslotIdRange problemAnalysis,
					let time = Temporal.Time.mkTime day timeslotId,
					(Just synchronisationId, synchronisedTeacherIds)	<- Data.Map.toList . Data.Map.fromListWith Data.Set.union . map (
						Data.Course.getMaybeSynchronisationId . ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters &&& Data.Set.singleton . StudentView.LessonResourceIds.getTeacherId . Model.Lesson.getResourceIds
					 ) $ Model.Timetable.extractSynchronousLessonsAt time studentViewTimetable,
					let missingTeacherIds = teacherIdsBySynchronisationId ! synchronisationId \\ synchronisedTeacherIds,
					not $ Data.Set.null missingTeacherIds
			 ] -- List-comprehension.
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckSynchronisedCourses timetableValidationSwitches) || null timesSynchronisationIdAndMissingTeacherIds
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkSynchronisedCoursesTag ++ ": there are times when some teachers of a set of synchronised courses haven't a lesson booked; " ++ show timesSynchronisationIdAndMissingTeacherIds,

-- Check minimumConsecutiveLessons.
		let
			coordinatesOfShortRunlengths	= [
				coordinates |
					booking		<- linearStudentViewTimetable,
					let
						(coordinates, course)			= Model.Timetable.getBookedCoordinates &&& ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters . Model.Timetable.getBookedLesson $ booking
						(studentBody, (day, timeslotId))	= Model.TimetableCoordinates.getObserverId &&& (Temporal.Time.getDay &&& Temporal.Time.getTimeslotId) . Model.TimetableCoordinates.getTime $ coordinates,	-- Deconstruct.
					Data.Course.requiresConsecutiveLessons course,
					let findGeneralisedLessonRunlengthsByTimeslotId	= Model.Timetable.findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId studentViewTimetable ! studentBody Data.Array.IArray.! day,
					runlengthCode	<- map snd {-runlengthCode-} $ filter ((== timeslotId) . fst {-startingTimeslotId-}) findGeneralisedLessonRunlengthsByTimeslotId,
					ToolShed.Data.List.Runlength.getLength runlengthCode < Data.Course.getMinimumConsecutiveLessons course
			 ] -- List-comprehension.
		in if not (ProblemConfiguration.TimetableValidationSwitches.getCheckMinimumConsecutiveLessons timetableValidationSwitches) || null coordinatesOfShortRunlengths
			then Nothing
			else Just $ show ProblemConfiguration.TimetableValidationSwitches.checkMinimumConsecutiveLessonsTag ++ ": some lessons have been booked in shorter runlengths than required by their course; " ++ show (Control.Arrow.first Aggregate.StudentBody.getMnemonic `map` coordinatesOfShortRunlengths)
 ] where
	timetableValidationSwitches	= ProblemConfiguration.ProblemParameters.getTimetableValidationSwitches problemParameters
	locationCatalogue		= ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters
	studentBodyRegister		= ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters
	teacherRegister			= ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters
	linearStudentViewTimetable	= LinearModel.Timetable.fromTimetable studentViewTimetable
	(locationIds, teacherIds)	= unzip $ map (
		(StudentView.LessonResourceIds.getLocationId &&& StudentView.LessonResourceIds.getTeacherId) . Model.Lesson.getResourceIds . Model.Timetable.getBookedLesson
	 ) linearStudentViewTimetable

-- | The type of a function used to attempt to atomically book any /lesson/s associated with that specified, returning 'Nothing' if any were unsuccessful.
type BookAtomically campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
	= ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Data.Course.Course synchronisationId level timeslotId					-- ^ The /course/ to which the specified /lesson/ belongs.
	-> StudentView.Timetable.Booking timeslotId locationId teacherId level				-- ^ The /lesson/, & the /coordinates/ at which it has been booked in the /timetable/.
	-> Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level		-- ^ The initial /timetable/ on which to operate, as seen from the perspective of both /student/s & /teacher/s.
	-> Maybe (Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level)	-- ^ Returns a /timetable-pair/ if the associated /booking/s were completed.

-- | Use when no bookings are required.
bookAtomicallyNothing :: BookAtomically campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
bookAtomicallyNothing _ _ _ _ _	= Just

{- |
	* If the runlength of the specified /lesson/ (assumed to have already been booked), is shorter than the /minimum consecutive lessons/ for the /course/,
	then book duplicates in any available adjacent undefined /time-slot/s, until this requirement is met.

	* 'bookAtomicallyWithRamifications' books all specific time requests, & then for each one, calls this function to satisfy /minimumConsecutiveLessons/.
	This function can therefore satisfy /minimumConsecutiveLessons/, not only by booking lessons into undefined timeslots, but by merging with identical lessons.

	* Given a choice between adjacent undefined /time-slot/s, prefer those which have been specifically requested,
	& failing that, those which are closer to any /ideal timeslot/ requested for the /course/.

	* If insufficient adjacent undefined /time-slot/s (or defined with the required /lesson/s) are available to grow a runlength of the required length,
	or any of the sequence of /booking/s, sequentially applied to the /timetable/, fails 'isSuitableBooking',
	then return 'Nothing'.

	* After successfully booking all these /lesson/s, calls the specified /continuation/-function to book any /synchronised course/s.
-}
bookAtomicallyMinimumConsecutiveLessons :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			synchronisationId,
	Ord			level,
	Ord			locationId,
	Ord			stream,
	Ord			teacherId,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			timeslotId,
	Show			teacherId
 )
	=> BookAtomically campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Continuation.
	-> BookAtomically campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
bookAtomicallyMinimumConsecutiveLessons continuation problemParameters executionOptions problemAnalysis course studentViewBooking {-already booked-} timetablePair
	| minimumConsecutiveLessons == 1 || consecutiveLessonShortage <= 0	= continuation problemParameters executionOptions problemAnalysis course studentViewBooking timetablePair	-- Shortage may be negative, if consecutive specified time-slots exceeds 'Data.Course.getMinimumConsecutiveLessons'.
	| otherwise	= Data.Maybe.listToMaybe {-select the most suitable timetable-} . Data.Maybe.mapMaybe (
		(
			\requiredAdjacentBookings -> foldr (
				(=<<) . continuation problemParameters executionOptions problemAnalysis course	-- Take the Booking & the Maybe TimetablePair & pass them to the continuation; if the latter is 'Nothing' then that's merely academic.
			) (
				Dynamic.HumanViewTimetable.bookAtomically (
					ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger executionOptions
				) (
					isSuitableBooking problemParameters executionOptions problemAnalysis	-- Partially apply the predicate; it now merely requires a TimetablePair & Booking.
				) timetablePair requiredAdjacentBookings	-- Construct an initial TimetablePair, by validating & booking the required adjacent lessons. Returns 'Nothing' if any booking fails the predicate.
			) $ studentViewBooking : requiredAdjacentBookings	-- Forward all the required bookings including the original, to the continuation.
		) . map (
			flip (,) studentViewLesson . (,) studentBody . Temporal.Time.mkTime day	-- Construct a booking from each undefined timeslotId.
		) . filter (
			Data.Maybe.isNothing . (studentViewTimetableForDay Data.Array.IArray.!)	-- There's no need to account for any bookings with which this runlength was merged.
		) . map fst {-timeslotIds-}
	) . Data.List.sortBy (
		Data.Ord.comparing $ (
			\(primarySortCriterion, secondarySortCriterion, tertiarySortCriterion)	-> (
				length $ filter not primarySortCriterion,	-- Minimise the number of unspecified times included in the span, & therefore the number of additional bookings required (since any specified times have already been booked by 'bookAtomicallyWithRamifications').
				sum secondarySortCriterion,			-- Minimise blocking of synchronous synchronised courses in which we're interested.
				maximum tertiarySortCriterion			-- Minimise the maximum distance from any ideal timeslotId; 'Nothing' compares less than 'Just'.
			) -- Triple.
		) . unzip3 . map snd {-sort-criteria-}
	) . map (
		take consecutiveLessonShortage	-- Take the required length from the start of each tail, which includes the booked coordinates, so there's no gap.
	) . take (
		succ {-fence-post-} $ length adjacentSuitableTimeslotIds - consecutiveLessonShortage	-- Take the shrinking tails until they're insufficiently long to satisfy the shortage.
	) . Data.List.tails $ map (
		\adjacentSuitableTimeslotId -> (
			adjacentSuitableTimeslotId,
			let
				adjacentSuitableTime	= Temporal.Time.mkTime day adjacentSuitableTimeslotId
			in (
				Temporal.TimeslotRequest.isASpecifiedTime adjacentSuitableTime timeslotRequest,									-- Primary sort-criterion.
				length {-count-} . filter (
					Data.Set.member studentBody . (
						ProblemConfiguration.ProblemAnalysis.getStudentBodiesBySynchronisationId problemAnalysis !
					) -- Select synchronous synchronised courses in which we've an interest.
				) . Data.Maybe.catMaybes . filter (
					/= Data.Course.getMaybeSynchronisationId course	-- We're looking for problems, so a match for the current course can be ignored.
				) . map (
					Data.Course.getMaybeSynchronisationId . ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters
				) $ Model.Timetable.extractSynchronousLessonsAt adjacentSuitableTime studentViewTimetable,							-- Secondary sort-criterion.
				Temporal.Time.calculateAbsoluteDistance adjacentSuitableTimeslotId `fmap` Temporal.TimeslotRequest.getMaybeIdealTimeslotId timeslotRequest	-- Tertiary sort-criterion.
			) -- Triple of sort-criteria.
		) -- Pair.
	) adjacentSuitableTimeslotIds
	where
		((studentBody, (day, timeslotId)), studentViewLesson)	= (
			Model.TimetableCoordinates.getObserverId &&& (
				Temporal.Time.getDay &&& Temporal.Time.getTimeslotId
			) . Model.TimetableCoordinates.getTime
		 ) . Model.Timetable.getBookedCoordinates &&& Model.Timetable.getBookedLesson $ studentViewBooking	-- Deconstruct.

		studentViewTimetable		= Dynamic.HumanViewTimetable.getStudentViewTimetable timetablePair	-- Access.
		studentViewTimetableForDay	= studentViewTimetable ! studentBody Data.Array.IArray.! day	-- Lookup.
		adjacentSuitableTimeslotIds	= uncurry (++) . Control.Arrow.first reverse {-chronological order-} . ToolShed.Data.Pair.mirror (
			map Model.TimetableForDay.getTimeslotId . take consecutiveLessonShortage {-ensure that the new runlength reaches the booked lesson-} . takeWhile (
				Data.Maybe.maybe True {-undefined-} (
					== studentViewLesson	-- We could merge the current runlength with a separated span of identical lessons.
				) . Model.TimetableForDay.getMaybeLesson	-- Find space into which the required runlength can be grown.
			) . dropWhile (
				(== Just studentViewLesson) . Model.TimetableForDay.getMaybeLesson	-- Traverse any adjacent runlength.
			)
		 ) $ Model.TimetableForDay.bisectAt timeslotId studentViewTimetableForDay {-extract generalised bookings in two halves surrounding a gap @ the pre-booked coordinates-}

		minimumConsecutiveLessons, consecutiveLessonShortage :: Size.NTimeslots
		(minimumConsecutiveLessons, timeslotRequest)	= Data.Course.getMinimumConsecutiveLessons &&& Data.Course.getTimeslotRequest $ course	-- Deconstruct.
		consecutiveLessonShortage			= minimumConsecutiveLessons - Model.TimetableForDay.countRunlengthAt timeslotId studentViewTimetableForDay

{- |
	* Makes the specified /booking/.

	* If the /course/ to which the proposed /lesson/ belongs, defines any specified /time/s (even if it's not the proposed /lesson/ itself), then it attempts to book them all also.

	* Then having made the originally booking, with similar /lesson/s at any specifically requested times, calls 'bookAtomicallyMinimumConsecutiveLessons' for each /booking/ (including the original),
	to account for the possibility that the /course/ they share, mandates a non-trivial 'Data.Course.getMinimumConsecutiveLessons'.

	* If any of the /booking/s (including the original), sequentially applied to the /timetable/, fails 'isSuitableBooking', then return 'Nothing'.

	* CAVEAT: it is assumed that no /lesson/ is booked at the specified /coordinates/, since this will be obliterated;
	otherwise an existing run of identical /lesson/s used to meet /minimumConsecutiveLessons/ may be compromised.

	* CAVEAT: any additional /lesson/s required at /specified time/s, may result in deletion of previous /booking/s (even those for /synchronised course/s),
	on the grounds that there's no alternative /coordinate/ at which to book them.

	* TODO: All specifically requested times booked before forwarding the resulting timetable to bookAtomicallyMinimumConsecutiveLessons,
	which then finds it can't book thre required minimumConsecutiveLessons since it bumps into some of those just made, even though they're of the correct type (which it doesn't check).
	The reason it doesn't check is that joining with an existing /lesson/ may result in too long a span.
-}
bookAtomicallyWithRamifications :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			stream,
	Ord			synchronisationId,
	Ord			teacherId,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> BookAtomically campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Continuation.
	-> BookAtomically campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
bookAtomicallyWithRamifications continuation problemParameters executionOptions problemAnalysis course studentViewBooking timetablePair
	| Data.Foldable.any (
		uncurry (||) . (
			not . (
				$ (
					(
						ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters !
					) . Model.TimetableCoordinates.getObserverId {-studentBody-} . Model.Timetable.getBookedCoordinates &&& (
						ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters !
					) . StudentView.LessonResourceIds.getLocationId . Model.Lesson.getResourceIds . Model.Timetable.getBookedLesson
				) studentViewBooking -- The original booking requires a resource which is unavailable on a specified day; there's no need to check the teacher who's offering the course, since they must be available on all specified days.
			) . Data.Resource.isAvailableOn . Temporal.Time.getDay . Model.TimetableCoordinates.getTime &&& (
				`Model.Timetable.isDefinedTimeslot` studentViewTimetable
			) -- A different lesson has already been booked @ one of the specified times; & it can't easily be removed, since it may have a non-trivial minimumConsecutiveLessons or be from a synchronised course. By not booking any of the specified times, an alternative course is still an option.
		) . Model.Timetable.getBookedCoordinates {-discard the booked lesson-}
	) specifiedBookings		= Nothing
	| otherwise			= let
		requiredBookings	= Data.Set.insert studentViewBooking specifiedBookings	-- Include the original booking (if it hasn't already been), which is assumed to be currently unbooked.
	in Data.Set.foldr (
		(=<<) . bookAtomicallyMinimumConsecutiveLessons continuation problemParameters executionOptions problemAnalysis course -- Attempt to book the minimum consecutive lessons.
	) (
		Dynamic.HumanViewTimetable.bookAtomically (
			ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger executionOptions
		) (
			isSuitableBooking problemParameters executionOptions problemAnalysis	-- Partially apply the predicate; it now merely requires a TimetablePair & Booking.
		) timetablePair requiredBookings	-- Construct an initial TimetablePair, from the original booking copied to any times specified by the course. Returns 'Nothing' is any booking fails 'isSuitableBooking'.
	) requiredBookings
	where
		studentViewTimetable	= Dynamic.HumanViewTimetable.getStudentViewTimetable timetablePair	-- Access.
		specifiedBookings	= Data.Set.filter (
			not . (`Model.Timetable.isBookedWith` studentViewTimetable)	-- Ignore any of the times specified for the course, @ which the required booking has already been made.
		 ) . Data.Set.map (
			\time -> Control.Arrow.first {-coordinates-} (
				Control.Arrow.second $ const time	-- Replace the booking-time with the one specified for the course. Books with the same teacher, into the same location, to avoid breaking routine, since this is required by 'isSuitableBooking'.
			) studentViewBooking
		 ) . Temporal.TimeslotRequest.getSpecifiedTimes $ Data.Course.getTimeslotRequest course

-- | The number of places still available for /student/s, at the specified time, in the /location/ & by the /teacher/, defined by the specified /lesson/.
calculateRemainingPlaces :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> StudentView.TimetableForWeek.Booking timeslotId locationId teacherId level	-- ^ The proposed /booking/.
	-> Size.NStudents
calculateRemainingPlaces problemParameters timetable studentViewTimetableForWeekBooking	= ProblemConfiguration.ProblemAnalysis.countStudentPlaces problemParameters (
	ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson
 ) (
	StudentView.LessonResourceIds.getLocationId $ Model.Lesson.getResourceIds studentViewLesson
 ) - Aggregate.StudentClass.getSize (StudentView.Timetable.extractStudentClassAt studentViewTimetableForWeekBooking timetable) where
	studentViewLesson	= Model.TimetableForWeek.getBookedLesson studentViewTimetableForWeekBooking

{- |
	* For all /lesson/-definitions, finds the /average absolute deviation/ (<https://en.wikipedia.org/wiki/Absolute_deviation#Average_absolute_deviation>),
	between the /time-slot/ at which they were booked, & any /ideal timeslot-request/ for the /course/.

	* Finds the /average/, by weighting each time-difference by the size of the /student-body/ for whom it was booked,
	& dividing by the total number of /student/s involved.

	* If the /course/ corresponding to the /lesson/ at the specified /time-slot/, also specifies a /minimum consecutive lesson/, then one cannot simultaneously satisfy both criteria.
	Under these circumstances one measures the /average signed deviation/ of the runlength of consecutive /lesson/s from the /ideal timeslot-request/, & applies it to each member of the runlength.
	This results in a lower value than the /average absolute deviation/ when the runlength bridges the /ideal timeslot-request/, because some deviations cancel those at the opposite end of the span.
	E.g. if the /minimum consecutive lessons/ is 3, & the /ideal time-slot/ is in the middle of the observed runlength,
	the the deviation attributed to each member is /[0, 0, 0]/, rather than /[1, 0, 1]/.

	* Instead of measuring the signed deviation from the /ideal timeslot-request/,
	one actually measures the signed deviation from the closest to the ideal than can be achieved given the specified /minimum consecutive lessons/.
	This avoids disadvantaging booking of /lesson/s in /course/s whose /ideal timeslot-request/ can't be achieved as an average of the /lesson/s in the runlength required by the /minimum consecutive lessons/.
	E.g. if @timeslotIdBounds = (0,4)@, @idealTimeslotId = 0@, & @minimumConsecutiveLessons = 3@, then measure the deviation wrt one rather than zero.
-}
calculateAverageAbsoluteDeviationFromIdealTimeslotRequest :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			locationId,
	Fractional		deviation,
	Ord			level,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> deviation
calculateAverageAbsoluteDeviationFromIdealTimeslotRequest problemParameters problemAnalysis timetable
	| Data.Set.null (
		ProblemConfiguration.ProblemAnalysis.getSubjectsWithAnIdealTimeslotRequest problemAnalysis
	) || denominator == 0	= 0					-- Zero student-bodies are affected by deviations from ideal timeslot-requests, so there's zero significance.
	| denominator == 0	= error "WeekDaze.Dynamic.StudentViewTimetableUtilities.calculateAverageAbsoluteDeviationFromIdealTimeslotRequest:\tattempt to divide by zero (lessons)."
	| otherwise		= numerator / fromIntegral denominator	-- Average over all courses which request an ideal timeslot-request.
	where
		(minTimeslotIdBound, maxTimeslotIdBound)	= Factory.Data.Interval.getMinBound &&& Factory.Data.Interval.getMaxBound $ ProblemConfiguration.ProblemParameters.getTimeslotIdBounds problemParameters
		(numerator, denominator)			= Data.Map.foldrWithKey (
			\studentBody studentViewTimetableForWeek	-> let
				nStudents :: Size.NStudents
				nStudents	= Aggregate.StudentBody.getSize studentBody
			in uncurry (***) . (
				(+) . (* fromIntegral nStudents) *** (+) . (* nStudents)	-- Independently accumulate the numerator & denominator, each weighted by the size of the student-body.
			) $ Data.Foldable.foldr (
				\studentViewTimetableForDay total	-> foldr (
					\(timeslotId, maybeStudentViewLesson) total'	-> let
						(minimumConsecutiveLessons, maybeIdealTimeslot)	= Data.Course.getMinimumConsecutiveLessons &&& Temporal.TimeslotRequest.getMaybeIdealTimeslotId . Data.Course.getTimeslotRequest $ ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters $ Data.Maybe.fromJust maybeStudentViewLesson
						(runlengthBounds, runLengthSpan)		= Model.TimetableForDay.measureRunlengthAt timeslotId studentViewTimetableForDay
					in Data.Maybe.maybe total' (
						\idealTimeslotId -> (
							(
								if minimumConsecutiveLessons > 1
									then (
										/ fromIntegral runLengthSpan	-- Divide the discrepancy evenly over all lessons in the observed runlength.
									) . fromIntegral . (
										floor {-fractional improvements can't be achieved-} :: Rational -> Size.NTimeslots
									) . abs . (
										+ (
											negate . max (
												(/ 2) . fromIntegral $ pred {-fence-post-} (fromEnum minTimeslotIdBound * 2) + minimumConsecutiveLessons	-- The mid-point of a span of minimumConsecutiveLessons starting at the first timeslot.
											) . min (
												(/ 2) . fromIntegral $ succ {-fence-post-} (fromEnum maxTimeslotIdBound * 2) - minimumConsecutiveLessons	-- The mid-point of a span of minimumConsecutiveLessons ending at the last timeslot.
											) . fromIntegral $ fromEnum idealTimeslotId
										) -- Measure the signed distance from, the closest to the ideal timeslot-request that can actually be achieved given the specified minimumConsecutiveLessons.
									) . (
										/ 2	-- Find the mid-point of the observed runlength.
									) . fromIntegral . uncurry (+) $ ToolShed.Data.Pair.mirror fromEnum runlengthBounds
									else fromIntegral $ Temporal.Time.calculateAbsoluteDistance idealTimeslotId timeslotId
							) +
						) *** succ {-count lessons whose course specifies an ideal timeslotId-} $ total'
					) maybeIdealTimeslot
				) total . filter (
					Data.Maybe.maybe False (
						(`Data.Set.member` ProblemConfiguration.ProblemAnalysis.getSubjectsWithAnIdealTimeslotRequest problemAnalysis) . Model.Lesson.getSubject	-- Include only those lessons for which there's a possibility that the corresponding course has an ideal timeslot-request.
					) . snd {-maybe lesson-}
				) $ Data.Array.IArray.assocs studentViewTimetableForDay
			) (0, 0) studentViewTimetableForWeek	-- One could select only those days on which the studentBody is available, but is saves no time.
		 ) (0, 0) timetable

{- |
	The number of /lesson/s booked for each /student-body/, relative to the required limit of their maximum workload;
	based on 'Model.Timetable.calculateUtilisationRatioByObserverId'.
-}
calculateUtilisationRatioByStudentBody
	:: (Data.Array.IArray.Ix timeslotId, RealFrac teachingRatio)
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> Data.Map.Map Aggregate.StudentBody.StudentBody teachingRatio
calculateUtilisationRatioByStudentBody problemParameters problemAnalysis	= Model.Timetable.calculateUtilisationRatioByObserverId (ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters) (ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis)

-- | The weighted mean over all /student-bodies/, of the number of /lesson/s booked, relative to the required limit of their maximum workload.
calculateWeightedMeanUtilisationRatio
	:: (Data.Array.IArray.Ix timeslotId, RealFrac teachingRatio)
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> teachingRatio
calculateWeightedMeanUtilisationRatio problemParameters problemAnalysis timetable	= Factory.Math.Statistics.getWeightedMean . map (
	Data.Tuple.swap . Control.Arrow.first Aggregate.StudentBody.getSize {-weight this result-}
 ) . Data.Map.toList $ calculateUtilisationRatioByStudentBody problemParameters problemAnalysis timetable

{- |
	* Counts the total number of unallocated /time-slot/s for all /student/s in one week;
	discounting /day/s on which the /student-body/ is un/available/, & also /time-slot/s which are allocated to either free-study or /meeting/s.

	* The results for each /student-body/ are multiplied by the number of /student/s in the body.
-}
countFreeLessons
	:: (Data.Array.IArray.Ix timeslotId, RealFrac teachingRatio)
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> Size.NTimeslots
countFreeLessons problemParameters problemAnalysis = Data.Map.foldrWithKey (
	\studentBody -> let
		studentProfile	= ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters ! studentBody
	in (+) . (
		* Aggregate.StudentBody.getSize studentBody	-- Weight according to the number of students affected.
	) . subtract (
		Data.HumanResource.getNTimeslotsPerWeekOfNonTeaching (ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis) studentProfile	-- Includes both free study & meetings.
	) . Model.TimetableForWeek.countUnallocatedAvailableTimeslots studentProfile
 ) 0

-- | Counts the total number of times /student/s must change /campus/ within a /day/, during the /week/.
countInterCampusMigrations :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			campus,
	Ord			locationId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> Size.NTimeslots
countInterCampusMigrations problemParameters	= Data.Foldable.sum . countInterCampusMigrationsByStudentBody problemParameters

-- | Constructs a map indexed by /student-body/ of the number of inter-/campus/ migrations of /student/s, within a /day/, during the week.
countInterCampusMigrationsByStudentBody :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			campus,
	Ord			locationId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> StudentView.Timetable.InterCampusMigrationsByStudentBody
countInterCampusMigrationsByStudentBody problemParameters studentViewTimetable	= Data.Map.mapWithKey (
	\studentBody studentProfile	-> Aggregate.StudentBody.getSize studentBody * Dynamic.TimetableForWeekUtilities.countInterCampusMigrations problemParameters studentProfile StudentView.LessonResourceIds.getLocationId (studentViewTimetable ! studentBody)
 ) $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters

{- |
	Calculates the mean, over those /day/s on which the /student-body/ is /available/,
	of the ratio of those free (neither booked with a /lesson/, not reserved for a /meeting/) /time-slots/ which comply with the specified preference,
	to the total number of free /time-slot/s.
-}
calculateMeanFreePeriodCompliance :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		ratio,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> ratio
calculateMeanFreePeriodCompliance problemParameters	= Model.Timetable.calculateMeanFreePeriodCompliance (
	ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters
 ) (
	Aggregate.GroupCatalogue.getMeetingTimes (ProblemConfiguration.ProblemParameters.getGroupCatalogue problemParameters) . Data.HumanResource.getGroupMembership
 )

{- |
	* Find sessions of identical consecutive /lesson/s which are longer than the /idealConsecutiveLessons/, for the /course/ to which they belong.

	* Sessions which are longer than /minimumConsecutiveLessons/ are necessary when /requiredLessonsPerWeek/ isn't an integral multiple,
	but sessions longer than the rounded-up value of /idealConsecutiveLessons/ are neither necessary nor desirable.

	* CAVEAT: sessions which are longer than /minimumConsecutiveLessons/ are also necessary when the required /resource/s are simultaneously /available/ on fewer /day/s
	than @ requiredLessonsPerWeek / minimumConsecutiveLessons @,
	but that isn't addressed here.
-}
findExcessiveLessonRunlengthsByTimeByStudentBody :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> StudentView.Timetable.LessonRunlengthByStudentBody timeslotId locationId teacherId level
findExcessiveLessonRunlengthsByTimeByStudentBody problemParameters	= Data.Map.map (
	\generalisedLessonRunlengthsByTimeslotIdByDay -> [
		(Temporal.Time.mkTime day startingTimeslotId, (runlength, studentViewLesson)) |
		(day, generalisedLessonRunlengthsByTimeslotId)			<- Data.Array.IArray.assocs generalisedLessonRunlengthsByTimeslotIdByDay,
		(startingTimeslotId, (runlength, Just studentViewLesson))	<- generalisedLessonRunlengthsByTimeslotId,	-- Select bookings.
		runlength > 1,	-- Select runlengths which might be too long.
		let
			idealConsecutiveLessons :: Rational
			idealConsecutiveLessons	= Data.Course.calculateIdealConsecutiveLessons $ ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson,
		runlength > ceiling {-round up-} idealConsecutiveLessons
	] -- List-comprehension.
 ) . Model.Timetable.findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId

{- |
	* Find sessions of identical consecutive /lesson/s which are shorter than the /minimumConsecutiveLessons/, for the /course/ to which they belong.

	* CAVEAT: if this occurs, then there's a coding-error.
-}
findShortLessonRunlengthsByTimeByStudentBody :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> StudentView.Timetable.LessonRunlengthByStudentBody timeslotId locationId teacherId level
findShortLessonRunlengthsByTimeByStudentBody problemParameters	= Data.Map.map (
	\generalisedLessonRunlengthsByTimeslotIdByDay -> [
		(Temporal.Time.mkTime day startingTimeslotId, (runlength, studentViewLesson)) |
		(day, generalisedLessonRunlengthsByTimeslotId)			<- Data.Array.IArray.assocs generalisedLessonRunlengthsByTimeslotIdByDay,
		(startingTimeslotId, (runlength, Just studentViewLesson))	<- generalisedLessonRunlengthsByTimeslotId,	-- Select bookings.
		runlength < Data.Course.getMinimumConsecutiveLessons (ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson)
	] -- List-comprehension.
 ) . Model.Timetable.findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId

{- |
	* Locate any /lesson/s booked for /synchronised course/s.

	* CAVEAT: doesn't reference /synchronised course/s for which nothing has been booked.
-}
locateLessonsBySynchronisationId :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			level,
	Ord			synchronisationId,
	Ord			teacherId
 )	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> Data.Map.Map synchronisationId (StudentView.TimetableCoordinates.Vector timeslotId)
locateLessonsBySynchronisationId problemParameters studentViewTimetable	= Data.Map.fromListWith (++) [
	(synchronisationId, [bookedCoordinates]) |
		(Just synchronisationId, bookedCoordinates)	<- map (
			Data.Course.getMaybeSynchronisationId . ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters . Model.Timetable.getBookedLesson &&& Model.Timetable.getBookedCoordinates
		) $ LinearModel.Timetable.fromTimetable studentViewTimetable
 ] -- List-comprehension.

{- |
	Locates unallocated /coordinates/ in the specified /timetable/,
	omitting any /day/s on which the /student-body/ is un/available/, & any /coordinate/ reserved by the /student-body/ for a /meeting/.
-}
locateUnallocatedAvailableUnreservedCoordinates
	:: Data.Array.IArray.Ix timeslotId
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> StudentView.TimetableCoordinates.Vector timeslotId
locateUnallocatedAvailableUnreservedCoordinates problemParameters problemAnalysis	= Model.Timetable.locateUnallocatedAvailableUnreservedCoordinates (
	Data.Map.map ToolShed.Data.Triple.getSecond {-discard the sets of locationIds & teacherIds which are booked for meetings-} $ ProblemConfiguration.ProblemAnalysis.getMeetingResourcesByTime problemAnalysis
 ) (
	ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters
 )
