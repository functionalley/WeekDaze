{-# LANGUAGE CPP, ScopedTypeVariables #-}
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

	* Constructs a /timetable/, based on the one specified, but with unallocated /time-slot/s booked where possible,
	with /lesson/-definitions conforming to the problem-specification, but are otherwise randomly selected.

	* It may be necessary to unbook some /lesson/s first, to create construction-space.
-}

module WeekDaze.Implementation.RandomConstructor(
-- * Functions
--	bookAtomicallySynchronisedLessons,
	mutateStudentViewTimetable
) where

import			Control.Arrow((&&&))
import			Data.Map((!))
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	System.Random
import qualified	ToolShed.System.Random
import qualified	WeekDaze.Aggregate.LocationCatalogue			as Aggregate.LocationCatalogue
import qualified	WeekDaze.Aggregate.StudentBody				as Aggregate.StudentBody
import qualified	WeekDaze.Data.Course					as Data.Course
import qualified	WeekDaze.Data.Resource					as Data.Resource
import qualified	WeekDaze.Data.Student					as Data.Student
import qualified	WeekDaze.Data.Teacher					as Data.Teacher
import qualified	WeekDaze.Dynamic.HumanViewTimetable			as Dynamic.HumanViewTimetable
import qualified	WeekDaze.Dynamic.StudentViewTimetableUtilities		as Dynamic.StudentViewTimetableUtilities
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.Model.Lesson					as Model.Lesson
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.Model.TimetableCoordinates			as Model.TimetableCoordinates
import qualified	WeekDaze.Model.TimetableForDay				as Model.TimetableForDay
import qualified	WeekDaze.Model.TimetableForWeek				as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.StudentView.LessonResourceIds			as StudentView.LessonResourceIds
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.StudentView.TimetableCoordinates		as StudentView.TimetableCoordinates
import qualified	WeekDaze.Temporal.Time					as Temporal.Time

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<*>))
#endif

{- |
	* If the specified /lesson/ (which is assumed to have already been booked), is a member of a set of /synchronised courses/,
	then where required, attempt to book synchronous /lesson/s for the other /teacher/s.

	* The /subject/ & /teacher/ of any additional /lesson/s is defined by their respective synchronised /course/s.
	The /locationId/ & /student-body/ for any additional /lesson/s are selected randomly.

	* CAVEAT: any additional /booking/s are delegated to 'Dynamic.StudentViewTimetableUtilities.bookAtomicallyWithRamifications',
	which accounts for further ramifications; e.g. other /time/s specified by the /course/, & non-trivial /minimum consecutive lessons/.

	* CAVEAT: before changing, review the namesake 'Implementation.DeterministicConstructor.bookAtomicallySynchronisedLessons'.
-}
bookAtomicallySynchronisedLessons :: forall campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio randomGen stream synchronisationId teacherId teachingRatio timeslotId. (
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
	Show			timeslotId,
	System.Random.RandomGen	randomGen
 ) => randomGen -> Dynamic.StudentViewTimetableUtilities.BookAtomically campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
bookAtomicallySynchronisedLessons randomGen problemParameters@ProblemConfiguration.ProblemParameters.MkProblemParameters {
	ProblemConfiguration.ProblemParameters.getLocationCatalogue	= locationCatalogue,
	ProblemConfiguration.ProblemParameters.getStudentBodyRegister	= studentBodyRegister,
	ProblemConfiguration.ProblemParameters.getTeacherRegister	= teacherRegister
} executionOptions problemAnalysis course studentViewBooking {-already booked-} humanViewTimetablePair	= Data.Maybe.maybe (
	Just humanViewTimetablePair	-- Since the original booking has already been made, no action is required.
 ) slave $ Data.Course.getMaybeSynchronisationId course where
	studentViewTimetableCoordinates			= Model.Timetable.getBookedCoordinates studentViewBooking	-- Access.
	(bookedStudentBody, time)			= Model.TimetableCoordinates.getObserverId &&& Model.TimetableCoordinates.getTime $ studentViewTimetableCoordinates	-- Deconstruct.
	day						= Temporal.Time.getDay time	-- The day on which to book a lesson, for each synchronised course.
	(studentViewTimetable, teacherViewTimetable)	= Dynamic.HumanViewTimetable.getStudentViewTimetable &&& Dynamic.HumanViewTimetable.getTeacherViewTimetable $ humanViewTimetablePair	-- Deconstruct.

	isAvailable :: Data.Resource.Resource resource => resource -> Bool
	isAvailable	= Data.Resource.isAvailableOn day	-- Partially apply.

	slave :: synchronisationId -> Maybe (Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level)
	slave synchronisationId
		| or [
			any (
				\(teacherId, synchronisedCourse)	-> Model.Timetable.hasMatchingLessonAt (
					(/= Data.Course.getSubject synchronisedCourse) . Model.Lesson.getSubject	-- Check for an incompatible booking.
				) (teacherId, time) {-coordinates-} teacherViewTimetable
			) $ Data.Map.toList synchronisedCoursesByTeacherId,	-- Some of the teachers of this set of synchronised courses, have booked a lesson at the required time, but in the wrong subject.
			not $ all isAvailable synchronisedTeacherProfiles,	-- Some of the required teachers are unavailable on the required day.
			any (
				Data.Foldable.any (Data.Course.isASpecifiedTime time) . Data.Set.filter (
					(/= Just synchronisationId) . Data.Course.getMaybeSynchronisationId	-- Consider only other courses.
				) . Data.Teacher.getService
			) synchronisedTeacherProfiles,	-- Some of the teachers of this set of synchronised courses, are already committed at the required time; even if it's not yet booked.
			not . Data.Foldable.all isAvailable $ Data.Map.filter (
				Data.Student.requiresAnySubjectBy (
					`Data.Foldable.elem` Data.Map.map Data.Course.getSubject requiredSynchronisedCoursesByTeacherId
				)
			) studentBodyRegister	-- Some interested student-bodies are unavailable. TODO: inessential; not all student-bodies must sit each lesson simultaneously with all other bodies ?
		]							= Nothing			-- Some of the resources are unavailable or otherwise committed.
		| Data.Map.null requiredSynchronisedCoursesByTeacherId	= Just humanViewTimetablePair	-- Lessons for all the synchronised courses have already been booked at the required time.
		| otherwise						= ToolShed.System.Random.select randomGen humanViewTimetablePairs
		where
			synchronisedCoursesByTeacherId		= ProblemConfiguration.ProblemAnalysis.getCoursesByTeacherIdBySynchronisationId problemAnalysis ! synchronisationId	-- Extract this set of synchronised courses.

			requiredSynchronisedCoursesByTeacherId	= Data.Map.filterWithKey (
				\teacherId _	-> not $ Model.Timetable.isDefinedTimeslot (teacherId, time) teacherViewTimetable
			 ) synchronisedCoursesByTeacherId	-- We've checked that teachers who offer one of this set of synchronised courses, & who're booked at this time, must be teaching the correct subject; find those who're unbooked.
			synchronisedTeacherProfiles		= map (teacherRegister !) $ Data.Map.keys requiredSynchronisedCoursesByTeacherId
{-
	Starting from a singleton constructed from the original timetable-pair,
	book one lesson, in the synchronised course offered by each of the other teachers, thus maintaining the synchronisation required.
	Returns all viable permutations, along with an evaluation of their fitness.

	CAVEAT:
		This algorithm only books ONE of the student-bodies interested in each of the required synchronised courses.
		This is sufficient but suboptimal; since student-body S1 can't be split, S2 & S3 MUST merge if they're to book a second lesson:

					Time 1	Time 2
					======	======
		(Teacher 1,Course a)	S1	S1
		(Teacher 2,Course b)	S2	S3

		So, if after booking S1, S1 xor S2 is booked,
		then when the requirement for the other is discovered, the booking can only be made, by merging with the previous booking,
		even though all the teachers at that time are already, as minimally required, booked.
-}
			humanViewTimetablePairs :: [Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level]
			humanViewTimetablePairs	= Data.Map.foldr (
				\unaryBookingFunctions -> Data.Maybe.catMaybes {-forward viable permutations to the next teacher-} . (
					unaryBookingFunctions <*>	-- Generate permutations of the list of unary functions applied to the list of possible Human-view TimetablePairs.
				)
			 ) [
				humanViewTimetablePair
			 ] {-Initial value-} $ Data.Map.mapWithKey (
				\teacherId synchronisedCourse	-> [
					Dynamic.StudentViewTimetableUtilities.bookAtomicallyWithRamifications Dynamic.StudentViewTimetableUtilities.bookAtomicallyNothing {-continuation. CAVEAT: don't call 'bookAtomicallySynchronisedLessons' recursively, which would book lessons, that this call has identify as required-} problemParameters executionOptions problemAnalysis synchronisedCourse synchronisedStudentViewBooking |	-- Construct a unary booking-function by partial application.
						let subject	= Data.Course.getSubject synchronisedCourse,	-- Find the single subject offered by this teacher of one of the synchronised courses.
						studentBody	<- Data.Map.keys $ Data.Map.filter (
							Data.Set.member subject . Data.Student.deriveAmalgamatedKnowledgeRequirement
						) studentBodyRegister,	-- Find the student-bodies requiring the subject offered by this teacher of the synchronised courses.
						studentBody /= bookedStudentBody,	-- The synchronised lessons can't involve the same studentBody as the original 'studentViewBooking'.
						not . uncurry (||) . (
							Model.TimetableForWeek.isDefinedTimeslot time {-check the student-body isn't already booked-} &&& Model.TimetableForDay.isSubjectBooked subject . (Data.Array.IArray.! day)
						) $ studentViewTimetable ! studentBody,
						not $ ProblemConfiguration.ProblemAnalysis.doAllCoursesSatifyingAnotherKnowledgeRequirementSpecifyThisTime problemParameters problemAnalysis subject studentViewTimetableCoordinates,	-- Avoid booking-times specified by all courses that match one of the other knowledge-requirements of this student-body.
						locationId	<- Data.Map.keys $ Aggregate.LocationCatalogue.findSuitableLocations (
							Aggregate.StudentBody.getSize studentBody
						) (
							Data.Course.getRequiredFacilityNames synchronisedCourse
						) $ Data.Map.filter isAvailable locationCatalogue,	-- Identify suitable locations.
						(/= locationId) . StudentView.LessonResourceIds.getLocationId . Model.Lesson.getResourceIds $ Model.Timetable.getBookedLesson studentViewBooking, -- The synchronised lessons can't occupy the same location as the original 'studentViewBooking'.
						let synchronisedStudentViewBooking	= ((studentBody, time), Model.Lesson.MkLesson (StudentView.LessonResourceIds.MkLessonResourceIds locationId teacherId) subject),
						Dynamic.StudentViewTimetableUtilities.isSuitableBooking problemParameters executionOptions problemAnalysis humanViewTimetablePair synchronisedStudentViewBooking	-- Remove any which are unsuitable in the context of the specified timetable.
				] -- List-comprehension: to generate a list of viable student-view bookings for this teacher.
			 ) requiredSynchronisedCoursesByTeacherId

{- |
	* Books where possible, /lesson/s into unallocated /time-slot/s, in the specified /timetable/, at the specified /coordinates/.

	* Each /lesson/ booked, is randomly selected from a set constrained only by the /problem-parameters/.
	No /lesson/s are discarded merely because they're considered to be a poor choice at the specified /coordinates/ in the specified /timetable/,
	which permits exploration of a wider solution-space than 'Implementation.DeterministicConstructor.mutateStudentViewTimetable'.
-}
mutateStudentViewTimetable :: forall campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio randomGen stream synchronisationId teacherId teachingRatio timeslotId. (
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
	Show			timeslotId,
	System.Random.RandomGen	randomGen
 )
	=> randomGen
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level	-- ^ The initial /timetable/ on which to base the mutation.
	-> StudentView.TimetableCoordinates.Vector timeslotId				-- ^ The ordered list of /coordinates/ to visit.
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
mutateStudentViewTimetable randomGen problemParameters executionOptions problemAnalysis studentViewTimetable	= Dynamic.HumanViewTimetable.getStudentViewTimetable . fst {-humanViewTimetablePair-} . foldr (
	\studentViewTimetableCoordinates visitor@(humanViewTimetablePair, randomGen')	-> let
		studentBody	= Model.TimetableCoordinates.getObserverId studentViewTimetableCoordinates	-- Deconstruct.
	in if Model.Timetable.isDefinedTimeslot studentViewTimetableCoordinates $ Dynamic.HumanViewTimetable.getStudentViewTimetable humanViewTimetablePair
		then visitor	-- No change.
		else Control.Arrow.first (
			\randomGen''	-> let
				humanViewTimetablePairs :: [Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level]
				humanViewTimetablePairs	= Data.Maybe.mapMaybe (
					($ humanViewTimetablePair) . uncurry (
						Dynamic.StudentViewTimetableUtilities.bookAtomicallyWithRamifications (
							bookAtomicallySynchronisedLessons randomGen''	-- Continuation.
						) problemParameters executionOptions problemAnalysis	-- Either make all or none of the bookings mandated by this lesson's course.
					) . (
						ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters &&& (,) studentViewTimetableCoordinates {-construct a booking-}
					)
				 ) . ToolShed.System.Random.shuffle randomGen'' . filter (
					(
						`Data.Set.member` (
							Data.Map.map Data.Student.amalgamateKnowledgeRequirements (
								Dynamic.StudentViewTimetableUtilities.findIncompletelyBookedKnowledgeRequirementsByStudentBody problemParameters studentViewTimetable
							) ! studentBody
						)
					) . Model.Lesson.getSubject	-- For efficiency, select only those subjects which are still required.
				 ) $ ProblemConfiguration.ProblemAnalysis.getRequiredLessonCombinationsByStudentBodyAvailableByDay problemAnalysis ! Temporal.Time.getDay (
					Model.TimetableCoordinates.getTime studentViewTimetableCoordinates
				 ) ! studentBody
			in if null humanViewTimetablePairs
				then humanViewTimetablePair	-- No change; other than to the random-generator.
				else head humanViewTimetablePairs
		) $ System.Random.split randomGen'	-- Forward an unused random-generator.
 ) (
	Dynamic.HumanViewTimetable.mkTimetablePair executionOptions problemAnalysis studentViewTimetable,
	randomGen
 ) -- Initial value.

