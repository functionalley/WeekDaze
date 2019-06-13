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

	* Find a /timetable/ based on the one supplied, by /booking/ unallocated /time-slot/s,
	with /lesson/s which match the /problem-parameters/,
	using a purely deterministic algorithm.

	* Defines the /fitness function/ of an /evolutionary algorithm/; <https://en.wikipedia.org/wiki/Evolutionary_algorithm>.
-}

module WeekDaze.Implementation.DeterministicConstructor(
-- * Types
-- ** Type-synonyms
--	HumanViewTimetablePairFitness,
	LessonCriteriaStatistics,
-- * Functions
--	bookAtomicallySynchronisedLessons,
--	bookAtomicallyAndEvaluateFitness,
--	getHumanViewTimetableFitnessFromSuitableAvailableLessonCombinations,
--	mutator,
	mutateStudentViewTimetable,
-- ** Accessors
--	getWriter,
--	getWeightedMean,
	getLessonCriteriaStatistics,
--	getLessonCriterionBounds
) where

import			Control.Arrow((&&&), (***))
import			Data.Map((!))
import qualified	Control.Arrow
import qualified	Control.Monad.Writer	-- The lazy instance.
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	Data.Set
import qualified	Factory.Math.Statistics
import qualified	WeekDaze.Aggregate.LocationCatalogue			as Aggregate.LocationCatalogue
import qualified	WeekDaze.Aggregate.StudentBody				as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentClass				as Aggregate.StudentClass
import qualified	WeekDaze.Data.Course					as Data.Course
import qualified	WeekDaze.Data.Location					as Data.Location
import qualified	WeekDaze.Data.Requirements				as Data.Requirements
import qualified	WeekDaze.Data.Resource					as Data.Resource
import qualified	WeekDaze.Data.Student					as Data.Student
import qualified	WeekDaze.Data.Subject					as Data.Subject
import qualified	WeekDaze.Data.Teacher					as Data.Teacher
import qualified	WeekDaze.Dynamic.HumanViewTimetable			as Dynamic.HumanViewTimetable
import qualified	WeekDaze.Dynamic.StudentViewTimetableUtilities		as Dynamic.StudentViewTimetableUtilities
import qualified	WeekDaze.Dynamic.TimetableForWeekUtilities		as Dynamic.TimetableForWeekUtilities
import qualified	WeekDaze.ExecutionConfiguration.Criterion		as ExecutionConfiguration.Criterion
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.ExecutionConfiguration.LessonCriteriaWeights	as ExecutionConfiguration.LessonCriteriaWeights
import qualified	WeekDaze.Model.Lesson					as Model.Lesson
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.Model.TimetableCoordinates			as Model.TimetableCoordinates
import qualified	WeekDaze.Model.TimetableForDay				as Model.TimetableForDay
import qualified	WeekDaze.Model.TimetableForWeek				as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.Size						as Size
import qualified	WeekDaze.StudentView.LessonResourceIds			as StudentView.LessonResourceIds
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.StudentView.TimetableCoordinates		as StudentView.TimetableCoordinates
import qualified	WeekDaze.TeacherView.LessonResourceIds			as TeacherView.LessonResourceIds
import qualified	WeekDaze.Temporal.Availability				as Temporal.Availability
import qualified	WeekDaze.Temporal.Time					as Temporal.Time
import qualified	WeekDaze.Temporal.TimeslotRequest			as Temporal.TimeslotRequest

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<*>))
#endif

{- |
	A /writer/ based on a /human-view timetable-pair/ including a specific /booking/ (which logs the associated /lesson-criteria/ values),
	paired with the /weighted mean/ over all /lesson-criteria/, as a measure of the fitness of that /booking/.
-}
type HumanViewTimetablePairFitness criterionValue timeslotId locationId teacherId level weightedMean	= (
	Control.Monad.Writer.Writer [Maybe criterionValue] (Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level),
	weightedMean
 ) -- Pair.

-- | Accessor.
getWriter :: HumanViewTimetablePairFitness criterionValue timeslotId locationId teacherId level weightedMean -> Control.Monad.Writer.Writer [Maybe criterionValue] (Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level)
getWriter	= fst

-- | Accessor. Gets the weighted mean over all /lesson-criteria/ for a specific /booking/.
getWeightedMean :: HumanViewTimetablePairFitness criterionValue timeslotId locationId teacherId level weightedMean -> weightedMean
getWeightedMean	= snd

{- |
	* If the specified /lesson/ (which is assumed to have already been booked), is a member of a set of /synchronised courses/,
	then where required, attempt to book synchronous /lesson/s for the other /teacher/s.

	* The /subject/ & /teacher/ of any additional /lesson/s is defined by their respective synchronised /course/s.
	The /locationId/ & /student-body/ for any additional /lesson/s are selected to maximise the mean over all /lesson-criteria/.

	* CAVEAT: any additional /booking/s are delegated to 'bookAtomicallyAndEvaluateFitness',
	which accounts for further ramifications; e.g. other /time/s specified by the /course/, & non-trivial /minimum consecutive lessons/.

	* CAVEAT: before changing, review the namesake 'Implementation.RandomConstructor.bookAtomicallySynchronisedLessons'.
-}
bookAtomicallySynchronisedLessons :: forall campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId. (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			campus,
	Ord			level,
	Ord			locationId,
	Ord			stream,
	Ord			synchronisationId,
	Ord			teacherId,
	Real			criterionWeight,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 ) => Dynamic.StudentViewTimetableUtilities.BookAtomically campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
bookAtomicallySynchronisedLessons problemParameters@ProblemConfiguration.ProblemParameters.MkProblemParameters {
	ProblemConfiguration.ProblemParameters.getLocationCatalogue	= locationCatalogue,
	ProblemConfiguration.ProblemParameters.getStudentBodyRegister	= studentBodyRegister,
	ProblemConfiguration.ProblemParameters.getTeacherRegister	= teacherRegister
} executionOptions problemAnalysis course studentViewBooking {-already booked-} humanViewTimetablePair	= Data.Maybe.maybe (
	Just humanViewTimetablePair	-- Since it's not a synchronised course, & the original booking has already been made, no further action is required.
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
			not $ all isAvailable synchronisedTeacherProfiles,	-- Some of the required teachers of this set of synchronised courses, are unavailable on the required day.
			any (
				Data.Foldable.any (Data.Course.isASpecifiedTime time) . Data.Set.filter (
					(/= Just synchronisationId) . Data.Course.getMaybeSynchronisationId	-- Consider only other courses.
				) . Data.Teacher.getService
			) synchronisedTeacherProfiles,	-- Some of the required teachers of this set of synchronised courses, are already committed at the required time; even if it's not yet booked.
			not . Data.Foldable.all isAvailable $ Data.Map.filter (
				Data.Student.requiresAnySubjectBy (
					`Data.Foldable.elem` Data.Map.map Data.Course.getSubject requiredSynchronisedCoursesByTeacherId
				)
			) studentBodyRegister	-- Some interested student-bodies are unavailable. TODO: inessential; since not all student-bodies must attend each lesson simultaneously with all other bodies.
		]							= Nothing			-- Some of the resources are unavailable or otherwise committed.
		| Data.Map.null requiredSynchronisedCoursesByTeacherId	= Just humanViewTimetablePair	-- Lessons for all the synchronised courses have already been booked at the required time.
		| null humanViewTimetablePairFitnessList		= Nothing			-- No viable timetable result from booking a lesson for each of the synchronised courses.
		| otherwise						= Just . fst {-timetable-pair-} . Control.Monad.Writer.runWriter . getWriter $ Data.List.maximumBy (
			Data.Ord.comparing getWeightedMean	-- Select the fittest timetable-pair according to the weighted mean over lesson-criteria.
		) humanViewTimetablePairFitnessList
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
			humanViewTimetablePairFitnessList :: [HumanViewTimetablePairFitness Rational timeslotId locationId teacherId level Rational]
			humanViewTimetablePairFitnessList	= Data.Map.foldr (
				\unaryBookingFunctions -> Data.Maybe.catMaybes {-forward viable permutations to the next teacher-} . (
					unaryBookingFunctions <*>	-- Generate permutations of the list of unary functions applied to the list of possible Human-view TimetablePairs.
				) . map (
					fst {-timetable-pair-} . Control.Monad.Writer.runWriter . getWriter	-- Retrieve the Human-view Timetable-pair.
				)
			 ) [
				(
					return {-to Writer-monad-} humanViewTimetablePair,
					undefined	-- The initial fitness isn't used.
				) -- HumanViewTimetablePairFitness.
			 ] {-Initial value-} $ Data.Map.mapWithKey (
				\teacherId synchronisedCourse	-> [
					bookAtomicallyAndEvaluateFitness Dynamic.StudentViewTimetableUtilities.bookAtomicallyNothing {-continuation. CAVEAT: don't call 'bookAtomicallySynchronisedLessons' recursively, which would book lessons, that this call has identified as required-} problemParameters executionOptions problemAnalysis synchronisedStudentViewBooking |	-- Construct a unary booking-function by partial application.
						let subject	= Data.Course.getSubject synchronisedCourse,	-- Find the single subject offered by this teacher of one of the synchronised courses.
						studentBody	<- Data.Map.keys $ Data.Map.filter (
							Data.Set.member subject . Data.Student.deriveAmalgamatedKnowledgeRequirement
						) studentBodyRegister,	-- Find the student-bodies requiring the subject offered by this teacher of the synchronised courses.
						studentBody /= bookedStudentBody,	-- The synchronised lessons can't involve the same studentBody as the original 'studentViewBooking'.
						not . uncurry (||) . (
							Model.TimetableForWeek.isDefinedTimeslot time {-check the student-body isn't already booked-} &&& Model.TimetableForDay.isSubjectBooked subject . (Data.Array.IArray.! day)
						) $ studentViewTimetable ! studentBody,
						not $ ProblemConfiguration.ProblemAnalysis.doAllCoursesSatifyingAnotherKnowledgeRequirementSpecifyThisTime problemParameters problemAnalysis subject studentViewTimetableCoordinates,	-- Avoid booking-times specified by all courses that match one of the other knowledge-requirements of this student-body. TODO: what if they're already booked ?
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
	* Makes the proposed /booking/ in the /timetable/.

	* Occasionally additional /lesson/s may also be booked as mandated by the corresponding /course/.
	e.g. when @ Data.Course.requiresConsecutiveLessons || Data.Course.specifiesTimes || Data.Course.isSynchronised @.
	If any of the additional /booking/s can't be made, then 'Nothing' is returned.

	* Evaluates the fitness of the proposed /booking/, according to the weighted mean over heterogeneous /lesson-criteria/.

	* Returns the resulting /timetable/, & also writes the value of each /lesson-criterion/, to facilitate subsequent analysis.
-}
bookAtomicallyAndEvaluateFitness :: forall campus criterionValue criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId weightedMean. (
	Data.Array.IArray.Ix	timeslotId,
	Enum			criterionValue,
	Enum			timeslotId,
	Eq			campus,
	Fractional		criterionValue,
	Fractional		weightedMean,
	Ord			level,
	Ord			locationId,
	Ord			stream,
	Ord			synchronisationId,
	Ord			teacherId,
	Real			criterionValue,
	Real			criterionWeight,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> Dynamic.StudentViewTimetableUtilities.BookAtomically campus criterionWeight fecundityDecayRatio level locationId populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Continuation.
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Booking timeslotId locationId teacherId level						-- ^ The proposed /booking/.
	-> Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level				-- ^ The original /timetable/ on which to operate, as seen from two points of view.
	-> Maybe (HumanViewTimetablePairFitness criterionValue timeslotId locationId teacherId level weightedMean)	-- ^ The resulting /timetable/ including the /booking/, & the value each /lesson-criterion/; paired with weighted mean over all /lesson-criteria/.
bookAtomicallyAndEvaluateFitness continuation problemParameters@ProblemConfiguration.ProblemParameters.MkProblemParameters {
	ProblemConfiguration.ProblemParameters.getLocationCatalogue	= locationCatalogue,
	ProblemConfiguration.ProblemParameters.getStudentBodyRegister	= studentBodyRegister,
	ProblemConfiguration.ProblemParameters.getTeacherRegister	= teacherRegister
} executionOptions problemAnalysis studentViewBooking@((studentBody, time), studentViewLesson) humanViewTimetablePair	= (
	\proposedHumanViewTimetable -> let
		proposedStudentClass :: Aggregate.StudentClass.StudentClass
		proposedStudentClass	= StudentView.Timetable.extractStudentClassAt (time, studentViewLesson) $ Dynamic.HumanViewTimetable.getStudentViewTimetable proposedHumanViewTimetable

		studentClassSize :: Size.NStudents
		studentClassSize	= Aggregate.StudentClass.getSize proposedStudentClass

		wastedResources :: Data.Location.WastedResources
		wastedResources	= Data.Location.calculateWaste studentClassSize (Data.Course.getRequiredFacilityNames course) locationProfile

		(relativeCapacityUtilisation, relativeFacilityUtilisation)	= Data.Location.calculateRelativeUse locationProfile wastedResources

		nTeachers :: Size.NTeachers
		nTeachers	= ProblemConfiguration.ProblemAnalysis.getNTeachers problemAnalysis

-- Calculate the weighted mean over all lesson-criteria, & log the unweighted values of lesson-criteria.
		(weightedMean, lessonCriteriaValues)	= Control.Monad.Writer.runWriter $ ExecutionConfiguration.LessonCriteriaWeights.calculateWeightedMean (
			ExecutionConfiguration.ExecutionOptions.getLessonCriteriaWeights executionOptions
		 ) (
{-
	The question is whether or not the proposed lesson can be temporarily merged to form a student-class with any of the incumbent synchronous lessons,
	rather than how big the resulting student-class is; so the criterion-value has a binary rather than a linear range.
-}
			ExecutionConfiguration.Criterion.mkCriterionFrom $ studentViewLesson `elem` Model.Timetable.extractSynchronousLessonsAt time studentViewTimetable {-original-}	-- areResourcesReused.
		 ) (
			if nTimeslotsPerDay == 1
				then minBound	-- The concept is voidm & would evaluate to 0/0.
				else ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.LessonCriteriaWeights.weightOfGreatestMinimumConsecutiveLessonsTag . (
					/ fromIntegral (pred nTimeslotsPerDay)	-- The booking-difficulty, is inversely proportional to the number of timeslots per day.
				) . fromIntegral $ pred {-the fewest is one-} minimumConsecutiveLessons
		 ) (
			let
				simultaneousAvailability	= Data.Resource.getAvailability (teacherProfile, studentProfile)	-- Find the days on which both student-body & teacher are available.
			in if Temporal.Availability.isUnavailable simultaneousAvailability
				then error $ "WeekDaze.Implementation.DeterministicConstructor.bookAtomicallyAndEvaluateFitness:\tstudent-body & teacher are never simultaneously available for the tuition of a subject; " ++ shows (studentBody, studentViewLesson) "."
				else ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.LessonCriteriaWeights.weightOfGreatestRemainingCourseLessonsTag . min 1 $ (
					fromIntegral (
						(
							Data.Course.getRequiredLessonsPerWeek course - Model.TimetableForWeek.countSubjectWorkload subject studentViewTimetableForWeek {-original-}
						) `div` minimumConsecutiveLessons -- The number of sessions still to be booked.
					) / fromIntegral (
						Data.Set.size . Data.Set.filter (
							not . Model.TimetableForDay.isSubjectBooked subject . (
								studentViewTimetableForWeek Data.Array.IArray.!
							)
						) $ Temporal.Availability.deconstruct simultaneousAvailability
					) -- The number of days on which this subject isn't booked; which must include today or we wouldn't be here.
				)
		 ) (
			Data.Maybe.maybe minBound (
				ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.LessonCriteriaWeights.weightOfGreatestSynchronisedCourseSetSizeTag . (
					/ fromIntegral (pred nTeachers) -- Normalise; each teacher can offer one course in each synchronised set.
				) . fromIntegral . pred {-the fewest teachers is two-} . Data.Map.size {-number of teachers-} . (
					ProblemConfiguration.ProblemAnalysis.getCoursesByTeacherIdBySynchronisationId problemAnalysis !
				)
			 ) $ Data.Course.getMaybeSynchronisationId course
		 ) (
			ExecutionConfiguration.Criterion.mkCriterionFrom . Data.Set.member subject . Data.Requirements.getCore $ Data.Student.getKnowledgeRequirements studentProfile	-- isCoreKnowledgeRequirement.
		 ) (
			ExecutionConfiguration.Criterion.mkCriterionFrom $ Data.Teacher.isSpecialistIn (Data.Subject.getTopic subject) teacherProfile	-- isSpecialistInTopic.
		 ) (
			Data.Maybe.maybe ExecutionConfiguration.Criterion.median {-this course has no maximum class-size-} (
				\maximumClassSize -> let
					numerator, denominator :: Size.NStudents
					(numerator, denominator)	= abs {-symmetrical-} . uncurry (-) &&& uncurry (+) $ (
						pred {-the minimum is one-} $ Data.Location.getCapacity locationProfile,
						pred {-the minimum is one-} maximumClassSize
					 ) -- Pair.
				in if denominator == 0
					then maxBound	-- The location-capacity & maximum class-size for the course, must both equal one.
					else ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.LessonCriteriaWeights.weightOfMatchCourseClassSizeToLocationCapacityTag $ fromIntegral numerator / fromIntegral denominator	-- '1' if maximum course-size matches the location-capacity, tending towards '0' as the discrepancy rises. It doesn't matter if the course-size exceeds location-capacity, because it doesn't reflect the actual class-size.
			) maybeMaximumClassSizeOfCourse
		 ) (
			ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.LessonCriteriaWeights.weightOfMaximiseRelativeFacilityUtilisationTag relativeFacilityUtilisation
		 ) (
			Data.Maybe.maybe ExecutionConfiguration.Criterion.median {-this course has no maximum class-size-} (
				ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.LessonCriteriaWeights.weightOfMaximiseStudentClassSizeOverCourseClassSizeTag . (fromIntegral studentClassSize /) . fromIntegral
			) maybeMaximumClassSizeOfCourse -- Incompatible with 'ExecutionConfiguration.ExecutionOptions.setPermitTemporaryStudentBodyMerger', which allows this ratio to change.
		 ) (
			ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.LessonCriteriaWeights.weightOfMaximiseStudentClassSizeOverLocationCapacityTag relativeCapacityUtilisation	-- Incompatible with 'ExecutionConfiguration.ExecutionOptions.setPermitTemporaryStudentBodyMerger', which allows this ratio to change.
		 ) (
			let
				calculateProbabilitiesTimeNotRequired studentBody'	= map (
					(
						1 -	-- The complementary probability that a course is selected for the specified subject which doesn't require the proposed time.
					) . uncurry (
						/	-- The probability that a course is selected for the specified subject which requires the proposed time.
					) . (
						fromIntegral . Data.Map.size . Data.Map.filter (
							Data.Course.isASpecifiedTime time
						) &&& fromIntegral . Data.Map.size {-there's @ least one-}
					) . (
						ProblemConfiguration.ProblemAnalysis.getSuitableCoursesByTeacherIdBySubjectByStudentBody problemAnalysis ! studentBody' !
					)
				 ) . Data.Set.toList {-avoid accidentally merging equal members-}
			in ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseBookingAtAnotherCoursesSpecifiedTimeTag . product {-the probability that no other course requires the proposed time-} . Data.Maybe.maybe (
				calculateProbabilitiesTimeNotRequired studentBody . Data.Set.filter (
					/= Model.Lesson.getSubject studentViewLesson	-- Find the other subjects required by this student-body.
				) . Data.Student.deriveAmalgamatedKnowledgeRequirement $ studentBodyRegister ! studentBody
			) (
				\synchronisationId -> concatMap (
					\synchronisedStudentBody -> calculateProbabilitiesTimeNotRequired synchronisedStudentBody . Data.Set.filter (
						not . Data.Foldable.any (
							(== Just synchronisationId) . Data.Course.getMaybeSynchronisationId
						) . (
							ProblemConfiguration.ProblemAnalysis.getSuitableCoursesByTeacherIdBySubjectByStudentBody problemAnalysis ! synchronisedStudentBody !
						)
					) . Data.Student.deriveAmalgamatedKnowledgeRequirement $ studentBodyRegister ! synchronisedStudentBody
				) . Data.Set.toList $ ProblemConfiguration.ProblemAnalysis.getStudentBodiesBySynchronisationId problemAnalysis ! synchronisationId
			) $ Data.Course.getMaybeSynchronisationId course
		 ) (
			if nTeachers <= 1
				then minBound	-- There're zero other teachers, so the concept is void.
				else if Data.Set.null {-no choice of location for this subject-} . Data.Set.delete locationId . Data.Set.fromList . map (
					StudentView.LessonResourceIds.getLocationId . Model.Lesson.getResourceIds	-- Catalogue alternative locations @ which this subject may be taught.
				) . filter (
					(== subject) . Model.Lesson.getSubject
				) $ ProblemConfiguration.ProblemAnalysis.getRequiredLessonCombinationsByStudentBody problemAnalysis ! studentBody {-disregard availability, because if preferable we can opt to book another day-}
					then maxBound	-- There's no choice of location for this subject, so don't penalise this lesson if the location is shared, wrt another lesson where it isn't.
					else ExecutionConfiguration.Criterion.invertWholeNumbersIntoUnitInterval ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseBookingOfLocationByOtherTeachersTag . fromIntegral . Data.Set.size . Data.Set.delete teacherId $ StudentView.Timetable.findDistinctTeacherIdsFor locationId studentViewTimetable	-- Count the other teachers currently booked.
		 ) (
{-
	In order of preference, book a lesson in a course that:
		GOOD;		is either at a time specified for the course, or can span all the specified times for today using minimumConsecutiveLessons,
				or is near enough any requested ideal timeslotId to span the gap using minimumConsecutiveLessons (missing the ideal timeslot may be as good as a mile, so a binary rather than a linear range).
		INDIFFERENT;	hasn't a relevant timeslot-request.
		BAD;		is on the same day as a time precisely specified for the course, but at a different timeslotId which can span to the specified timeslot using minimumConsecutiveLessons.
				or is too far from any requested ideal timeslotId to span the gap using minimumConsecutiveLessons.

	N.B.: As soon as any booking is requested, for a course with either specified times or minimumConsecutiveLessons > 1,
	then either the remainder will be booked, or none will be booked.
-}
			let
				canSpan	= (< minimumConsecutiveLessons) . Temporal.Time.calculateAbsoluteDistance timeslotId	-- Check whether the specified timeslotId can span to the current timeslotId using minimumConsecutiveLessons.
			in case timeslotRequest of
				Temporal.TimeslotRequest.Ideally idealTimeslotId				-> ExecutionConfiguration.Criterion.mkCriterionFrom $ canSpan idealTimeslotId	-- If the ideal timeslot has already been booked, then a similar lesson won't be proposed for the same day, so we'll never accidentally create an excessive runlength.
				Temporal.TimeslotRequest.Specifically specificallyRequestedTimes {-which may be null-}
					| Temporal.TimeslotRequest.isASpecifiedTime time timeslotRequest	-> maxBound	-- Most desirable.
					| Temporal.TimeslotRequest.isASpecifiedDay day timeslotRequest		-> ExecutionConfiguration.Criterion.mkCriterionFrom . uncurry (&&) . (
						canSpan . Data.Set.findMin &&& canSpan . Data.Set.findMax
					) . Data.Set.map Temporal.Time.getTimeslotId $ Data.Set.filter (
						(== day) . Temporal.Time.getDay	-- There must be @ least one according to 'isASpecifiedDay'.
					) specificallyRequestedTimes	-- Discourage booking on the same day, but at a different time-slot from a time specified for this course, which can't span to specified times by minimumConsecutiveLessons.
					| otherwise								-> ExecutionConfiguration.Criterion.median	-- 'ProblemConfiguration.ProblemAnalysis.getRequiredLessonCombinationsByStudentBodyAvailableByDay' doesn't return a lesson from rigid courses on the wrong day.
		 ) (
			ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseInterCampusMigrationsOfStudentsTag . (
				/ 2	-- Normalise.
			) . fromIntegral . countTemporallyAdjacentCampusMigrations $ Dynamic.TimetableForWeekUtilities.extractTemporallyAdjacentCampuses problemParameters studentProfile StudentView.LessonResourceIds.getLocationId time studentViewTimetableForWeek {-the original-}
		 ) (
			ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseInterCampusMigrationsOfTeachersTag . (
				/ 2	-- Normalise.
			) . fromIntegral . countTemporallyAdjacentCampusMigrations . Dynamic.TimetableForWeekUtilities.extractTemporallyAdjacentCampuses problemParameters teacherProfile TeacherView.LessonResourceIds.getLocationId time $ teacherViewTimetable {-the original-} ! teacherId
		 ) (
			Data.Maybe.maybe maxBound {-this student-body hasn't previously booked any lessons of this type-} (
				ExecutionConfiguration.Criterion.mkCriterionFrom . Data.Set.member proposedStudentClass	-- The number of student-body combinations isn't relevant, only whether this lesson makes it worse; i.e. a binary rather than a linear range.
			) . Data.Map.lookup studentViewLesson $ StudentView.Timetable.findStudentClassesByLessonFor studentViewTimetable studentBody	-- studentBodyCombinations.
		 ) (
			let
				distinctLocationIds	= StudentView.Timetable.findDistinctLocationIdsFor teacherId studentViewTimetable
			in if Data.Set.null distinctLocationIds
				then maxBound	-- This teacher hasn't made any bookings.
				else ExecutionConfiguration.Criterion.mkCriterionFrom $ Data.Set.member locationId distinctLocationIds	-- teachersLocusOperandi. Otherwise the teacher is using a new location. The number of other teachers isn't relevant, only whether this lesson makes it worse; i.e. a binary rather than a linear range.
		 ) (
			let
{-
	For each facility wasted, by holding a lesson in this course at this location;
	the demand for it, is the sum over courses which require it, of Data.Course.getRequiredLessonsPerweek;
	the supply of it, is the sum over locations which offer it, of available timeslots / week.

	The total of this ratio over all facilities, should then be normalised by the number of distinct facilities.
-}
				denominator :: Int
				denominator	= nTimeslotsPerDay * Data.Set.size (Aggregate.LocationCatalogue.extractDistinctFacilityNames locationCatalogue)
			in if denominator == 0
				then minBound	-- There is no concept of this criterion, for any lesson.
				else ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseWasteOfScarceFacilitiesTag {-low waste is good-} . (
					/ fromIntegral denominator	-- Normalise to the unit-interval.
				) . Data.Set.foldr (
					\facilityName -> (
						(
							fromIntegral (
								Data.Maybe.fromMaybe 0 . Data.Map.lookup facilityName $ ProblemConfiguration.ProblemAnalysis.getTotalLessonsPerWeekByFacilityName problemAnalysis	-- The demand for this facility from courses; redundant courses have been removed by 'ProblemConfiguration.ProblemParameters.removeRedundantCourses'.
							) / fromIntegral (
								Aggregate.LocationCatalogue.countDaysByFacilityName locationCatalogue ! facilityName	-- The supply of this facility by locations; which must exceed zero.
							)
						) +
					) -- Section.
				) 0 $ Data.Location.getWastedFacilityNames wastedResources
		 )
	in (
		Control.Monad.Writer.writer (
			proposedHumanViewTimetable,
			lessonCriteriaValues
		),		-- The timetable including the proposed booking.
		weightedMean	-- The fitness of the proposed booking.
	) -- HumanViewTimetablePairFitness.
 ) `fmap` Dynamic.StudentViewTimetableUtilities.bookAtomicallyWithRamifications continuation problemParameters executionOptions problemAnalysis course studentViewBooking humanViewTimetablePair where
	nTimeslotsPerDay :: Size.NTimeslots
	nTimeslotsPerDay	= ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis

	(studentViewTimetable, teacherViewTimetable)	= Dynamic.HumanViewTimetable.getStudentViewTimetable &&& Dynamic.HumanViewTimetable.getTeacherViewTimetable $ humanViewTimetablePair	-- Deconstruct.

	(((studentViewTimetableForWeek, studentProfile), (day, timeslotId)), (subject, (locationId, teacherId)))	= (
		(
			(studentViewTimetable !) &&& (studentBodyRegister !)
		) *** (
			Temporal.Time.getDay &&& Temporal.Time.getTimeslotId
		)
	 ) *** (
		Model.Lesson.getSubject &&& (
			StudentView.LessonResourceIds.getLocationId &&& StudentView.LessonResourceIds.getTeacherId
		) . Model.Lesson.getResourceIds
	 ) $ studentViewBooking	-- Deconstruct.

	locationProfile	= locationCatalogue ! locationId	-- Lookup.
	teacherProfile	= teacherRegister ! teacherId		-- Lookup.

	course				= Data.Maybe.fromJust $ Data.Teacher.lookupCourseIn subject teacherProfile
	maybeMaximumClassSizeOfCourse	= Data.Course.getMaybeMaximumClassSize course
	minimumConsecutiveLessons	= Data.Course.getMinimumConsecutiveLessons course
	timeslotRequest			= Data.Course.getTimeslotRequest course

	countTemporallyAdjacentCampusMigrations :: (Maybe campus, Maybe campus) -> Int
	countTemporallyAdjacentCampusMigrations (maybeCampusBefore, maybeCampusAfter)	= length $ filter (
		/= Data.Location.getCampus locationProfile	-- Mismatch.
	 ) $ Data.Maybe.catMaybes {-an unallocated time-slot doesn't constitute a mismatch-} [maybeCampusBefore, maybeCampusAfter]

{- |
	* Transforms the specified /timetable/, by independently /booking/ each suitable & available /lesson/ into the specified /coordinate/s, to create a population of candidate /timetable/s.

	* Avoids /booking/s which conflict with /time/s specified for other /course/s, which are either;
	offered by the /teacher/;
	or necessary to satisy the other /subject/s required by the /student-body/.

	* For each alternative /timetable/;
		the individual weighted /lesson-criteria/ are written to facilitate analysis,
		& the weighted mean of /lesson-criteria/ is returned to facilitate selection.
-}
getHumanViewTimetableFitnessFromSuitableAvailableLessonCombinations :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			criterionValue,
	Enum			timeslotId,
	Eq			campus,
	Fractional		criterionValue,
	Fractional		weightedMean,
	Ord			level,
	Ord			locationId,
	Ord			stream,
	Ord			synchronisationId,
	Ord			teacherId,
	Real			criterionValue,
	Real			criterionWeight,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.TimetableCoordinates.Coordinates timeslotId						-- ^ The coordinates of the /time-slot/ at which to book a suitable available /lesson/.
	-> Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level			-- ^ The /timetable/ on which to operate.
	-> [HumanViewTimetablePairFitness criterionValue timeslotId locationId teacherId level weightedMean]	-- ^ The resulting population of candidate /timetable/s, each paired with a fitness-evaluation; each including a log of individual /lesson-criteria/ values.
getHumanViewTimetableFitnessFromSuitableAvailableLessonCombinations problemParameters executionOptions problemAnalysis studentViewTimetableCoordinates humanViewTimetablePair	= Data.Maybe.mapMaybe (
	($ humanViewTimetablePair) . bookAtomicallyAndEvaluateFitness bookAtomicallySynchronisedLessons problemParameters executionOptions problemAnalysis . (,) studentViewTimetableCoordinates {-construct a booking from lesson-}
 ) . filter (
	(
		\(subject, teacherId)	-> not $ or [

-- Avoid subjects which are already completely booked.
			Data.Set.notMember subject $ Data.Map.map Data.Student.amalgamateKnowledgeRequirements (
				Dynamic.StudentViewTimetableUtilities.findIncompletelyBookedKnowledgeRequirementsByStudentBody problemParameters studentViewTimetable	-- Includes subjects which aren't booked at all.
			) ! studentBody,
{-
	Avoid subjects already booked today, since any additionally required to meet 'minimumConsecutiveLessons' are booked automatically with the first for the day.
	CAVEAT: subjects already booked today, for any synchronised courses, are checked separately in 'bookAtomicallySynchronisedLessons'.
-}
			Model.TimetableForDay.isSubjectBooked subject $ studentViewTimetableForWeek Data.Array.IArray.! day,

-- Avoid booking-times specified by another course offered by this teacher.
			Data.Foldable.any (
				Data.Course.isASpecifiedTime time
			) . Data.Set.filter (
				(/= subject) . Data.Course.getSubject	-- Find the other courses offered by this teacher.
			) . Data.Teacher.getService $ ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters ! teacherId,

-- Avoid booking-times specified by all courses that match one of the other knowledge-requirements of this student-body.
			ProblemConfiguration.ProblemAnalysis.doAllCoursesSatifyingAnotherKnowledgeRequirementSpecifyThisTime problemParameters problemAnalysis subject studentViewTimetableCoordinates
		] -- Quickly select those lessons, which can be independently booked into this timetable at the specified coordinates.
	) . (
		Model.Lesson.getSubject &&& StudentView.LessonResourceIds.getTeacherId . Model.Lesson.getResourceIds	-- Deconstruct the lesson.
	)
 ) $ ProblemConfiguration.ProblemAnalysis.getRequiredLessonCombinationsByStudentBodyAvailableByDay problemAnalysis ! day ! studentBody where
	(studentBody, time)		= Model.TimetableCoordinates.getObserverId &&& Model.TimetableCoordinates.getTime $ studentViewTimetableCoordinates	-- Deconstruct.
	studentViewTimetable		= Dynamic.HumanViewTimetable.getStudentViewTimetable humanViewTimetablePair	-- Access.
	studentViewTimetableForWeek	= studentViewTimetable ! studentBody	-- Lookup.
	day				= Temporal.Time.getDay time	-- Access.

{- |
	* Books /lesson/s where possible (except if they're already booked or none are viable), into the specified /timetable/ at the specified /coordinates/.

	* The /lesson/ is selected to maximise the /weighted mean/ over all /lesson-criteria/.

	* The values of each /lesson-criterion/, are also written to facilitate post-analysis.
-}
mutator :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			criterionValue,
	Enum			timeslotId,
	Eq			campus,
	Fractional		criterionValue,
	Ord			level,
	Ord			locationId,
	Ord			stream,
	Ord			synchronisationId,
	Ord			teacherId,
	Real			criterionValue,
	Real			criterionWeight,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.TimetableCoordinates.Coordinates timeslotId				-- ^ The /coordinates/ at which to book the most suitable available /lesson/.
	-> Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level	-- ^ The /timetable/ on which to operate.
	-> Control.Monad.Writer.Writer [[Maybe criterionValue]] (Dynamic.HumanViewTimetable.TimetablePair timeslotId locationId teacherId level)
mutator problemParameters executionOptions problemAnalysis studentViewTimetableCoordinates humanViewTimetablePair
	| Model.Timetable.isDefinedTimeslot studentViewTimetableCoordinates studentViewTimetable	= return {-to Writer-monad-} humanViewTimetablePair	-- The specified coordinates are already booked, so no change.
	| not $ null humanViewTimetableFitnessFromSuitableAvailableLessonCombinations			= Control.Monad.Writer.mapWriter (
		Control.Arrow.second return {-to List-monad-}	-- Re-write the list of lesson-criteria values, corresponding to the lesson found to be most effective, when booked into specified coordinated of the timetable.
	) . getWriter $ Data.List.maximumBy (
		Data.Ord.comparing (
			\pair -> getWeightedMean pair :: Rational
		)
	) humanViewTimetableFitnessFromSuitableAvailableLessonCombinations
	| not $ null qualifiedHumanViewTimetablePairsFromSuitableAdjacentLessons			= return {-to Writer-monad-} . fst {-humanViewTimetablePair-} $ Data.List.maximumBy (
		Data.Ord.comparing snd {-runlength-shortfall-}
	) qualifiedHumanViewTimetablePairsFromSuitableAdjacentLessons
	| otherwise											= return {-to Writer-monad-} humanViewTimetablePair	-- Can't book anything into the specified coordinates.
	where
		(studentBody, (day, timeslotId))					= Model.TimetableCoordinates.getObserverId &&& (Temporal.Time.getDay &&& Temporal.Time.getTimeslotId) . Model.TimetableCoordinates.getTime $ studentViewTimetableCoordinates	-- Deconstruct.
		studentViewTimetable							= Dynamic.HumanViewTimetable.getStudentViewTimetable humanViewTimetablePair
		humanViewTimetableFitnessFromSuitableAvailableLessonCombinations	= getHumanViewTimetableFitnessFromSuitableAvailableLessonCombinations problemParameters executionOptions problemAnalysis studentViewTimetableCoordinates humanViewTimetablePair
		qualifiedHumanViewTimetablePairsFromSuitableAdjacentLessons		= Data.Maybe.mapMaybe (
			(
				\proposedBooking -> let
					course	= ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters $ Model.TimetableForDay.getBookedLesson proposedBooking
				in (
					id &&& (
						Data.Course.getMinimumConsecutiveLessons course -	-- Evaluate the signed runlength-shortfall; they should all be <= 0.
					) . Model.TimetableForDay.countRunlengthAt timeslotId . (
						Data.Array.IArray.! day	-- Extract the timetableForDay.
					) . (
						! studentBody	-- Extract the timetableForWeek.
					) . Dynamic.HumanViewTimetable.getStudentViewTimetable
				) `fmap` Dynamic.StudentViewTimetableUtilities.bookAtomicallyWithRamifications bookAtomicallySynchronisedLessons {-continuation-} problemParameters executionOptions problemAnalysis course proposedBooking humanViewTimetablePair	-- The adjacent lesson should already have been booked in a runlength of minimumConsecutiveLessons & for the times specified by the course, but it may also have synchronised courses which should be booked.
			) . (,) studentViewTimetableCoordinates . Model.TimetableForDay.getBookedLesson {-replicate the adjacent bookings @ the current coordinates-}
		 ) . Model.TimetableForDay.extractAdjacentBookings timeslotId $ studentViewTimetable ! studentBody Data.Array.IArray.! day

-- | The ((mean, standard deviation), (min, max)) over the values of a single /lesson-criterion/.
type LessonCriteriaStatistics criterionValue mean standardDeviation	= ((mean {-mean-}, standardDeviation), (criterionValue {-min-}, criterionValue {-max-}))	-- Pair.

-- | Accessor.
getLessonCriteriaStatistics :: LessonCriteriaStatistics criterionValue mean standardDeviation -> (mean, standardDeviation)
getLessonCriteriaStatistics	= fst

-- | Accessor.
getLessonCriterionBounds :: LessonCriteriaStatistics criterionValue mean standardDeviation -> (criterionValue {-min-}, criterionValue {-max-})
getLessonCriterionBounds	= snd

{- |
	* Books where possible, /lesson/s into the specified /timetable/, at unallocated /time-slot/s amongst the specified ordered /coordinates/.

	* The individual /lesson/s are selected to maximise the weighted mean of various /lesson-criteria/.

	* 'LessonCriteriaStatistics' gathered over all /booking/s, for each /lesson-criterion/ (unless the corresponding weight is zero), are also written to facilitate analysis.
-}
mutateStudentViewTimetable :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			criterionValue,
	Enum			timeslotId,
	Eq			campus,
	Floating		standardDeviation,
	Fractional		criterionValue,
	Fractional		mean,
	Ord			level,
	Ord			locationId,
	Ord			stream,
	Ord			synchronisationId,
	Ord			teacherId,
	Real			criterionValue,
	Real			criterionWeight,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level	-- ^ The initial /timetable/ to refine.
	-> StudentView.TimetableCoordinates.Vector timeslotId				-- ^ The ordered list of /coordinates/.
	-> Control.Monad.Writer.Writer [Maybe (LessonCriteriaStatistics criterionValue mean standardDeviation)] (
		StudentView.Timetable.Timetable timeslotId locationId teacherId level
	)										-- ^ The filled /timetable/, & the 'LessonCriteriaStatistics' gathered over all /booking/s, for each weighted /lesson-criterion/.
mutateStudentViewTimetable problemParameters executionOptions problemAnalysis studentViewTimetable	= Control.Monad.Writer.mapWriter (
	Dynamic.HumanViewTimetable.getStudentViewTimetable *** map (
		fmap (
			(Factory.Math.Statistics.getMean &&& Factory.Math.Statistics.getStandardDeviation) &&& (minimum &&& maximum)	-- Gather statistics over all selected bookings, for the value of each unweighted homogeneous lesson-criterion.
		) . sequence {-if the value of any lesson-criterion is unavailable, then the corresponding statistics are indeterminate-}
	) . Data.List.transpose
 ) . Data.List.foldl' {-not foldr, since the coordinates are ordered-} (
	\humanViewTimetableWriter -> (humanViewTimetableWriter >>=) . mutator problemParameters executionOptions problemAnalysis	-- Thread the writer through the coordinates, evolving the timetable & accumulating statistics for each lesson-criterion.
 ) (
	return {-to Writer-monad-} $ Dynamic.HumanViewTimetable.mkTimetablePair executionOptions problemAnalysis studentViewTimetable	-- The initial value.
 ) {-coordinate-vector-}

