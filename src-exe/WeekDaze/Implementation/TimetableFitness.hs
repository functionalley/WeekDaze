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

	* Defines the fitness of a /timetable/ for the given configuration,
	as required for selection within one generation of its evolution; <https://en.wikipedia.org/wiki/Evolutionary_algorithm>.
-}

module WeekDaze.Implementation.TimetableFitness(
-- * Functions
--	calculateRatioOfSeparatedEqualLessonsWithinAnyDayCriterion,
--	measureDeviationFromMinimumConsecutiveLessonsCriterion,
--	measureLocationChangesOfTeachers,
	evaluateFitness
) where

import			Control.Arrow((&&&))
import			Data.Map((!))
import qualified	Control.Monad.Writer	-- The lazy instance.
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	ToolShed.Data.List.Runlength
import qualified	WeekDaze.Aggregate.StudentBody					as Aggregate.StudentBody
import qualified	WeekDaze.Data.Course						as Data.Course
import qualified	WeekDaze.Data.Requirements					as Data.Requirements
import qualified	WeekDaze.Dynamic.StudentViewTimetableUtilities			as Dynamic.StudentViewTimetableUtilities
import qualified	WeekDaze.Dynamic.TeacherViewTimetableUtilities			as Dynamic.TeacherViewTimetableUtilities
import qualified	WeekDaze.ExecutionConfiguration.Criterion			as ExecutionConfiguration.Criterion
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions		as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.ExecutionConfiguration.TimetableCriteriaWeights	as ExecutionConfiguration.TimetableCriteriaWeights
import qualified	WeekDaze.Model.Timetable					as Model.Timetable
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis			as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters			as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.Size							as Size
import qualified	WeekDaze.StudentView.Timetable					as StudentView.Timetable
import qualified	WeekDaze.TeacherView.Timetable					as TeacherView.Timetable
import qualified	WeekDaze.Temporal.Day						as Temporal.Day

{- |
	* Counts the number of separated equal /lesson/s, occuring in any single /day/, across the whole /timetable/,
	discounting those implied by the specified /time/s for the /course/ to which they belong.

	* The number of such occurances is weighted by the number of /student/s in the body affected, & finally normalised to the unit-interval.
-}
calculateRatioOfSeparatedEqualLessonsWithinAnyDayCriterion :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		criterionValue,
	Ord			level,
	Ord			locationId,
	Ord			synchronisationId,
	Ord			teacherId,
	Real			criterionValue
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> ExecutionConfiguration.Criterion.Criterion criterionValue
calculateRatioOfSeparatedEqualLessonsWithinAnyDayCriterion problemParameters problemAnalysis
	| denominator == 0	= error "WeekDaze.Implementation.TimetableFitness.calculateRatioOfSeparatedEqualLessonsWithinAnyDayCriterion:\tattempt to divide by zero (time-slots)."
	| otherwise		= ExecutionConfiguration.Criterion.reflectUnitInterval "ratioOfSeparatedEqualLessonsWithinAnyDay" . (
		/ fromIntegral denominator	-- Normalise to the unit-interval.
	) . fromIntegral . Data.Map.foldrWithKey (
		\studentBody -> (+) . (
			* Aggregate.StudentBody.getSize studentBody	-- Weight according to the number of students affected.
		) . foldr (
			(+) . pred {-two is one too many-} . ToolShed.Data.List.Runlength.getLength
		) 0 . filter (
			(
				`Data.Set.notMember` ProblemConfiguration.ProblemAnalysis.getCoursesRequestingSeparatedLessonsWithinAnyDay problemAnalysis -- No course specifying times, separated within a day, deprecates them; unless it's possible to join them using a contiguous span.
			) . ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters . ToolShed.Data.List.Runlength.getDatum {-Lesson-}
		)
	) 0 . Model.Timetable.findSeparatedEqualLessonsWithinAnyDayByObserverId
	where
		denominator	= (
			ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis - 2		-- Concept doesn't exist until there're three timeslots/day.
		 ) * ProblemConfiguration.ProblemAnalysis.getNAvailableDaysPerStudentTimetable problemAnalysis

{- |
	* Totals over the whole /timetable/, the absolute deviation between the lengths of consecutive equal /lesson/s,
	& the minimum specified for the corresponding /course/.

	* /Course/s where 'Data.Course.getRequiredLessonsPerWeek' isn't an integral multiple of 'Data.Course.getMinimumConsecutiveLessons',
	would show a discrepancy for all /lesson/s,
	to avoid which, deviations smaller than half a /time-slot/ are ignored.

	* The deviation is weighted by the number of /student/s in the body affected, & finally normalised to the unit-interval.

	* When a /course/ specifies consecutive times, it is considered to tacitly approve runlengths of that magnitude.

	* If (requiredLessonsPerWeek `div` minimumConsecutiveLessons) for the course > nAvailableDays for the student, then bookings long tranches is unavoidable.
	This calculation is complicated because the /lesson/s which must be booked each day, (which may well exceed 'Data.Course.calculateIdealConsecutiveLessons'),
	might be partitioned into shorter tranches by other /lesson/s or unallocated /time-slots/, thus reducing the runlength.
	So in practice, there's typically no increase to 'Data.Course.calculateIdealConsecutiveLessons'.
-}
measureDeviationFromMinimumConsecutiveLessonsCriterion :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			locationId,
	Fractional		criterionValue,
	Ord			level,
	Ord			synchronisationId,
	Ord			teacherId,
	Real			criterionValue
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> ExecutionConfiguration.Criterion.Criterion criterionValue
measureDeviationFromMinimumConsecutiveLessonsCriterion problemParameters problemAnalysis
	| denominator == 0	= error "WeekDaze.Implementation.TimetableFitness.measureDeviationFromMinimumConsecutiveLessonsCriterion:\tattempt to divide by zero (time-slots)."
	| otherwise		= ExecutionConfiguration.Criterion.reflectUnitInterval "ratioOfConsecutiveEqualLessons" . (
		/ fromIntegral denominator	-- Normalise to the unit-interval by dividing by the worse possible case.
	) . fromIntegral . Data.Map.foldrWithKey (
		\studentBody -> (+) . (
			* Aggregate.StudentBody.getSize studentBody	-- Weight the discrepancy according to the number of students affected.
		) . Data.Foldable.foldr (
			flip $ foldr (
				\(_ {-timeslotId-}, generalisedLessonRunlengthCode)	-> let
					(runlength, generalisedLesson)	= ToolShed.Data.List.Runlength.getLength &&& ToolShed.Data.List.Runlength.getDatum $ generalisedLessonRunlengthCode	-- Deconstruct.
				in Data.Maybe.maybe id (
					\lesson -> let
						course	= ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters lesson
					in if Data.Set.member runlength $ ProblemConfiguration.ProblemAnalysis.getDistinctRunlengthsOfSpecifiedTimesByCourse problemAnalysis ! course
						then id	-- No course which specifies consecutive times, of equal duration to that observed, can reasonably deprecate it.
						else (
							+ floor (
								abs $ toRational runlength - Data.Course.calculateIdealConsecutiveLessons course	-- Measure the absolute deviation, to prevent cancellation of deviations resulting from excess & deficit.
							) -- If the ideal lies between two integers, then either is acceptable.
						)
				) generalisedLesson
			)
		) 0
	) 0 . Model.Timetable.findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId
	where
		denominator	= pred {-concept doesn't exist until there're two timeslots/day-} (ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis) * ProblemConfiguration.ProblemAnalysis.getNAvailableDaysPerStudentTimetable problemAnalysis

{- |
	* Gets the number of /location/-changes over all /teacher/s, normalised to the unit-interval.

	* The minimum value of zero; is achieved either by a completely unbooked /timetable/, or by completely booking /lesson/s in the same /location/.
-}
measureLocationChangesOfTeachers :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			locationId,
	Fractional		mean
 )
	=> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level
	-> mean
measureLocationChangesOfTeachers problemAnalysis	= (
	/ fromIntegral (
		ProblemConfiguration.ProblemAnalysis.getNTeachers {-counts only those offering a service-} problemAnalysis * pred {-fence-post-} (ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis * Temporal.Day.nDaysPerWeek)
	)
 ) . fromIntegral . Data.Foldable.sum . TeacherView.Timetable.countLocationChangesByTeacherId

{- |
	* Evaluates the fitness of the /timetable/ according to the /weighted mean/ of the /timetable-criteria/, which defines the direction of the evolutionary process.

	* Also writes the value of each /timetable-criterion/, for post-analysis.
-}
evaluateFitness :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			criterionValue,
	Enum			timeslotId,
	Eq			campus,
	Fractional		criterionValue,
	Fractional		weightedMean,
	Ord			level,
	Ord			locationId,
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
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level	-- ^ The /timetable/ to evaluate.
	-> Control.Monad.Writer.Writer [Maybe criterionValue] weightedMean		-- ^ The fitness of the specified /timetable/, including the values of /timetable-criteria/ (where the weight is non-zero).
evaluateFitness problemParameters executionOptions problemAnalysis studentViewTimetable	= ExecutionConfiguration.TimetableCriteriaWeights.calculateWeightedMean (
	ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights executionOptions
 ) (
	ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.TimetableCriteriaWeights.weightOfMaximiseComplianceWithFreePeriodPreferencesTag $ (
		Dynamic.StudentViewTimetableUtilities.calculateMeanFreePeriodCompliance problemParameters studentViewTimetable + Dynamic.TeacherViewTimetableUtilities.calculateMeanFreePeriodCompliance problemParameters teacherViewTimetable
	) / 2
 ) (
	ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.TimetableCriteriaWeights.weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacityTag $ Dynamic.TeacherViewTimetableUtilities.calculateMeanRatioOfStudentClassSizeToLocationCapacity problemParameters teacherViewTimetable
 ) (
	ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.TimetableCriteriaWeights.weightOfMaximiseMeanStudentClassSizeTag $ TeacherView.Timetable.calculateMeanStudentClassSize teacherViewTimetable / fromIntegral (ProblemConfiguration.ProblemAnalysis.getNStudents problemAnalysis)
 ) (
	ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMaximiseSynchronisationOfSynchronisedCoursesTag $ Dynamic.TeacherViewTimetableUtilities.calculateRatioOfAsynchronousLessonsInSynchronisedCourses problemAnalysis teacherViewTimetable
 ) (
	ExecutionConfiguration.Criterion.mkCriterion ExecutionConfiguration.TimetableCriteriaWeights.weightOfMaximiseWeightedMeanStudentBodyUtilisationRatioTag . realToFrac $ Dynamic.StudentViewTimetableUtilities.calculateWeightedMeanUtilisationRatio problemParameters problemAnalysis studentViewTimetable
 ) (
	let
		denominator	= pred {-fence-post-} nTimeslotsPerDay	-- Normalise to the unit-interval.
	in if denominator == 0
		then minBound	-- Zero lessons have been booked for courses which define an ideal timeslotId; the concept is void.
		else ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequestTag $ Dynamic.StudentViewTimetableUtilities.calculateAverageAbsoluteDeviationFromIdealTimeslotRequest problemParameters problemAnalysis studentViewTimetable / fromIntegral denominator
 ) (
	ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseDispersionOfStudentFreePeriodsPerDayTag . (
		* (
			2 / fromIntegral nTimeslotsPerDay
		) -- Normalise to the unit-interval; the maximum average absolute deviation is half the number of timeslots per day.
	) $ Model.Timetable.calculateAverageAbsoluteDeviationOfFreeLessonsPerDay (
		ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters
	) studentViewTimetable	-- CAVEAT: ought to be weighted by the size of the student-body.
 ) (
	ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseDispersionOfTeacherFreePeriodsPerDayTag . (
		* (
			2 / fromIntegral nTimeslotsPerDay
		) -- Normalise to the unit-interval; the maximum average absolute deviation is half the number of timeslots-per-day.
	) $ Model.Timetable.calculateAverageAbsoluteDeviationOfFreeLessonsPerDay (ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters) teacherViewTimetable
 ) (
	ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseDispersionOfTeacherWorkloadTag . (
		* 2	-- Normalise to the unit-interval; the maximum average absolute deviation in utilisation-ratio is 1/2.
	) $ Dynamic.TeacherViewTimetableUtilities.calculateAverageAbsoluteDeviationInUtilisationRatio problemParameters problemAnalysis teacherViewTimetable
 ) (
	ExecutionConfiguration.Criterion.invertWholeNumbersIntoUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanInterCampusMigrationsOfStudentsTag . (
		/ fromIntegral (
			ProblemConfiguration.ProblemAnalysis.getNAvailableDaysPerStudentTimetable problemAnalysis * pred nTimeslotsPerDay	-- The maximum possible number of migrations.
		) -- Derive mean & normalise.
	) . fromIntegral $ Dynamic.StudentViewTimetableUtilities.countInterCampusMigrations problemParameters studentViewTimetable
 ) (
	ExecutionConfiguration.Criterion.invertWholeNumbersIntoUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanInterCampusMigrationsOfTeachersTag . (
		/ fromIntegral (
			ProblemConfiguration.ProblemAnalysis.getNAvailableDaysPerTeacherTimetable problemAnalysis * pred nTimeslotsPerDay	-- The maximum possible number of migrations.
		) -- Derive mean & normalise.
	) . fromIntegral $ Dynamic.TeacherViewTimetableUtilities.countInterCampusMigrations problemParameters teacherViewTimetable
 ) (
	ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanLocationChangesOfTeachersTag $ measureLocationChangesOfTeachers problemAnalysis teacherViewTimetable	-- CAVEAT: may conflict with 'minimiseRatioOfConsecutiveEqualLessons'.
 ) (
	ExecutionConfiguration.Criterion.invertWholeNumbersIntoUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanLocusOperandiOfTeachersTag $ StudentView.Timetable.calculateMeanLocusOperandiOfTeachers studentViewTimetable
 ) (
	ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledgeTag $ Data.Requirements.getCore meanRatioOfIncompletelyBookedKnowledgeRequirements
 ) (
	ExecutionConfiguration.Criterion.reflectUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledgeTag $ Data.Requirements.getOptional meanRatioOfIncompletelyBookedKnowledgeRequirements
 ) (
	ExecutionConfiguration.Criterion.invertNaturalNumbersIntoUnitInterval ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanStudentBodyCombinationsPerLessonTag $ StudentView.Timetable.calculateWeightedMeanStudentBodyCombinationsPerLesson studentViewTimetable	-- CAVEAT: performance-hotspot.
 ) (
	measureDeviationFromMinimumConsecutiveLessonsCriterion problemParameters problemAnalysis studentViewTimetable	-- minimiseRatioOfConsecutiveEqualLessons.
 ) (
	calculateRatioOfSeparatedEqualLessonsWithinAnyDayCriterion problemParameters problemAnalysis studentViewTimetable
 ) where
	nTimeslotsPerDay :: Size.NTimeslots
	nTimeslotsPerDay	= ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis

	teacherViewTimetable					= Dynamic.TeacherViewTimetableUtilities.fromStudentViewTimetable executionOptions problemAnalysis studentViewTimetable
	meanRatioOfIncompletelyBookedKnowledgeRequirements	= Dynamic.StudentViewTimetableUtilities.findMeanRatioOfIncompletelyBookedKnowledgeRequirements problemParameters problemAnalysis studentViewTimetable

