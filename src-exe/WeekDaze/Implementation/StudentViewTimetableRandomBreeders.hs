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

	* Defines many different strategies used to breed a candidate population, from a parent.

	* Each strategy depletes the specified /timetable/ in a variety of different ways;
	then for each depleted /timetable/, reconstructs it a variety of different ways,
	using the supplied reconstructor to which a /fecundity/ is supplied in order to generate the required size candidate-population.
-}

module WeekDaze.Implementation.StudentViewTimetableRandomBreeders(
-- * Types
-- ** Data-types
	StudentViewTimetableRandomBreederStrategy(..),
-- ** Type-synonyms
--	RandomBreeder,
--	StudentViewTimetableRandomBreeder,
	StudentViewTimetableQuantifiedRandomBreeder,
-- * Functions
	breed,
--	findRelocatableSynchronousLessons,
--	locateConsecutivesAndSynchronous,
--	studentViewTimetableSynchronisedCourseDepleter,
--	studentViewTimetableSynchronisedCourseByDayDepleter,
--	studentViewTimetableExcessRunlengthDepleter,
--	studentViewTimetableHomogeneousStudentViewLessonDepleter,
--	studentViewTimetableIncompleteCourseDepleter,
--	studentViewTimetableSingletonStudentClassDepleter,
--	studentViewTimetableSplitSessionDepleter,
--	studentViewTimetableStudentBodyCombinationDepleter,
--	studentViewTimetableStudentViewTimetableForDayDepleter,
--	studentViewTimetableStudentViewTimetableForWeekDepleter,
--	studentViewTimetableSynchronousLessonDepleter,
-- ** Accessors
	getTimetableBreederFecundity,
-- ** Predicates
--	isBookingCumbersome,
-- ** Operators
--	(>*>)
) where

import qualified	Control.Arrow
import			Control.Arrow((&&&), (***))
import			Control.Monad((>=>))
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Map
import			Data.Map((!))
import qualified	Data.Maybe
import qualified	Data.Set
import			Data.Set((\\))
import qualified	System.Random
import qualified	ToolShed.Data.List.Runlength
import qualified	ToolShed.System.Random
import qualified	WeekDaze.Aggregate.TeacherRegister				as Aggregate.TeacherRegister
import qualified	WeekDaze.Data.Course						as Data.Course
import qualified	WeekDaze.Data.Resource						as Data.Resource
import qualified	WeekDaze.Data.Student						as Data.Student
import qualified	WeekDaze.Dynamic.StudentViewTimetableForWeekUtilities		as Dynamic.StudentViewTimetableForWeekUtilities
import qualified	WeekDaze.Dynamic.StudentViewTimetableUtilities			as Dynamic.StudentViewTimetableUtilities
import qualified	WeekDaze.ExecutionConfiguration.EvolutionStrategies		as ExecutionConfiguration.EvolutionStrategies
import qualified	WeekDaze.ExecutionConfiguration.TimetableBreederFecundity	as ExecutionConfiguration.TimetableBreederFecundity
import qualified	WeekDaze.LinearModel.Timetable					as LinearModel.Timetable
import qualified	WeekDaze.LinearModel.TimetableForWeek				as LinearModel.TimetableForWeek
import qualified	WeekDaze.Model.Lesson						as Model.Lesson
import qualified	WeekDaze.Model.Timetable					as Model.Timetable
import qualified	WeekDaze.Model.TimetableCoordinates				as Model.TimetableCoordinates
import qualified	WeekDaze.Model.TimetableForDay					as Model.TimetableForDay
import qualified	WeekDaze.Model.TimetableForWeek					as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis			as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters			as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.Size							as Size
import qualified	WeekDaze.StudentView.LessonResourceIds				as StudentView.LessonResourceIds
import qualified	WeekDaze.StudentView.Timetable					as StudentView.Timetable
import qualified	WeekDaze.StudentView.TimetableCoordinates			as StudentView.TimetableCoordinates
import qualified	WeekDaze.Temporal.Availability					as Temporal.Availability
import qualified	WeekDaze.Temporal.Day						as Temporal.Day
import qualified	WeekDaze.Temporal.Time						as Temporal.Time
import			WeekDaze.ExecutionConfiguration.TimetableBreederFecundity((>*<))

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<*>))
#endif

infixr 7 >*>	-- Same precedance as (*), but right-associative.

-- | The type of a function used to breed a candidate population from a single parent; the reproduction phase in an evolutionary algorithm.
type RandomBreeder randomGen a
	= randomGen	-- ^ Used to mutate the childen.
	-> a		-- ^ The parent from which to breed.
	-> [a]

-- | A 'RandomBreeder' which has been specialised for /student-view timetable/s.
type StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level	= RandomBreeder randomGen (StudentView.Timetable.Timetable timeslotId locationId teacherId level)

-- | A 'StudentViewTimetableRandomBreeder' which quantifies the required population-size.
type StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level	= ExecutionConfiguration.TimetableBreederFecundity.Fecundity -> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level

{- |
	* A combinator which sequences two 'RandomBreeder's: the implementation uses the List-monad's implementation of @ (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c @,
	the parts of which match the signature of 'RandomBreeder' after the application of the 'randomGen' parameter.

	* CAVEAT: this operation is rather like multiplication, in that if either 'RandomBreeder' produces nothing, then the other becomes irrelevant,
	but it's not commutative (<https://en.wikipedia.org/wiki/Commutative_property>):
	a single child is extracted from the left-hand 'RandomBreeder' & then used by the right-hand 'RandomBreeder' to build a population of grandchildren,
	before proceeding to extract the next child from the left-hand 'RandomBreeder';
	consequently, if the tail of resulting population is dropped, then some offspring from the left-hand 'RandomBreeder' may never breed.
-}
(>*>) :: RandomBreeder randomGen a -> RandomBreeder randomGen a -> RandomBreeder randomGen a
firstGenerationRandomBreeder >*> secondGenerationRandomBreeder	= \randomGen -> firstGenerationRandomBreeder randomGen >=> secondGenerationRandomBreeder randomGen	-- There's no need to split the random-generator, since it's deployed in different environments.

-- | Identifies the various strategies for breeding a candidate population from a /timetable/.
data StudentViewTimetableRandomBreederStrategy
	= SynchronisedCourseMutation
	| SynchronisedCourseByDayMutation
	| ExcessRunlengthMutation
	| HomogeneousStudentViewLessonMutation
	| IncompleteCourseMutation
	| RandomLessonMutation Int Size.NTimeslots							-- ^ Qualified by the number of random trials, & the number of randomly selected /lesson/s to delete from a /timetable/ in each trial, before reconstruction.
	| SingletonStudentClassMutation
	| SplitSessionMutation
	| StudentBodyCombinationMutation
	| StudentViewTimetableForDayMutation (Maybe Size.NDays)						-- ^ Optionally qualified by the number of whole /day/s to delete from a /studentBody/'s /timetableForWeek/, before reconstruction; if unspecified, then all valid numbers of day will be tried.
	| StudentViewTimetableForWeekMutation Int Size.NTimeslots					-- ^ Qualified by the number of random trials, & the number of randomly selected /lesson/s to delete from a /studentBody/'s /timetableForWeek/ in each trial, before reconstruction.
	| SynchronousLessonMutation
	| StudentViewTimetableRandomBreederStrategy :*> StudentViewTimetableRandomBreederStrategy	-- ^ An arbitrary (though preferrably orthogonal) binary combination of 'StudentViewTimetableRandomBreederType's. N.B.: it isn't commutative; the order to the two strategies is significant.

instance Show StudentViewTimetableRandomBreederStrategy where
	showsPrec _ SynchronisedCourseMutation					= showString ExecutionConfiguration.EvolutionStrategies.synchronisedCourseMutationTag
	showsPrec _ SynchronisedCourseByDayMutation				= showString ExecutionConfiguration.EvolutionStrategies.synchronisedCourseByDayMutationTag
	showsPrec _ ExcessRunlengthMutation					= showString ExecutionConfiguration.EvolutionStrategies.excessRunlengthMutationTag
	showsPrec _ HomogeneousStudentViewLessonMutation			= showString ExecutionConfiguration.EvolutionStrategies.homogeneousStudentViewLessonMutationTag
	showsPrec _ IncompleteCourseMutation					= showString ExecutionConfiguration.EvolutionStrategies.incompleteCourseMutationTag
	showsPrec _ (RandomLessonMutation nTrials nTimeslots)			= showString ExecutionConfiguration.EvolutionStrategies.randomLessonMutationTag . showChar '(' . shows nTrials . showChar ',' . shows nTimeslots . showChar ')'
	showsPrec _ SingletonStudentClassMutation				= showString ExecutionConfiguration.EvolutionStrategies.singletonStudentClassMutationTag
	showsPrec _ SplitSessionMutation					= showString ExecutionConfiguration.EvolutionStrategies.splitSessionMutationTag
	showsPrec _ StudentBodyCombinationMutation				= showString ExecutionConfiguration.EvolutionStrategies.studentBodyCombinationMutationTag
	showsPrec _ (StudentViewTimetableForDayMutation maybeNDays)		= showString ExecutionConfiguration.EvolutionStrategies.studentViewTimetableForDayMutationTag . Data.Maybe.maybe id (
		\nDays -> showString ExecutionConfiguration.EvolutionStrategies.studentViewTimetableForDayMutationTag . showChar '(' . shows nDays . showChar ')'
	 ) maybeNDays
	showsPrec _ (StudentViewTimetableForWeekMutation nTrials nTimeslots)	= showString ExecutionConfiguration.EvolutionStrategies.studentViewTimetableForWeekMutationTag . showChar '(' . shows nTrials . showChar ',' . shows nTimeslots . showChar ')'
	showsPrec _ SynchronousLessonMutation					= showString ExecutionConfiguration.EvolutionStrategies.synchronousLessonMutationTag
	showsPrec _ (l :*> r)							= showChar '(' . shows l . showString " . " {-intended to look like function-composition-} . shows r . showChar ')'

-- | Determines how wide a candidate population to breed in any one generation.
getTimetableBreederFecundity
	:: StudentViewTimetableRandomBreederStrategy
	-> ExecutionConfiguration.EvolutionStrategies.EvolutionStrategies fecundityDecayRatio populationDiversityRatio
	-> ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity	-- ^ Returns the /fecundity/ to be used for this /evolution-strategy/.
getTimetableBreederFecundity SynchronisedCourseMutation			= ExecutionConfiguration.EvolutionStrategies.getSynchronisedCourseMutationFecundity
getTimetableBreederFecundity SynchronisedCourseByDayMutation		= ExecutionConfiguration.EvolutionStrategies.getSynchronisedCourseByDayMutationFecundity
getTimetableBreederFecundity ExcessRunlengthMutation			= ExecutionConfiguration.EvolutionStrategies.getExcessRunlengthMutationFecundity
getTimetableBreederFecundity HomogeneousStudentViewLessonMutation	= ExecutionConfiguration.EvolutionStrategies.getHomogeneousStudentViewLessonMutationFecundity
getTimetableBreederFecundity IncompleteCourseMutation			= ExecutionConfiguration.EvolutionStrategies.getIncompleteCourseMutationFecundity
getTimetableBreederFecundity (RandomLessonMutation _ _)			= ExecutionConfiguration.EvolutionStrategies.getRandomLessonMutationFecundity
getTimetableBreederFecundity SingletonStudentClassMutation		= ExecutionConfiguration.EvolutionStrategies.getSingletonStudentClassMutationFecundity
getTimetableBreederFecundity SplitSessionMutation			= ExecutionConfiguration.EvolutionStrategies.getSplitSessionMutationFecundity
getTimetableBreederFecundity StudentBodyCombinationMutation		= ExecutionConfiguration.EvolutionStrategies.getStudentBodyCombinationMutationFecundity
getTimetableBreederFecundity (StudentViewTimetableForDayMutation _)	= ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForDayMutationFecundity
getTimetableBreederFecundity (StudentViewTimetableForWeekMutation _ _)	= ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForWeekMutationFecundity
getTimetableBreederFecundity SynchronousLessonMutation			= ExecutionConfiguration.EvolutionStrategies.getSynchronousLessonMutationFecundity
getTimetableBreederFecundity (l :*> r)					= uncurry (>*<) . (getTimetableBreederFecundity l &&& getTimetableBreederFecundity r)	-- Recurse.

{- |
	* Use the /StudentViewTimetable-depleter/ & the specified 'StudentViewTimetableQuantifiedRandomBreeder', to breed a candidate population of the required size.

	* CAVEAT: if the combination of depleter & reconstructor can't generate enough combinations, the size of the resulting population may be smaller than requested.
-}
breed :: forall campus level locationId randomGen stream synchronisationId teacherId teachingRatio timeslotId. (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			synchronisationId,
	Ord			teacherId,
	RealFrac		teachingRatio,
	System.Random.RandomGen	randomGen
 )
	=> StudentViewTimetableRandomBreederStrategy
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level	-- ^ StudentViewTimetable-reconstructor.
	-> StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level
breed studentViewTimetableRandomBreederStrategy problemAnalysis problemParameters	= breedUsing $ getStudentViewTimetableDepleter studentViewTimetableRandomBreederStrategy where
	breedUsing
		:: StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level		-- Depleter.
		-> StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level	-- Reconstructor.
		-> StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level
	breedUsing studentViewTimetableDepleter studentViewTimetableReconstructor fecundity randomGen studentViewTimetable	= take fecundity {-truncate any excess resulting from the use of 'ceiling'-} . concatMap (
		studentViewTimetableReconstructor (
			ceiling $ toRational fecundity / fromIntegral (length depletedTimetables {-non-zero-})
		) randomGen	-- Generate the required population of candidates; though it could generate more because the estimate is rounded-up, & could generate fewer when insufficient distinct reconstructions can be generated. The same random generator can always be used to shuffle the available timeslots, since they're always different.
	 ) $ ToolShed.System.Random.shuffle randomGen {-relevant when fecundity < nDepletedTimetables-} depletedTimetables where
		depletedTimetables	= studentViewTimetableDepleter randomGen studentViewTimetable

	getStudentViewTimetableDepleter :: StudentViewTimetableRandomBreederStrategy -> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
	getStudentViewTimetableDepleter SynchronisedCourseMutation					= studentViewTimetableSynchronisedCourseDepleter problemAnalysis problemParameters
	getStudentViewTimetableDepleter SynchronisedCourseByDayMutation					= studentViewTimetableSynchronisedCourseByDayDepleter problemAnalysis problemParameters
	getStudentViewTimetableDepleter ExcessRunlengthMutation						= studentViewTimetableExcessRunlengthDepleter problemAnalysis problemParameters
	getStudentViewTimetableDepleter HomogeneousStudentViewLessonMutation				= studentViewTimetableHomogeneousStudentViewLessonDepleter problemAnalysis problemParameters
	getStudentViewTimetableDepleter IncompleteCourseMutation					= studentViewTimetableIncompleteCourseDepleter problemParameters
	getStudentViewTimetableDepleter (RandomLessonMutation nTrials nTimeslots)			= studentViewTimetableRandomDepleter nTrials nTimeslots problemAnalysis problemParameters
	getStudentViewTimetableDepleter SingletonStudentClassMutation					= studentViewTimetableSingletonStudentClassDepleter problemParameters
	getStudentViewTimetableDepleter SplitSessionMutation						= studentViewTimetableSplitSessionDepleter problemParameters
	getStudentViewTimetableDepleter StudentBodyCombinationMutation					= studentViewTimetableStudentBodyCombinationDepleter problemParameters
	getStudentViewTimetableDepleter (StudentViewTimetableForDayMutation maybeNDays)			= studentViewTimetableStudentViewTimetableForDayDepleter maybeNDays problemParameters
	getStudentViewTimetableDepleter (StudentViewTimetableForWeekMutation nTrials nTimeslots)	= studentViewTimetableStudentViewTimetableForWeekDepleter nTrials nTimeslots problemAnalysis problemParameters
	getStudentViewTimetableDepleter SynchronousLessonMutation					= studentViewTimetableSynchronousLessonDepleter problemParameters
	getStudentViewTimetableDepleter (l :*> r)							= getStudentViewTimetableDepleter l {-recurse-} >*> getStudentViewTimetableDepleter r {-recurse-}

{- |
	* Depletes the specified /timetable/, by undefining individual groups of identical student-view /lesson/s (though potentially different /student-body/).
	One could alternatively focus on identical /lesson/s from each /student-body/ in turn, but it doesn't seem to explore a different solution-space & empirically seems slightly less productive;
	see 'studentViewTimetableRandomBreederBySingletonStudentClassMutation'.

	* On encountering a /synchronised course/, all /lesson/s for the other /course/s with the same /synchronisationId/ are deleted also.

	* /Student-view lesson/s are composed of a /subject/, /teacher/ & /location/, which are the components of a routine which once formed must persist;
	because a /student/ doesn't want to study a /subject/ with different /teacher/s, or in a different /location/, for each /booking/.
	Thus removing one set of identical student-view /lesson/s, allows a new routine to be established,
	facilitating the transfer of workload between /teacher/s with compatible skills.

	* The supplied random generator is unused, so the result is deterministic.
-}
studentViewTimetableHomogeneousStudentViewLessonDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			synchronisationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableHomogeneousStudentViewLessonDepleter problemAnalysis problemParameters _ studentViewTimetable = map (
	\studentViewLesson -> LinearModel.Timetable.unbookWhere (
		Data.Maybe.maybe (
			(== studentViewLesson) . Model.Timetable.getBookedLesson
		) (
			\synchronisationId -> (
				`Data.Set.member` Data.Set.map Data.Course.getSubject (
					Aggregate.TeacherRegister.findDistinctCoursesBySynchronisationId (
						ProblemConfiguration.ProblemAnalysis.getCoursesByTeacherIdBySynchronisationId problemAnalysis
					) ! synchronisationId
				)
			) . Model.Lesson.getSubject . Model.Timetable.getBookedLesson
		) . Data.Course.getMaybeSynchronisationId $ ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson
	) studentViewTimetable {-deplete-}	-- Deplete the specified timetable.
 ) . Data.Set.elems $ Model.Timetable.extractDistinctLessons studentViewTimetable

{- |
	* True if the specified /booking/ is required to be at its current /coordinates/, either by another /booking/ or by explicit configuration.
	Under these circumstances, any attempt to relocate it, is unlikely to be beneficial, without addressing these issues also.
	Attempts to reconstruct a random set of /booking/s will be disadvantaged by the presence of any cumbersome one, & probably discarded after much futile effort.

	* Three types of /booking/ qualify, those;
		whose /course/ is /synchronised/;
		which are at /time/s specified by the /course/;
		whose runlength is irreducible.

	* CAVEAT: a /lesson/ booked at a /time/ specified by the /course/, may not be cumbersome,
	if another /teacher/ offers a /course/ in the same /subject/, which doesn't specify this /time/.
	This requires that all /bookings/ for the original /teacher/ are also removed, so that there's no established routine to break.
-}
isBookingCumbersome :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> StudentView.Timetable.Booking timeslotId locationId teacherId level
	-> Bool
isBookingCumbersome problemParameters studentViewTimetable booking	= any ($ course) [
	Data.Course.isSynchronised,
	Data.Course.isASpecifiedTime time,
	not . Model.TimetableForWeek.isRunlengthReducibleAt (studentViewTimetable ! studentBody) time
 ] where
	((studentBody, time), course)	= (Model.TimetableCoordinates.getObserverId &&& Model.TimetableCoordinates.getTime) . Model.Timetable.getBookedCoordinates &&& ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters . Model.Timetable.getBookedLesson $ booking

{- |
	* /Timetable/s which contain /lesson/s for any single /course/,
	in which the /student-class/ has been composed from different combinations of /student-bodies/ at various booking-/time/s, are considered undesirable.
	So, if /student-bodies/ @A@ & @B@ both require the same /subject/,
	one would probably prefer to teach the class @{A, B}@ consistently (or failing that, to teach each /student-body/ in isolation),
	rather than class-combinations for this /student-body/, @{A}, {B}, {A, B}@, at various times throughout the week,

	* Proliferation of /student-body/-combinations, for the /lesson/s booked for a /course/,
	can be discouraged by the weightings applied to specific /lesson-criteria/ & /timetable-criteria/,
	but unless all instances of a specific /student-body/-combination for a /course/ are unbooked simultaneously,
	a better weighted mean of the /timetable-criteria/ is unlikely to result,
	& therefore the resulting /timetable/ won't be selected as the basis of the next generation.

	* This strategy depletes the /timetable/, by finding instances where the /student-classes/, for the /lesson/s booked for a /course/,
	have been composed from more than one combination of /student-bodies/,
	unbooks all the /lesson/s for one of these /student-body/-combinations.

	* If a /lesson/ is deleted, which is part of a runlength, then the other (identical) members will also be deleted
	(they're still identical, even if they result in a different /studentbody-combination/);
	so required runlengths can't erroneously be split, merely completely obliterated.

	* If a /lesson/ is booked at a /time/ specified by the /course/, then it may still be unbooked,
	since it could be replaced by a /lesson/ from a compatible /course/ offered by another /teacher/,
	or by a /lesson/ at the same /time/, but at an alternative /location/.

	* CAVEAT: avoids /lesson/s belonging to /synchronised course/s, because the probability of beneficial mutation is low.

	* The supplied random generator is unused, so the result is deterministic.
-}
studentViewTimetableStudentBodyCombinationDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableStudentBodyCombinationDepleter problemParameters _ studentViewTimetable	= [
	Model.Timetable.undefineTimeslots studentViewTimetable (
		Data.Foldable.concatMap (
			\studentBody -> map (
				(,) studentBody . Model.TimetableForWeek.getBookedTime	-- Construct coordinates.
			) . filter (
				uncurry (&&) . (
					(== studentViewLesson) . Model.TimetableForWeek.getBookedLesson &&& (== moribundStudentClass) . ($ studentViewTimetable) . StudentView.Timetable.extractStudentClassAt . flip (,) studentViewLesson . Model.TimetableForWeek.getBookedTime
				) -- Select all lessons for this class; even if they're at a specified time, since it can be replaced by a lesson at this same time, but with a different location or teacher.
			) . LinearModel.TimetableForWeek.fromTimetableForWeek $ studentViewTimetable ! studentBody
		) moribundStudentClass
	) {-deplete-} |
		(studentViewLesson, studentClassSet)	<- Data.Map.toList . Data.Map.filter (
			(> 1) . Data.Set.size	-- Select lessons for which more than one combination of student-body has been booked, & therefore with potential for improvement.
		) $ StudentView.Timetable.findStudentClassesByLesson studentViewTimetable,	-- This returns more than just student-body combinations, including identical lessons for mutually exclusive student-classes.
		moribundStudentClass			<- Data.Set.toList studentClassSet,	-- Select in turn, each student-class booked for this lesson.
		Data.Foldable.any (
			\studentBody -> Data.Foldable.any (Data.Set.member studentBody) $ Data.Set.delete moribundStudentClass studentClassSet
		) moribundStudentClass,	-- Select where at least one member student-body exists in one of the other student-class combinations for this studentViewLesson.
		not . Data.Course.isSynchronised $ ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson	-- One could alternatively unbook all synchronised lessons also, but the chance of a beneficial mutation is low.
 ] -- List-comprehension.

{- |
	* Finds identical consecutive /lesson/s whose runlength exceeds the ideal for the /course/ to which they belong.
	Also attempts to locate a /booking/ at a suitable alternative /time/; which as a side-effect increases the diversity in the population of candidates.

	* Unbooks; excess terminal /lesson/s from the long runlength, any /lesson/s from other /student-class/-members with the same problem, & the /lesson/ at the alternative /time/.

	* CAVEAT: avoids /lesson/s belonging to /synchronised course/s, because the probability of beneficial mutation is low.

	* CAVEAT: avoids /lesson/s whose /course/ specifies the current booking-/time/,
	since the remaining /lesson/s for this /course/ define the /teacher/, preventing enrolment on a compatible /course/ offered by another /teacher/.
-}
studentViewTimetableExcessRunlengthDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableExcessRunlengthDepleter problemAnalysis problemParameters _ studentViewTimetable	= [
	Model.Timetable.undefineTimeslots studentViewTimetable {-deplete-} . (
		blockingCoordinates :	-- Provide an alternative place for one of the deleted lessons.
	) $ map (,) (
		(
			studentBody :	-- Prepend the original.
		) . filter (
			elem session . (excessiveLessonRunlengthsByTimeByStudentBody !)
		) . Data.Set.toList . Data.Set.delete studentBody . Data.Set.unions $ map (
			(
				`StudentView.Timetable.extractStudentClassAt` studentViewTimetable
			) . flip (,) studentViewLesson {-construct a TimetableForWeek-booking-}
		) terminalTimeRange	-- Identify class-members who've booked the same excessive lesson-runlength.
	) <*> terminalTimeRange {-construct coordinates from the permutations of student-bodies & terminal times-} |
		(studentBody, excessiveLessonRunlengthsByTime)	<- Data.Map.toList excessiveLessonRunlengthsByTimeByStudentBody,
		session						<- excessiveLessonRunlengthsByTime,
		let
			studentViewTimetableForWeek	= studentViewTimetable ! studentBody
			(
				(day, startingTimeslotId),
				(runLength, studentViewLesson)
			 )				= (Temporal.Time.getDay &&& Temporal.Time.getTimeslotId) *** (ToolShed.Data.List.Runlength.getLength &&& ToolShed.Data.List.Runlength.getDatum) $ session	-- Deconstruct.
			studentViewLessonResourceIds	= StudentView.LessonResourceIds.getLocationId &&& StudentView.LessonResourceIds.getTeacherId $ Model.Lesson.getResourceIds studentViewLesson	-- Deconstruct.
			studentViewLessonResources	= (ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters !) *** (ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters !) $ studentViewLessonResourceIds	-- Lookup.
			course				= ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson
			minimumConsecutiveLessons	= Data.Course.getMinimumConsecutiveLessons course	-- Access.
			excessLength			= runLength - minimumConsecutiveLessons,
		not $ Data.Course.isSynchronised course,	-- Leave these for the specialist; 'studentViewTimetableRandomBreederBySynchronisedCourseMutation'.
		(
			Dynamic.StudentViewTimetableForWeekUtilities.countUnbookedByLesson problemParameters studentViewTimetableForWeek ! studentViewLesson
		) + excessLength >= minimumConsecutiveLessons,	-- Ensure there're enough unbooked lessons in this course, to enable booking of a new isolated session of length minimumConsecutiveLessons.
		(
			>= Data.Course.getRequiredLessonsPerWeek course	-- Any fewer & at least one excessive lesson-runlength is inevitable; so accept it & move on.
		) . (
			* minimumConsecutiveLessons
		) $ Data.Resource.countDaysPerWeekAvailable (
			studentViewLessonResources,
			ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters ! studentBody
		), -- We merely want to relocate the lesson, not mutate it, so all its resources must be available on enough other days.
		terminalTimeRange	<- let
			takeWhileRelocatable	= takeWhile (
				not . (`Data.Course.isASpecifiedTime` course)
			 ) . map (
				Temporal.Time.mkTime day
			 )
		in [
			takeWhileRelocatable . uncurry enumFromTo $ (
				id &&& Temporal.Time.shift (
					pred {-fence-post-} excessLength
				)
			) startingTimeslotId,	-- Take from the start of the runlength.
			reverse . takeWhileRelocatable . reverse . uncurry enumFromTo . (
				Temporal.Time.shift (
					negate $ pred {-fence-post-} excessLength
				) &&& id
			) $ Temporal.Time.shift (
				pred {-fence-post-} runLength	-- Derive the timeslotId at the end of the runlength.
			) startingTimeslotId	-- Take from the end of the runlength.
		], -- Find the timeslots at either end of the runlength, in the excess span, none of which are at times specified for the course.
		blockingCoordinates				<- map (
			(,) studentBody	-- Construct coordinates.
		) . filter (
			not . (
				$ studentViewLessonResourceIds	-- Complete the function-application.
			) . uncurry . (
				$ studentBody	-- Progress the function-application slightly.
			) . ProblemConfiguration.ProblemAnalysis.areAnyResourcesBookedForGroupMeeting problemAnalysis	-- Partially apply.
		) $ let
{-
	Ensure that the resources required for the studentViewLesson, are regularly available on the specified alternative day;
	it is assumed that there's an incumbent lesson, so the student-body is known to be available.
	Avoid days on which this lesson has already been booked, to avoid creating a new excessive runlength, or worse, a split session.
-}
			isSuitableAlternativeDay :: Temporal.Day.Day -> Bool
			isSuitableAlternativeDay	= uncurry (&&) . (
				(`Data.Resource.isAvailableOn` studentViewLessonResources) &&& Data.Foldable.notElem (
					Just studentViewLesson
				) . (
					studentViewTimetableForWeek Data.Array.IArray.!
				) -- Inevitably excludes studentViewLesson's current day.
			 )
		in if minimumConsecutiveLessons == 1 {-the typical scenario-}
			then [
				Temporal.Time.mkTime day' timeslotId |
					(day', timetableForDay)	<- Data.Array.IArray.assocs studentViewTimetableForWeek,
					isSuitableAlternativeDay day',
					(timeslotId, Just _)	<- Data.Array.IArray.assocs timetableForDay	-- Select bookings.
			] -- List-comprehension. Locate lessons when one of our extra lessons might alternatively be booked.
			else [
				Temporal.Time.mkTime day' timeslotId |
					(day', generalisedLessonRunlengthsByTimeslotId)	<- Data.Array.IArray.assocs $ Model.Timetable.findGeneralisedLessonRunlengthsByTimeslotIdByDayByObserverId studentViewTimetable ! studentBody,
					isSuitableAlternativeDay day',
					(
						startingTimeslotId',
						(
							unbookedTimeslotRunlength,
							Nothing	-- i.e. unbooked.
						) -- Pair
					) {-pair-}					<- generalisedLessonRunlengthsByTimeslotId,	-- Select unbooked timeslots.
					unbookedTimeslotRunlength >= pred minimumConsecutiveLessons,					-- Select unbooked timeslot-runlengths that're almost long enough; or longer.
					timeslotId					<- filter (ProblemConfiguration.ProblemAnalysis.isValidTimeslotId problemAnalysis) [
						pred startingTimeslotId',
						Temporal.Time.shift unbookedTimeslotRunlength startingTimeslotId'
					] -- Derive the timeslotIds either side of the unbooked runlength; which if valid, must contain a lesson.
			], -- List-comprehension. Locate unallocated timeslots, then elongate them to the minimum length required to book a runlength of studentViewLessons.
		not . isBookingCumbersome problemParameters studentViewTimetable . (,) blockingCoordinates {-construct a booking-} . Data.Maybe.fromJust $ Model.Timetable.getMaybeLesson blockingCoordinates studentViewTimetable	-- Avoid unbooking lessons which can't easily be relocated.
 ] {-list-comprehension-} where
	excessiveLessonRunlengthsByTimeByStudentBody	= Dynamic.StudentViewTimetableUtilities.findExcessiveLessonRunlengthsByTimeByStudentBody problemParameters studentViewTimetable

{- |
	* Attempts to reduce the incidence of split sessions of identical /lesson/s within a /day/.

	* Locates separated equal-lesson sessions occuring in any one /day/, then unbooks;
	one whole runlength of /lesson/s, leaving the remainder; & any /lesson/s from /student-class/-members with the same problem.

	* CAVEAT: avoids /lesson/s belonging to /synchronised course/s, because the probability of beneficial mutation is low.

	* CAVEAT: avoids unbooking any runlength of /lesson/s, which spans a /time/ specified for the /course/,
	since the remaining /lesson/s for this /course/ define the /location/ & /teacher/, preventing any alternative solution.

	* CAVEAT: /split session/s are only introduced in the first place by 'Implementation.RandomConstructor.mutateStudentViewTimetable',
	so @ ExecutionConfiguration.TimetableBreederFecundity.getDeterministicConstructorFecundity . ExecutionConfiguration.EvolutionStrategies.getSplitSessionMutation @ can be zero,
	or they can be avoided completely by only configuring non-zero fecundity for 'Implementation.DeterministicConstructor.mutateStudentViewTimetable'.

	* The supplied random generator is unused, so the result is deterministic.
-}
studentViewTimetableSplitSessionDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableSplitSessionDepleter problemParameters _ studentViewTimetable	= [
	Model.Timetable.undefineTimeslots studentViewTimetable {-deplete-} $ map (,) (
		(
			studentBody :	-- Prepend the original.
		) . filter (
			Data.Maybe.maybe False {-this student-body doesn't have any split sessions for this lesson-type-} (
				elem session	-- Select student-bodies which have a split-session with the same starting timeslot & length.
			) . lookup studentViewLesson . (
				Data.Array.IArray.! day
			) . (
				separatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId !
			) -- Identify class-members who've booked the same split session.
		) . Data.Set.toList . Data.Set.delete studentBody . Data.Set.unions $ map (
			(
				`StudentView.Timetable.extractStudentClassAt` studentViewTimetable
			) . flip (,) studentViewLesson {-construct a TimetableForWeek-booking-}
		) timeRange
	) <*> timeRange {-construct coordinates from the permutations of student-bodies & times-} |
		(studentBody, separatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDay)	<- Data.Map.toList separatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId,
		(day, separatedEqualLessonRunlengthsByStartingTimeslotIdByLesson)		<- Data.Array.IArray.assocs separatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDay,
		(studentViewLesson, separatedEqualLessonRunlengthsByStartingTimeslotId)		<- separatedEqualLessonRunlengthsByStartingTimeslotIdByLesson,
		let course	= ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson,
		not $ Data.Course.isSynchronised course,
		session@(startingTimeslotId, runlength)						<- separatedEqualLessonRunlengthsByStartingTimeslotId,
		let timeRange	= map (Temporal.Time.mkTime day) . uncurry enumFromTo $ (id &&& Temporal.Time.shift (pred {-fence-post-} runlength)) startingTimeslotId,
		not $ any (`Data.Course.isASpecifiedTime` course) timeRange
 ] {-list-comprehension-} where
	separatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId	= Model.Timetable.findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId studentViewTimetable

{- |
	* Find groups of synchronous /lesson/s, at coordinates where one might alternatively book a set of /lesson/s for the /course/s of the specified /synchronisationId/.

	* Excludes synchronous /booking/s which include any /lesson/ which can't be easily relocated.
-}
findRelocatableSynchronousLessons :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			synchronisationId,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> synchronisationId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> [StudentView.TimetableCoordinates.Vector timeslotId]
findRelocatableSynchronousLessons problemAnalysis problemParameters synchronisationId	= (
	\(synchronisedStudentBodySet, synchronousBlockingCoordinatesVectors)	-> filter (
		(
			\(bookedStudentBodies, time)	-> all (
				ProblemConfiguration.ProblemAnalysis.isFree problemParameters time . (
					ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters !	-- Lookup the teacher's profile.
				) -- Check that all teachers who're required, are free.
			) (
				Data.Map.keys {-teacherIds-} $ ProblemConfiguration.ProblemAnalysis.getCoursesByTeacherIdBySynchronisationId problemAnalysis ! synchronisationId
			) && Data.Foldable.all (
				ProblemConfiguration.ProblemAnalysis.isFree problemParameters time . (
					ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters !	-- Lookup the student-body's profile.
				)
			) (
				synchronisedStudentBodySet \\ Data.Set.fromList bookedStudentBodies	-- Derive the complementary unbooked student-bodies.
			) -- Check that all those student-bodies who're required but not currently booked, are free.
		) . (
			map Model.TimetableCoordinates.getObserverId {-the student-bodies who're booked-} &&& Model.TimetableCoordinates.getTime . head {-the common booking-time-}
		)
	) synchronousBlockingCoordinatesVectors
 ) . (
	Data.Map.keysSet {-the set of student-bodies, who require the specified synchronised course-} &&& (
		\synchronisedStudentViewTimetable -> map (
			map Model.Timetable.getBookedCoordinates	-- Discard the lesson.
		) . filter (
			not . any (
				isBookingCumbersome problemParameters synchronisedStudentViewTimetable
			) -- Ensure that all bookings can be easily relocated & that they're not already part of the synchronised course we're seeking to book.
		) . Data.List.Extra.groupSortOn (
			Model.TimetableCoordinates.getTime . Model.Timetable.getBookedCoordinates	-- Group into synchronous bookings.
		) $ LinearModel.Timetable.fromTimetable synchronisedStudentViewTimetable
	)
 ) . Data.Map.filterWithKey (
	\studentBody _	-> Data.Set.member studentBody $ ProblemConfiguration.ProblemAnalysis.getStudentBodiesBySynchronisationId problemAnalysis ! synchronisationId	-- Select from the timetable, those student-bodies who require the specified synchronised course.
 ) -- Identify bookings which block a row of synchronous available timeslots.

{- |
	* For each /synchronised course/, mutates the specified /timetable/ by unbooking all the corresponding /lesson/s.
	This is performed without reference to any problem with the current booking.

	* By unbooking all /lesson/s for a /synchronised course/,
	this strategy automatically accounts for the possibility that it might also have a non-trivial /minimumConsecutiveLessons/.

	* Since /student/s are typically fully booked, there will be relatively few unallocated /time-slot/s (other than those reserved for /meeting/s),
	even fewer of which will by coincidence be synchronised;
	therefore most attempts to relocate the /lesson/s of /synchronised course/s, will fail.
	To liberate some space in which improvements might occur,
	any /lesson/s which block the /booking/ of the synchronous /lesson/s required for the deleted /synchronised course/
	(except when that /lesson/ is either at an explicitly specified /time/, or when it completes a runlength of /minimumConsecutiveLessons/),
	are deleted first.

	* The supplied random generator is unused, so the result is deterministic.
-}
studentViewTimetableSynchronisedCourseDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			synchronisationId,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableSynchronisedCourseDepleter problemAnalysis problemParameters _ studentViewTimetable	= [
	Model.Timetable.undefineTimeslots (
		Data.Maybe.maybe studentViewTimetable (
			Model.Timetable.undefineTimeslots studentViewTimetable
		) . Data.Map.lookup synchronisationId $ Dynamic.StudentViewTimetableUtilities.locateLessonsBySynchronisationId problemParameters studentViewTimetable	-- Unbook any lessons for this synchronisationId.
	) synchronousTimetableCoordinatesVector {-deplete further-} |
		synchronisationId			<- ProblemConfiguration.ProblemAnalysis.findDistinctSynchronisationIds problemAnalysis,
		synchronousTimetableCoordinatesVector	<- findRelocatableSynchronousLessons problemAnalysis problemParameters synchronisationId studentViewTimetable {-the original one-}
 ] -- List-comprehension.

{- |
	* For each /synchronised course/, & then for each /day/, mutates the specified /timetable/ by unbooking all the corresponding /lesson/s.
	This is performed without reference to any problem with the current booking.

	* By unbooking all /lesson/s for one /synchronised course/ in one /day/,
	this strategy automatically accounts for the possibility that it might also have a non-trivial /minimumConsecutiveLessons/.

	* Since /student/s are typically fully booked, there will be relatively few unallocated /time-slot/s (other than those reserved for /meeting/s),
	even fewer of which will by coincidence be synchronised;
	therefore most attempts to relocate the /lesson/s of /synchronised course/s, will fail.
	To liberate some space in which improvements might occur,
	any /lesson/s which block the /booking/ of the synchronous /lesson/s required for the deleted /synchronised course/;
	except when the /lesson/ is at an explicitly specified /time/, or when it completes a runlength of /minimumConsecutiveLessons/;
	are deleted first.

	* The supplied random generator is unused, so the result is deterministic.
-}
studentViewTimetableSynchronisedCourseByDayDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			synchronisationId,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableSynchronisedCourseByDayDepleter problemAnalysis problemParameters _ studentViewTimetable	= [
	Model.Timetable.undefineTimeslots depletedStudentViewTimetable synchronousTimetableCoordinatesVector {-deplete further-} |
		(synchronisationId, coordinatesVector)	<- Data.Map.toList $ Dynamic.StudentViewTimetableUtilities.locateLessonsBySynchronisationId problemParameters studentViewTimetable,
		depletedStudentViewTimetable		<- map (
			Model.Timetable.undefineTimeslots studentViewTimetable {-deplete-}
		) $ Data.List.Extra.groupSortOn (
			Temporal.Time.getDay . Model.TimetableCoordinates.getTime
		) coordinatesVector {-not null-},
		synchronousTimetableCoordinatesVector	<- filter (
			(
				\day -> Data.Foldable.all {-student-body-} (
					Data.Foldable.all {-timeslotId-} (
						Data.Maybe.maybe True {-no lesson defined-} $ (/= Just synchronisationId) . Data.Course.getMaybeSynchronisationId . ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters
					) . (
						Data.Array.IArray.! day	-- Lookup the timetableForDay.
					) -- Section.
				) depletedStudentViewTimetable
			) . Temporal.Time.getDay . Model.TimetableCoordinates.getTime . head -- Select only those synchronous vectors whose common day doesn't contain any lessons for this synchronisationId.
		) $ findRelocatableSynchronousLessons problemAnalysis problemParameters synchronisationId studentViewTimetable {-the original one-}
 ] -- List-comprehension.

{- |
	* For each /student-body/, unbooks /lesson/s which block the completion of each /course/.

	* Avoids unbooking any /lesson/s whose /course/; requires a non-trivial /minimumConsecutiveLessons/, specifies the current booking-/time/, or is /synchronised/.
-}
studentViewTimetableIncompleteCourseDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableIncompleteCourseDepleter problemParameters _ studentViewTimetable	= [
	Model.Timetable.undefineTimeslotsFor studentBody studentViewTimetable bookingTimes {-deplete-} |
		(studentBody, incompletelyBookedKnowledge)	<- Data.Map.assocs . Data.Map.filter (
			not . Data.Set.null	-- Select only those student-bodies whose knowledge-requirements are incomplete.
		) . Data.Map.map Data.Student.amalgamateKnowledgeRequirements $ Dynamic.StudentViewTimetableUtilities.findIncompletelyBookedKnowledgeRequirementsByStudentBody problemParameters studentViewTimetable,
		let studentViewTimetableForWeek	= studentViewTimetable ! studentBody,
		studentViewLesson				<- filter (
			(`Data.Set.member` incompletelyBookedKnowledge) . Model.Lesson.getSubject	-- Select lessons from incomplete courses.
		) . Data.Set.toList $ Model.TimetableForWeek.extractDistinctLessons studentViewTimetableForWeek,
		let
			bookingTimes = filter (
				\time -> Model.TimetableForWeek.hasMatchingLessonAt (
					not . uncurry (||) . (
						(
							== studentViewLesson	-- Don't delete more lessons from the incomplete course.
						) &&& isBookingCumbersome problemParameters studentViewTimetable . (,) (studentBody, time) {-construct a booking from the blocking lesson-}
					) -- Predicate.
				) time studentViewTimetableForWeek
			 ) . map (
				Model.TimetableCoordinates.getTime . Model.Timetable.getBookedCoordinates	-- Extract the time at which these lessons were booked.
			 ) . filter (
				(== studentViewLesson) . Model.Timetable.getBookedLesson	-- Select identical lessons (subject, teacher, location), to those from the incomplete course of the original student-body.
			 ) . LinearModel.Timetable.fromTimetable $ Data.Map.delete studentBody studentViewTimetable {-select all the other student-bodies-},
		not $ null bookingTimes
 ] -- List-comprehension.

{- |
	* Unbooks groups of synchronous heterogeneous /lesson/s.

	* Avoids unbooking any /lesson/s, whose runlength is irreducible, or which are booked at /time/s specified by the /course/.

	* The supplied random generator is unused, so the result is deterministic.
-}
studentViewTimetableSynchronousLessonDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableSynchronousLessonDepleter problemParameters _ studentViewTimetable	= map (
	Model.Timetable.undefineTimeslots studentViewTimetable . map Model.Timetable.getBookedCoordinates
 ) . filter (
	not . null
 ) . map (
	filter $ (
		\((studentViewTimetableForWeek, time), course)	-> uncurry (&&) $ (
			Model.TimetableForWeek.isRunlengthReducibleAt studentViewTimetableForWeek time &&& not . Data.Course.isASpecifiedTime time
		) course
	) . (
		(
			(
				(studentViewTimetable !) . Model.TimetableCoordinates.getObserverId &&& Model.TimetableCoordinates.getTime
			) . Model.Timetable.getBookedCoordinates
		) &&& ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters . Model.Timetable.getBookedLesson
	)
 ) . Data.List.Extra.groupSortOn (
	Model.TimetableCoordinates.getTime . Model.Timetable.getBookedCoordinates	-- Group into synchronous bookings.
 ) $ LinearModel.Timetable.fromTimetable studentViewTimetable

{- |
	* For each /student-body/ in isolation, remove all /lesson/s from combinations of the specified number of /day/s.

	* If the number of days is unspecified, then try all possible values.

	* Avoids any /lesson/s whose /course/ is /synchronised/, or which specifies precise booking-times on this /day/, even if not at the time of the current /lesson/.

	* The supplied random generator is unused, so the result is deterministic.
-}
studentViewTimetableStudentViewTimetableForDayDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			teacherId
 )
	=> Maybe Size.NDays	-- ^ The number of whole /day/s to unbook in a student-view timetable-for-week; no specification is interpreted as all available /day/s.
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableStudentViewTimetableForDayDepleter (Just 0) _ _ _					= []
studentViewTimetableStudentViewTimetableForDayDepleter maybeNDays problemParameters _ studentViewTimetable	= [
	Model.Timetable.undefineTimeslotsFor studentBody studentViewTimetable $ concatMap (
		\day -> [
			Temporal.Time.mkTime day timeslotId |
				let studentViewTimetableForDay	= studentViewTimetable ! studentBody Data.Array.IArray.! day,
				(timeslotId, Just studentViewLesson)	<- Data.Array.IArray.assocs studentViewTimetableForDay,	-- Select the bookings for this student-body's day.
				not . uncurry (||) . (
					Data.Course.isSynchronised &&& (
						\course -> any (
							(`Data.Course.isASpecifiedTime` course) . Temporal.Time.mkTime day	-- Avoid unbooking this lesson if the runlength here spans a booking-time specified by the course.
						) $ Model.TimetableForDay.spanRunlengthAt timeslotId studentViewTimetableForDay
					) -- Avoid unbooking lessons, either from synchronised courses which are hard to relocate, or booked @ times specified for the course.
				) $ ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson
		 ] -- List-comprehension.
	) days |
		studentBody	<- Data.Map.keys studentViewTimetable,
		days		<- filter (
			Data.Maybe.maybe (
				not . null	-- The order of the sets of days is irrelevant.
			) (
				\nDays -> (== nDays) . length	-- CAVEAT: assumes that the student-body's availability covers at least this number of days.
			) maybeNDays
		 ) . Data.List.subsequences . Data.Set.toList . Temporal.Availability.deconstruct . Data.Resource.getAvailability $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters ! studentBody
 ] -- List-comprehension.

{- |
	* From the specified /timetable-coordinate/s & /course/, determine any /minimum consecutive lessons/ & /synchronised courses/, & from these locate the coordinates of associated /booking/s.

	* Returns the coordinates of all the associated /booking/s, together with the original coordinates.
-}
locateConsecutivesAndSynchronous :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Eq			teacherId,
	Ord			synchronisationId
 )
	=> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> StudentView.TimetableCoordinates.Coordinates timeslotId
	-> Data.Course.Course synchronisationId level timeslotId
	-> [StudentView.TimetableCoordinates.Coordinates timeslotId]
locateConsecutivesAndSynchronous problemAnalysis studentViewTimetable studentViewTimetableCoordinates course	= (
	\runlengthTimes -> map (
		(,) studentBody	-- Construct coordinates.
	) runlengthTimes ++ Data.Maybe.maybe [] (
		(
			<*> runlengthTimes	-- Construct coordinates from the permutations of each synchronised student-body & each time in the runlength.
		) . map (,) {-partially apply-} . Data.Set.toList . Data.Set.delete studentBody . (
			ProblemConfiguration.ProblemAnalysis.getStudentBodiesBySynchronisationId problemAnalysis !
		)
	) (Data.Course.getMaybeSynchronisationId course)
 ) $ if Data.Course.getMinimumConsecutiveLessons course > 1
	then let
		(day, timeslotId)	= Temporal.Time.getDay &&& Temporal.Time.getTimeslotId $ time	-- Deconstruct.
	in uncurry map $ (
		Temporal.Time.mkTime &&& Model.TimetableForDay.spanRunlengthAt timeslotId . (
			studentViewTimetable ! studentBody Data.Array.IArray.!	-- Lookup the timetableForDay.
		)
	) day	-- Construct consecutive times for each member of the runlength.
	else [time] {-singleton-} where
		(studentBody, time)	= Model.TimetableCoordinates.getObserverId &&& Model.TimetableCoordinates.getTime $ studentViewTimetableCoordinates	-- Deconstruct.

{- |
	* For each /student-body/ in isolation, over the specified number of random trials, unbook a small number of randomly selected /lesson/s.

	* Avoids deleting any /lesson/s which are at /time/s specified for their /course/.

	* If the /course/ for the randomly selected /lesson/, defines a non-trivial /minimumConsecutiveLessons/, then unbooks the whole runlength;
	even if that includes a /lesson/ at a /time/ specified for the /course/, since the runlength could be shifted around this /time/.

	* If any unbooked /lesson/ is from a /synchronised course/, unbooks the synchronised /lesson/s for the other interested /student-bodies/.
-}
studentViewTimetableStudentViewTimetableForWeekDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			level,
	Eq			locationId,
	Ord			synchronisationId,
	Ord			teacherId,
	System.Random.RandomGen	randomGen
 )
	=> Int			-- ^ The number of random trials.
	-> Size.NTimeslots	-- ^ The number of /lesson/s to unbook in each trial.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableStudentViewTimetableForWeekDepleter 0 {-nTrials-} _ _ _ _ _									= []
studentViewTimetableStudentViewTimetableForWeekDepleter _ 0 {-nTimeslots-} _ _ _ _								= []
studentViewTimetableStudentViewTimetableForWeekDepleter nTrials nTimeslots problemAnalysis problemParameters randomGen studentViewTimetable	= [
	Model.Timetable.undefineTimeslots studentViewTimetable {-deplete-} . Data.Set.fromList {-de-duplicate-} . concatMap (
		uncurry (locateConsecutivesAndSynchronous problemAnalysis studentViewTimetable) . Control.Arrow.first ((,) studentBody) {-construct coordinates-}
	) . take nTimeslots {-potentially fewer-} . ToolShed.System.Random.shuffle randomGen' . filter (
		not . uncurry Data.Course.isASpecifiedTime	-- The probability of improving such bookings, is too low to warrant the effort.
	) . map (
		Model.TimetableForWeek.getBookedTime &&& ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters . Model.TimetableForWeek.getBookedLesson
	) $ LinearModel.TimetableForWeek.fromTimetableForWeek studentViewTimetableForWeek |
		(studentBody, studentViewTimetableForWeek)	<- Data.Map.toList studentViewTimetable,
		randomGen'					<- take nTrials $ ToolShed.System.Random.randomGens randomGen
 ] -- List-comprehension.

{- |
	* Over the specified number of random trials, unbook a number of randomly selected /lesson/s.

	* Avoids deleting any /lesson/s which are at /time/s specified for their /course/.

	* If the /course/ for the randomly selected /lesson/, defines a non-trivial /minimumConsecutiveLessons/, then unbooks the whole runlength;
	even if that includes a /lesson/ at a /time/ specified for the /course/, since the runlength could be shifted around this /time/.

	* If any unbooked /lesson/ is from a /synchronised course/, unbooks the synchronised /lesson/s for the other interested /student-bodies/.
-}
studentViewTimetableRandomDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			locationId,
	Eq			level,
	Ord			synchronisationId,
	Ord			teacherId,
	System.Random.RandomGen	randomGen
 )
	=> Int			-- ^ The number of random trials.
	-> Size.NTimeslots	-- ^ The number of /lesson/s to unbook in each trial.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableRandomDepleter 0 {-nTrials-} _ _ _ _ _								= []
studentViewTimetableRandomDepleter _ 0 {-nTimeslots-} _ _ _ _								= []
studentViewTimetableRandomDepleter nTrials nTimeslots problemAnalysis problemParameters randomGen studentViewTimetable	= map (
	\randomGen'	-> Model.Timetable.undefineTimeslots studentViewTimetable {-deplete-} . Data.Set.fromList {-de-duplicate-} . concatMap (
		uncurry $ locateConsecutivesAndSynchronous problemAnalysis studentViewTimetable
	) . take nTimeslots {-potentially fewer-} . ToolShed.System.Random.shuffle randomGen' . filter (
		not . uncurry Data.Course.isASpecifiedTime . Control.Arrow.first Model.TimetableCoordinates.getTime	-- The probability of improving such bookings, is too low to warrant the effort.
	) . map (
		Model.Timetable.getBookedCoordinates &&& ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters . Model.Timetable.getBookedLesson
	) $ LinearModel.Timetable.fromTimetable studentViewTimetable
 ) . take nTrials $ ToolShed.System.Random.randomGens randomGen

{- |
	* For each /student-body/ in isolation, removes all /lesson/s which haven't been merged with another /student-body/ to form a /student-class/.

	* Avoids any /lesson/s whose /course/ is /synchronised/.

	* Similar to 'studentViewTimetableRandomBreederByHomogeneousStudentViewLessonMutation', but only applied to singleton /student-class/es, & to isolated /timetableForWeek/s.

	* The supplied random generator is unused, so the result is deterministic.
-}
studentViewTimetableSingletonStudentClassDepleter :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentViewTimetableRandomBreeder randomGen timeslotId locationId teacherId level
studentViewTimetableSingletonStudentClassDepleter problemParameters _ studentViewTimetable	= [
	Model.Timetable.undefineTimeslotsFor studentBody studentViewTimetable times {-deplete-} |
		(studentViewLesson, studentClassByTime)	<- Data.Map.toList $ StudentView.Timetable.findStudentClassByTimeByLesson studentViewTimetable,
		not . Data.Course.isSynchronised $ ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters studentViewLesson,	-- Exclude lessons for synchronised courses, since these can't be relocated independently.
		(studentBody, times)			<- map (
			Model.TimetableCoordinates.getObserverId . head &&& map Model.TimetableCoordinates.getTime	-- All coordinates exist in the timetableForWeek of a single student-body.
		) . Data.List.Extra.groupSortOn Model.TimetableCoordinates.getObserverId {-student-body-} . map (
			\(time, studentClass)	-> (Data.Set.findMin studentClass {-extract the single student-body-}, time)	-- Construct coordinates.
		) . Data.Map.toList $ Data.Map.filter (
			(== 1) . Data.Set.size	-- Select singleton student-classes; i.e. composed from just one student-body (though potentially more than one student).
		) studentClassByTime
 ] -- List-comprehension.

