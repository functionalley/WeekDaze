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
	& on the dynamic state of the current 'TeacherView.Timetable.Timetable'.
-}

module WeekDaze.Dynamic.TeacherViewTimetableUtilities(
-- * Functions
	calculateUtilisationRatioByTeacherId,
	calculateAverageAbsoluteDeviationInUtilisationRatio,
	countFreeLessons,
	countInterCampusMigrations,
	countInterCampusMigrationsByTeacherId,
	calculateMeanFreePeriodCompliance,
	findUnbookedSpecifiedTimesByTeacherId,
	calculateRatioOfAsynchronousLessonsInSynchronisedCourses,
	calculateMeanRatioOfStudentClassSizeToLocationCapacity,
-- ** Translation
	fromStudentViewTimetable
) where

import			Control.Arrow((&&&), (***))
import			Data.Map((!))
import			Data.Set((\\))
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Set
import qualified	Factory.Math.Statistics
import qualified	WeekDaze.Aggregate.GroupCatalogue			as Aggregate.GroupCatalogue
import qualified	WeekDaze.Aggregate.StudentClass				as Aggregate.StudentClass
import qualified	WeekDaze.Data.Course					as Data.Course
import qualified	WeekDaze.Data.HumanResource				as Data.HumanResource
import qualified	WeekDaze.Data.Location					as Data.Location
import qualified	WeekDaze.Data.Teacher					as Data.Teacher
import qualified	WeekDaze.Dynamic.TimetableForWeekUtilities		as Dynamic.TimetableForWeekUtilities
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.LinearModel.TimetableForWeek			as LinearModel.TimetableForWeek
import qualified	WeekDaze.Model.Lesson					as Model.Lesson
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.Model.TimetableForWeek				as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.Size						as Size
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.TeacherView.LessonResourceIds			as TeacherView.LessonResourceIds
import qualified	WeekDaze.TeacherView.Timetable				as TeacherView.Timetable
import qualified	WeekDaze.Temporal.TimeslotRequest			as Temporal.TimeslotRequest

-- | A convenient interface to 'TeacherView.Timetable.fromStudentViewTimetable'.
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
	=> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio	-- ^ Static configuration-data.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level							-- ^ The /timetable/ to invert.
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level
fromStudentViewTimetable executionOptions problemAnalysis	= TeacherView.Timetable.fromStudentViewTimetable (
	ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger executionOptions
 ) $ ProblemConfiguration.ProblemAnalysis.getFreeTeacherViewTimetable problemAnalysis

{- |
	The number of /lesson/s booked for each /teacher/, relative to the contractual limit of their teaching-time;
	based on 'Model.Timetable.calculateUtilisationRatioByObserverId'.
-}
calculateUtilisationRatioByTeacherId :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level
	-> Data.Map.Map teacherId teachingRatio
calculateUtilisationRatioByTeacherId problemParameters problemAnalysis	= Model.Timetable.calculateUtilisationRatioByObserverId (ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters) (ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis)

-- | Measures the /average absolute deviation/ (<https://en.wikipedia.org/wiki/Absolute_deviation#Average_absolute_deviation>) in the relative /workload/ amongst /teacher/s.
calculateAverageAbsoluteDeviationInUtilisationRatio :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		average,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Static configuration-data.
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level									-- ^ The natural view from which to extract the required results.
	-> average
calculateAverageAbsoluteDeviationInUtilisationRatio problemParameters problemAnalysis	= Factory.Math.Statistics.getAverageAbsoluteDeviation . calculateUtilisationRatioByTeacherId problemParameters problemAnalysis

{- |
	Counts the total number of unallocated /time-slot/s for all /teacher/s in one week,
	discounting /day/s on which the /teacher/ is unavailable, & also /time-slot/s which are allocated to either administration or /meeting/s.
-}
countFreeLessons :: (
	Data.Array.IArray.Ix	timeslotId,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level
	-> Size.NTimeslots
countFreeLessons problemParameters problemAnalysis = Data.Map.foldrWithKey (
	\teacherId -> let
		teacherProfile	= ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters ! teacherId
	in (+) . subtract (
		Data.HumanResource.getNTimeslotsPerWeekOfNonTeaching (ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis) teacherProfile	-- Includes both administration & meetings.
	) . Model.TimetableForWeek.countUnallocatedAvailableTimeslots teacherProfile
 ) 0

-- | Counts the total number of times /teacher/s must change /campus/ within a /day/, during the /week/.
countInterCampusMigrations :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			campus,
	Ord			locationId,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level
	-> Size.NTimeslots
countInterCampusMigrations problemParameters	= Data.Foldable.sum . countInterCampusMigrationsByTeacherId problemParameters

-- | Constructs a map indexed by /teacher-id/ of the number of inter-/campus/ migrations of /teacher/s, within a /day/, during the week.
countInterCampusMigrationsByTeacherId :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			campus,
	Ord			locationId,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level
	-> TeacherView.Timetable.InterCampusMigrationsByTeacherId teacherId
countInterCampusMigrationsByTeacherId problemParameters teacherViewTimetable	= Data.Map.mapWithKey (
	\teacherId teacherProfile	-> Dynamic.TimetableForWeekUtilities.countInterCampusMigrations problemParameters teacherProfile TeacherView.LessonResourceIds.getLocationId $ teacherViewTimetable ! teacherId
 ) $ ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters

{- |
	Calculates the mean, over those /day/s on which the /teacher/ is available,
	of the ratio of those free (neither booked with a /lesson/, not reserved for a /meeting/) /time-slot/s which comply with the specified preference,
	to the total number of free /time-slot/s.
-}
calculateMeanFreePeriodCompliance :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		ratio,
	Ord			teacherId,
	RealFrac		teachingRatio
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level	-- ^ The /timetable/ as seen from the /teacher/'s perspective.
	-> ratio
calculateMeanFreePeriodCompliance problemParameters	= Model.Timetable.calculateMeanFreePeriodCompliance (
	ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters
 ) (
	Aggregate.GroupCatalogue.getMeetingTimes (ProblemConfiguration.ProblemParameters.getGroupCatalogue problemParameters) . Data.HumanResource.getGroupMembership
 )

-- | Map by /teacherId/, the /time/s specified by the /course/, which haven't been booked, for /course/s which have.
findUnbookedSpecifiedTimesByTeacherId :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			level,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level
	-> TeacherView.Timetable.TimesByTeacherId teacherId timeslotId
findUnbookedSpecifiedTimesByTeacherId problemParameters teacherViewTimetable	= Data.Map.mapWithKey (
	\teacherId -> Data.Set.foldr (
		\course s	-> let
			bookedTimes	= map Model.TimetableForWeek.getBookedTime . filter (
				(== Data.Course.getSubject course) . Model.Lesson.getSubject . Model.TimetableForWeek.getBookedLesson
			 ) . LinearModel.TimetableForWeek.fromTimetableForWeek $ teacherViewTimetable ! teacherId
		in if null bookedTimes
			then s	-- The course hasn't been booked at all.
			else Data.Set.union s $ Temporal.TimeslotRequest.getSpecifiedTimes (Data.Course.getTimeslotRequest course) \\ Data.Set.fromList bookedTimes
	) Data.Set.empty . Data.Teacher.getService
 ) $ ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters

{- |
	* It is desirable that the /student-class/es for the /lesson/s booked for a /course/, are composed consistently from the same /student-bodies/.

	* When /synchronised course/s exist, an additional associated problem arises; though all the /synchronised course/s may be free from /student-body/-combinations,
	the synchronous /lesson/s, don't necessarily have a consistent set of /student-class/es.
	As a result, should one /student-body/ decide to migrate to another /course/ within a synchronised set,
	they'll find, as required, that the /booking-time/s for the new /course/ match those of the old /course/,
	but the /student-class/ into which they'll temporarily merge, depends on the /booking-time/; i.e. a /student-body combination/ has been formed.

	* Returns the ratio of those /booking/s for /synchronised course/s, which aren't consistently synchronised; to all such /booking/s.

	* Since a solitary /booking/ can't be inconsistently synchronised, the first /booking/ doesn't factor in the ratio.

	* E.g. for 4 /student-bodies/;
	S1 & S3 who require two /lesson/s in /Course a/, offered by /Teacher 1/;
	S2 & S4 who require the synchronised /Course b/, offered by /Teacher 2/.

> E.g. 1:			Time 1	Time 2	Time 3
>				======	======	======
>	(Teacher 1,Course a)	{S1,S3}	{S1,S3}
>	(Teacher 2,Course b)	{S2,S4}		{S2,S4}
>				======================
>				[
>					{(Teacher 1,{S1,S3}), (Teacher 2,{S2,S4})},
>					{(Teacher 1,{S1,S3})},
>					{(Teacher 2,{S2,S4})}
>				] => (pred 3 / pred 4) == 2/3.
>				If either S2 or S4 were to transfer to Course a, then a student-body combination would result.
>
> E.g. 2:			Time 1	Time 2	Time 3	Time 4
>				======	======	======	======
>	(Teacher 1,Course a)	{S1,S3}	{S1,S3}
>	(Teacher 2,Course b)	{S2}	{S4}	{S2}	{S4}
>				==============================
>				[
>					{(Teacher 1,{S1,S3}), (Teacher 2,{S2})},
>					{(Teacher 1,{S1,S3}), (Teacher 2,{S4})},
>					{(Teacher 2,{S2})},
>					{(Teacher 2,{S4})}
>				] => (pred 4 / pred 6) == 3/5.
>				If either S2 or S4 were to transfer to Course a, then a student-body combination would result.
>
> E.g. 3:			Time 1	Time 2	Time 3	Time 4
>				======	======	======	======
>	(Teacher 1,Course a)	{S1,S3}		{S1,S3}
>	(Teacher 2,Course b)		{S2,S4}		{S2,S4}
>				==============================
>				[
>					{(Teacher 1,{S1,S3})},
>					{(Teacher 2,{S2,S4})}
>				] => (pred 2 / pred 4) == 1/3.
>
> E.g. 4:			Time 1	Time 2
>				======	======
>	(Teacher 1,Course a)	{S1}	{S3}	{S1}	{S3}
>	(Teacher 2,Course b)	{S2}	{S4}	{S2}	{S4}
>				==============
>				[
>					{(Teacher 1,{S1}), (Teacher 2,{S2})},
>					{(Teacher 1,{S3}), (Teacher 2,{S4})}
>				] => (pred 2 / pred 8) == 1/7.
>
> E.g. 5:			Time 1	Time 2
>				======	======
>	(Teacher 1,Course a)	{S1,S3}	{S1,S3}
>	(Teacher 2,Course b)	{S2,S4}	{S2,S4}
>				==============
>				[
>					{(Teacher 1,{S1,S3}), (Teacher 2,{S2,S4})}
>				] => (pred 1 / pred 4) == 0.

	* CAVEAT: neither this function nor the above examples, aim to measure either;
	incomplete /booking/s of a /course/, by a /student-body/ (see 'WeekDaze.Dynamic.StudentViewTimetableUtilities.findMeanRatioOfIncompletelyBookedKnowledgeRequirements');
	or /student-body combinations/ (see 'calculateRatioOfStudentClassCombinationsPerSynchronisedLesson');
	since these can be measured independently.
-}
calculateRatioOfAsynchronousLessonsInSynchronisedCourses :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			level,
	Fractional		ratio,
	Ord			teacherId
 )
	=> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level	-- ^ The /timetable/ as seen from the /teacher/'s perspective.
	-> ratio
calculateRatioOfAsynchronousLessonsInSynchronisedCourses problemAnalysis teacherViewTimetable
	| denominator == 0	= 0	-- Zero lessons were booked for synchronised courses, & therefore none can be booked asynchronously.
	| otherwise		= fromIntegral numerator / fromIntegral denominator
	where
		(numerator, denominator)	= Data.Map.foldr (
			uncurry (***) . (
				(+) . pred {-one is the minimum-} . Data.Set.size . Data.Set.fromList {-construct set of sets of pairs-} . Data.Map.elems {-discard booking-time-} &&& (+) . pred {-one is the minimum-} . Data.Map.size	-- Independently accumulate the numerator & denominator.
			)
		 ) (0, 0) . Data.Map.filter (
			(> 1) . Data.Map.size	-- Select synchronised courses with more than one booking-time, & therefore the potential to lack synchronisation.
		 ) . Data.Map.map (
			\synchronisedCoursesByTeacherId -> Data.Map.fromListWith Data.Set.union {-of (teacherId,student-class)-} [
				(
					Model.TimetableForWeek.getBookedTime teacherViewBooking,
					Data.Set.singleton (
						teacherId,
						TeacherView.LessonResourceIds.getStudentClass $ Model.Lesson.getResourceIds teacherViewLesson
					) -- Pair.
				) |
					(teacherId, synchronisedCourse)	<- Data.Map.toList synchronisedCoursesByTeacherId,	-- For this synchronisationId, identify the course offered by each teacher.
					teacherViewBooking		<- LinearModel.TimetableForWeek.fromTimetableForWeek $ teacherViewTimetable ! teacherId,
					let teacherViewLesson	= Model.TimetableForWeek.getBookedLesson teacherViewBooking,
					Model.Lesson.getSubject teacherViewLesson == Data.Course.getSubject synchronisedCourse	-- Select bookings in the synchronised course under investigation.
			] -- List-comprehension.
		 ) $ ProblemConfiguration.ProblemAnalysis.getCoursesByTeacherIdBySynchronisationId problemAnalysis	-- Assess each synchronisationId independently.

{- |
	* Finds the mean over all /booking/s, of the ratio of the /student-class/' size, to the /location/'s /capacity/.

	* CAVEAT: /location/s which are unbooked don't affect the result.

	* CAVEAT: no weighting is applied; so a ratio composed from @(100 / 200)@ is no more significant than one composed from @(1 / 2)@.
-}
calculateMeanRatioOfStudentClassSizeToLocationCapacity :: (
	Data.Array.IArray.Ix	timeslotId,
	Fractional		averageRatio,
	Ord			locationId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> TeacherView.Timetable.Timetable teacherId timeslotId locationId level	-- ^ The /timetable/ as seen from the /teacher/'s perspective.
	-> averageRatio
calculateMeanRatioOfStudentClassSizeToLocationCapacity problemParameters teacherViewTimetable
	| Data.Map.null studentClassesByLocationId	= 0	-- All locations have zero classes booked.
	| otherwise					= Factory.Math.Statistics.getMean [
		toRational (
			Aggregate.StudentClass.getSize studentClass
		) / fromIntegral (
			Data.Location.getCapacity $ ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters ! locationId
		) |
			(locationId, studentClasses)	<- Data.Map.toList studentClassesByLocationId,
			studentClass			<- studentClasses
	] -- List-comprehension.
	where
		studentClassesByLocationId	= TeacherView.Timetable.findStudentClassesByLocationId teacherViewTimetable

