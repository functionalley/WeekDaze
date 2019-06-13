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

	* Loosely connected utilities, which rely only on the 'ProblemConfiguration.ProblemParameters.ProblemParameters',
	rather than on the dynamically changing state of the /timetable/.

	* As a result, some of these functions can effectively be cached, after the problem has been defined.
-}

module WeekDaze.ProblemConfiguration.ProblemAnalysis(
-- * Types
-- ** Type-synonyms
--	CoursesByTeacherIdBySubjectByStudentBody,
--	DistinctTimeslotRunlengthsByCourse,
--	MeetingResourcesByTime,
--	LessonsByStudentBody,
--	LessonsByStudentBodyByDay,
--	StudentBodiesBySynchronisationId,
-- ** Data-types
	ProblemAnalysis(
--		MkProblemAnalysis,
		getCoursesByTeacherIdBySynchronisationId,
		getCoursesRequestingSeparatedLessonsWithinAnyDay,
		getDistinctRunlengthsOfSpecifiedTimesByCourse,
--		getDistinctSubjects,
		getFreeLocationViewTimetable,
		getFreeStudentViewTimetable,
		getFreeTeacherViewTimetable,
		getMeetingResourcesByTime,
		getMeetingsByTime,
		getNAvailableDaysPerStudentTimetable,
		getNAvailableDaysPerTeacherTimetable,
		getNStudents,
		getNTeachers,
--		getNTeacherInternalAvailabilityGaps,
--		getSpecifiedTimes,
		getNTimeslotsPerDay,
		getRequiredLessonCombinationsByStudentBody,
		getRequiredLessonCombinationsByStudentBodyAvailableByDay,
		getStudentBodiesBySynchronisationId,
		getSubjectsWithAnIdealTimeslotRequest,
		getSuitableCoursesByTeacherIdBySubjectByStudentBody,
		getTimeslotIdRange,
		getTotalLessonsPerWeekByFacilityName
	),
-- * Functions
	countStudentPlaces,
--	findCoursesFor,
	findCourseFor,
	findCourseForLocationViewLesson,
	findCourseForTeacherViewLesson,
	findDistinctSynchronisationIds,
--	findMeetingsByTime,
--	findMeetingResourcesByTime,
--	findCoursesRequestingSeparatedLessonsWithinAnyDay,
--	findSubjectsWithAnIdealTimeslotRequest,
--	findDistinctRunlengthsOfSpecifiedTimesByCourse,
--	findStudentBodiesBySynchronisationId,
--	findSuitableCoursesByTeacherIdBySubjectByStudentBody,
--	findSuitableLocations,
	lookupCourseFor,
--	generateRequiredLessonCombinationsByStudentBody,
--	generateRequiredLessonCombinationsByStudentBodyAvailableByDay,
-- ** Constructor
	mkProblemAnalysis,
-- ** Predicates
	areAnyResourcesBookedForGroupMeeting,
	areAvailableResources,
	doAllCoursesSatifyingAnotherKnowledgeRequirementSpecifyThisTime,
	isFree,
	isValidTimeslotId
) where

import			Control.Arrow((&&&), (***))
import			Data.Map((!))
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Factory.Data.Interval
import qualified	WeekDaze.Aggregate.GroupCatalogue			as Aggregate.GroupCatalogue
import qualified	WeekDaze.Aggregate.LocationCatalogue			as Aggregate.LocationCatalogue
import qualified	WeekDaze.Aggregate.StudentBody				as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentBodyRegister			as Aggregate.StudentBodyRegister
import qualified	WeekDaze.Aggregate.StudentClass				as Aggregate.StudentClass
import qualified	WeekDaze.Aggregate.TeacherRegister			as Aggregate.TeacherRegister
import qualified	WeekDaze.Data.Course					as Data.Course
import qualified	WeekDaze.Data.Group					as Data.Group
import qualified	WeekDaze.Data.HumanResource				as Data.HumanResource
import qualified	WeekDaze.Data.Location					as Data.Location
import qualified	WeekDaze.Data.Resource					as Data.Resource
import qualified	WeekDaze.Data.Student					as Data.Student
import qualified	WeekDaze.Data.Subject					as Data.Subject
import qualified	WeekDaze.Data.Teacher					as Data.Teacher
import qualified	WeekDaze.LocationView.Lesson				as LocationView.Lesson
import qualified	WeekDaze.LocationView.Timetable				as LocationView.Timetable
import qualified	WeekDaze.Model.Lesson					as Model.Lesson
import qualified	WeekDaze.Model.Meeting					as Model.Meeting
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.Size						as Size
import qualified	WeekDaze.StudentView.Lesson				as StudentView.Lesson
import qualified	WeekDaze.StudentView.LessonResourceIds			as StudentView.LessonResourceIds
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.StudentView.TimetableCoordinates		as StudentView.TimetableCoordinates
import qualified	WeekDaze.TeacherView.Lesson				as TeacherView.Lesson
import qualified	WeekDaze.TeacherView.Timetable				as TeacherView.Timetable
import qualified	WeekDaze.Temporal.Availability				as Temporal.Availability
import qualified	WeekDaze.Temporal.Day					as Temporal.Day
import qualified	WeekDaze.Temporal.Time					as Temporal.Time
import qualified	WeekDaze.Temporal.TimeslotRequest			as Temporal.TimeslotRequest

{- |
	* A map by /student-body/, of maps by /subject/, of maps by /teacherId/, of /course/s.

	* CAVEAT: it is assumed that no /teacher/ can offer more than one suitable /course/.
-}
type CoursesByTeacherIdBySubjectByStudentBody synchronisationId level teacherId timeslotId	= Data.Map.Map Aggregate.StudentBody.StudentBody (Data.Map.Map (Data.Subject.Subject level) (Data.Map.Map teacherId (Data.Course.Course synchronisationId level timeslotId)))

-- | Sets of distinct /time-slot/ run-lengths, for each /course/.
type DistinctTimeslotRunlengthsByCourse synchronisationId level timeslotId	= Data.Map.Map (Data.Course.Course synchronisationId level timeslotId) (Data.Set.Set Size.NTimeslots)

-- | A map by /time/, of sets of /resource/s.
type MeetingResourcesByTime timeslotId locationId teacherId	= Data.Map.Map (Temporal.Time.Time timeslotId) (Data.Set.Set locationId, Data.Set.Set Aggregate.StudentBody.StudentBody, Data.Set.Set teacherId)

-- | A map by /student-body/, of /lesson/s; as returned by 'generateRequiredLessonCombinationsByStudentBody'.
type LessonsByStudentBody locationId teacherId level	= Data.Map.Map Aggregate.StudentBody.StudentBody [StudentView.Lesson.Lesson locationId teacherId level]

-- | A map by /day/, of maps by /student-body/, of /lesson/s; as returned by 'generateRequiredLessonCombinationsByStudentBodyAvailableByDay'.
type LessonsByStudentBodyByDay locationId teacherId level	= Data.Map.Map Temporal.Day.Day (LessonsByStudentBody locationId teacherId level)

-- | A map by /synchronisationId/, of /student-bodies/ who require a /subject/ from one of the corresponding /course/s.
type StudentBodiesBySynchronisationId synchronisationId	= Data.Map.Map synchronisationId (Data.Set.Set Aggregate.StudentBody.StudentBody)

-- | The results of the analysis of 'ProblemConfiguration.ProblemParameters.ProblemParameters'.
data ProblemAnalysis level locationId synchronisationId teacherId timeslotId	= MkProblemAnalysis {
	getCoursesByTeacherIdBySynchronisationId		:: Aggregate.TeacherRegister.CoursesByTeacherIdBySynchronisationId synchronisationId teacherId level timeslotId,	-- ^ Index /course/s by the /teacherId/ who offers them, & by their /synchronisationId/.
	getCoursesRequestingSeparatedLessonsWithinAnyDay	:: Data.Set.Set (Data.Course.Course synchronisationId level timeslotId),						-- ^ Identifies /course/s which specify /time/s, separated within a single /day/.
	getDistinctRunlengthsOfSpecifiedTimesByCourse		:: DistinctTimeslotRunlengthsByCourse synchronisationId level timeslotId,						-- ^ The distinct runlengths of consecutive specified /time/s, per /course/.
	getDistinctSubjects					:: Data.Subject.Knowledge level,											-- ^ The distinct /subject/s amongst the /service/s offered by /teacher/s.
	getFreeLocationViewTimetable				:: LocationView.Timetable.Timetable locationId timeslotId teacherId level,						-- ^ An unallocated /timetable/ indexed by /location/s.
	getFreeStudentViewTimetable				:: StudentView.Timetable.Timetable timeslotId locationId teacherId level,						-- ^ An unallocated /timetable/ indexed by /student-bodies/.
	getFreeTeacherViewTimetable				:: TeacherView.Timetable.Timetable teacherId timeslotId locationId level,						-- ^ An unallocated /timetable/ indexed by /teacher/s.
	getMeetingResourcesByTime				:: MeetingResourcesByTime timeslotId locationId teacherId,								-- ^ A map by /time/, of the /resource/s used by the /meeting/s of /group/s. CAVEAT: /time/s at which zero /meeting/s are booked, don't exist in the map.
	getMeetingsByTime					:: Model.Meeting.MeetingsByTime timeslotId locationId teacherId,							-- ^ A map by /time/, of the /meeting/s of /group/s with at least one member.
	getNAvailableDaysPerStudentTimetable			:: Size.NDays,														-- ^ The number of /day/s in a /student-timetable/, each multiplied by the size of the /student-body/ for which it's applicable.
	getNAvailableDaysPerTeacherTimetable			:: Size.NDays,														-- ^ The number of /day/s in a /teacher-timetable/.
	getNStudents						:: Size.NStudents,													-- ^ Counts the number of /student/s on the register.
	getNTeachers						:: Size.NTeachers,													-- ^ Counts the number of /teacher/s (excluding purely administrative staff) on the register.
	getNTeacherInternalAvailabilityGaps			:: Size.NDays,														-- ^ Counts the non-terminal (/available/ <-> un/available/)-day boundaries, over all /teacher/s.
	getNTimeslotsPerDay					:: Size.NTimeslots,													-- ^ The number of /time-slot/s in any /day/.
	getRequiredLessonCombinationsByStudentBodyAvailableByDay:: LessonsByStudentBodyByDay locationId teacherId level,								-- ^ The map by /day/, of maps by /student-body/, of combinations of possible /lesson/s.
	getRequiredLessonCombinationsByStudentBody		:: LessonsByStudentBody locationId teacherId level,									-- ^ The map by /student-body/, of combinations of possible /lesson/s.
	getSpecifiedTimes					:: Temporal.Time.TimeSet timeslotId,											-- ^ The set of all specified /time/s, for any /course/, offered by any /teacher/.
	getStudentBodiesBySynchronisationId			:: StudentBodiesBySynchronisationId synchronisationId,									-- ^ A map by /synchronisationId/, of /student-bodies/ who require a /subject/ from one of the corresponding /course/s.
	getSubjectsWithAnIdealTimeslotRequest			:: Data.Set.Set (Data.Subject.Subject level),										-- ^ The /subject/s, of the subset of all /course/s, which have an /ideal timeslot-request/.
	getSuitableCoursesByTeacherIdBySubjectByStudentBody	:: CoursesByTeacherIdBySubjectByStudentBody synchronisationId level teacherId timeslotId,				-- ^ Catalogues all the /course/s, which match each of the /knowledge-requirement/s, of each /student-body/.
	getTimeslotIdRange					:: [timeslotId],													-- ^ The range of /time-slot/s in any /day/.
	getTotalLessonsPerWeekByFacilityName			:: Data.Map.Map Data.Location.FacilityName Size.NTimeslots								-- ^ The total /lesson/s per week, required by all those /course/s needing a specified /facility/. CAVEAT: only the subset of /facilities/ offered, which are also required, are indexed.
}

-- | Constructor.
mkProblemAnalysis :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			synchronisationId,
	Ord			teacherId,
	RealFrac		teachingRatio,
	Show			locationId,
	Show			teacherId
 ) => ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> ProblemAnalysis level locationId synchronisationId teacherId timeslotId
mkProblemAnalysis problemParameters	= problemAnalysis where
	timeslotIdBounds	= ProblemConfiguration.ProblemParameters.getTimeslotIdBounds problemParameters
	teacherRegister		= ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters
	studentBodies		= Aggregate.StudentBodyRegister.getStudentBodies $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters

	problemAnalysis	= MkProblemAnalysis {
		getCoursesByTeacherIdBySynchronisationId			= Aggregate.TeacherRegister.findCoursesByTeacherIdBySynchronisationId teacherRegister,
		getCoursesRequestingSeparatedLessonsWithinAnyDay		= findCoursesRequestingSeparatedLessonsWithinAnyDay problemParameters,
		getDistinctRunlengthsOfSpecifiedTimesByCourse			= findDistinctRunlengthsOfSpecifiedTimesByCourse problemParameters,
		getDistinctSubjects						= Aggregate.TeacherRegister.extractDistinctSubjects teacherRegister,
		getFreeLocationViewTimetable					= Model.Timetable.mkFreeTimetable (Aggregate.LocationCatalogue.getLocationIds $ ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters) timeslotIdBounds,
		getFreeStudentViewTimetable					= Model.Timetable.mkFreeTimetable studentBodies timeslotIdBounds,
		getFreeTeacherViewTimetable					= Model.Timetable.mkFreeTimetable (Aggregate.TeacherRegister.getTeacherIds teacherRegister) timeslotIdBounds,
		getMeetingResourcesByTime					= findMeetingResourcesByTime problemParameters,
		getMeetingsByTime						= findMeetingsByTime problemParameters,
		getNAvailableDaysPerStudentTimetable				= Aggregate.StudentBodyRegister.countAvailableStudentDays $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters,
		getNAvailableDaysPerTeacherTimetable				= Aggregate.TeacherRegister.countAvailableTeacherDays $ ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters,
		getNStudents							= Aggregate.StudentClass.getSize studentBodies,
		getNTeachers							= Data.Map.size $ Data.Map.filter Data.Teacher.offersService teacherRegister,
		getNTeacherInternalAvailabilityGaps				= Data.Map.foldr ((+) . Temporal.Availability.countInternalAvailabilityGaps . Data.Resource.getAvailability) 0 teacherRegister,
		getNTimeslotsPerDay						= ProblemConfiguration.ProblemParameters.calculateNTimeslotsPerDay timeslotIdBounds,
		getRequiredLessonCombinationsByStudentBodyAvailableByDay	= generateRequiredLessonCombinationsByStudentBodyAvailableByDay problemParameters problemAnalysis {-recurse-},
		getRequiredLessonCombinationsByStudentBody			= generateRequiredLessonCombinationsByStudentBody problemParameters,
		getSpecifiedTimes						= Aggregate.TeacherRegister.findSpecifiedTimes teacherRegister,
		getStudentBodiesBySynchronisationId				= findStudentBodiesBySynchronisationId problemParameters problemAnalysis {-recurse-},
		getSubjectsWithAnIdealTimeslotRequest				= findSubjectsWithAnIdealTimeslotRequest problemParameters,
		getSuitableCoursesByTeacherIdBySubjectByStudentBody		= findSuitableCoursesByTeacherIdBySubjectByStudentBody problemParameters,
		getTimeslotIdRange						= Factory.Data.Interval.toList timeslotIdBounds,
		getTotalLessonsPerWeekByFacilityName				= Aggregate.TeacherRegister.countLessonsPerWeekByFacilityName teacherRegister
	}

{- |
	* Catalogue by /time/, the /meeting/s of /group/s which have at least one member.

	* CAVEAT: the map doesn't necessarily contain a key for each possible /time/.
-}
findMeetingsByTime :: (
	Ord		locationId,
	Ord		teacherId,
	Ord		timeslotId,
	RealFrac	teachingRatio,
	Show		locationId,
	Show		teacherId
 ) => ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> Model.Meeting.MeetingsByTime timeslotId locationId teacherId
findMeetingsByTime problemParameters	= Data.Set.foldr (
	\groupId m	-> let
		(meetingTimes, maybeLocationId)	= Data.Group.getMeetingTimes &&& Data.Group.getMaybeLocationId $ ProblemConfiguration.ProblemParameters.getGroupCatalogue problemParameters ! groupId

		getResourceIdSet :: Aggregate.GroupCatalogue.ResourceIdsByGroupId resourceId -> Data.Set.Set resourceId
		getResourceIdSet	= Data.Maybe.fromMaybe Data.Set.empty . Data.Map.lookup groupId
	in Data.Set.foldr (
		(
			$ (
				Data.Set.singleton . uncurry (
					Model.Meeting.mkMeeting groupId maybeLocationId
				) . (
					getResourceIdSet {-studentBody-set-} *** getResourceIdSet {-teacherId-set-}
				) $ ProblemConfiguration.ProblemParameters.findHumanResourceIdsByGroupId problemParameters
			) -- Section.
		) . Data.Map.insertWith Data.Set.union
	) m meetingTimes
 ) Data.Map.empty $ ProblemConfiguration.ProblemParameters.extractDistinctGroupMembership problemParameters

{- |
	* Catalogues by /time/, the /resource/s used for /meeting/s of /group/s.

	* CAVEAT: the map doesn't necessarily contain a key for each possible /time/.
-}
findMeetingResourcesByTime :: (
	Ord		locationId,
	Ord		teacherId,
	Ord		timeslotId,
	RealFrac	teachingRatio,
	Show		locationId,
	Show		teacherId
 ) => ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> MeetingResourcesByTime timeslotId locationId teacherId
findMeetingResourcesByTime problemParameters	= Data.Map.map (
	Data.Set.foldr (
		\meeting (l, s, t) -> (
			Data.Maybe.maybe l (`Data.Set.insert` l) $ Model.Meeting.getMaybeLocationId meeting,
			s `Data.Set.union` Model.Meeting.getStudentClass meeting,
			t `Data.Set.union` Model.Meeting.getTeacherIds meeting
		) -- Triple
	) (
		Data.Set.empty,
		Data.Set.empty,
		Data.Set.empty
{-
	) . Data.Set.filter (
		Data.Group.getMandatesAttendance . (ProblemConfiguration.ProblemParameters.getGroupCatalogue problemParameters !) . Model.Meeting.getGroupId
-}
	)
 ) $ findMeetingsByTime problemParameters

-- | True if any of the required /resource/s, are already required at the specified /time/, for a /meeting/ of a /group/.
areAnyResourcesBookedForGroupMeeting :: (
	Ord	locationId,
	Ord	teacherId,
	Ord	timeslotId
 )
	=> ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Temporal.Time.Time timeslotId
	-> Aggregate.StudentBody.StudentBody
	-> locationId
	-> teacherId
	-> Bool
areAnyResourcesBookedForGroupMeeting problemAnalysis time studentBody locationId teacherId	= Data.Maybe.maybe False (
	\(locationIdSet, studentBodySet, teacherIdSet)	-> or [
		Data.Set.member locationId locationIdSet,
		Data.Set.member studentBody studentBodySet,	-- CAVEAT: performance-hotspot.
		Data.Set.member teacherId teacherIdSet
	]
 ) . Data.Map.lookup time $ getMeetingResourcesByTime problemAnalysis

-- | Catalogues by /teacherId/, all the /course/s which match each of the /knowledge-requirement/s, of each /student-body/.
findSuitableCoursesByTeacherIdBySubjectByStudentBody
	:: Ord level
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> CoursesByTeacherIdBySubjectByStudentBody synchronisationId level teacherId timeslotId
findSuitableCoursesByTeacherIdBySubjectByStudentBody problemParameters	= Data.Map.mapWithKey (
	\studentBody studentProfile	-> Data.Map.fromList . map (
		\subject -> (
			subject,
			Aggregate.TeacherRegister.findSuitableCourseByTeacherId (Aggregate.StudentBody.getSize studentBody) subject . Data.Map.filter (
				Data.Resource.isAvailable . (,) studentProfile	-- Check that they're simultaneously available.
			) $ ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters
		) -- Pair.
	) . Data.Set.toList $ Data.Student.deriveAmalgamatedKnowledgeRequirement studentProfile
 ) $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters

{- |
	* Checks for the proposed /booking/, whether all suitable /course/s, for any one of the /student-body/'s other knowledge-requirements, specify the proposed /time/.

	* CAVEAT: doesn't check whether the specified /subject/ can only be satisfied by a /synchronised course/,
	& if so, whether any /student-body/ who requires a different /course/ from the same synchronised set,
	has any other /knowledge-requirement/ which can only be satisfied by a /course/ which specifies the proposed /time/.
	This tricky scenario is dealt with in the /lesson-criterion/ 'minimiseBookingAtAnotherCoursesSpecifiedTime' where the specific /course/ is known.
-}
doAllCoursesSatifyingAnotherKnowledgeRequirementSpecifyThisTime
	:: (Ord level, Ord timeslotId)
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Data.Subject.Subject level					-- ^ The /subject/ of the proposed /lesson/.
	-> StudentView.TimetableCoordinates.Coordinates timeslotId	-- ^ The /student-body/ & /time/, of the proposed /booking/.
	-> Bool
doAllCoursesSatifyingAnotherKnowledgeRequirementSpecifyThisTime problemParameters problemAnalysis subject (studentBody, time)	= Data.Set.member time (
	getSpecifiedTimes problemAnalysis
 ) {-required for efficiency-} && (
	Data.Foldable.any (
		Data.Foldable.all (
			Data.Course.isASpecifiedTime time
		) . (
			getSuitableCoursesByTeacherIdBySubjectByStudentBody problemAnalysis ! studentBody !
		) -- Check whether all possible courses matching this other subject-requirement, specify the same booking-time.
	) . Data.Set.filter (
		/= subject	-- Find the other subjects required by this student-body.
	) . Data.Student.deriveAmalgamatedKnowledgeRequirement $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters ! studentBody
 )

{- |
	* If the specified /teacher/ has a suitable personal /location/, then that is prefered.

	* Otherwise, returns a list of /locationId/s, of sufficient /capacity/, & offering the /facilities/ required by the /course/ corresponding to the specified /subject/.

	* CAVEAT: the list of /locationId/s returned isn't ordered by suitability.
-}
findSuitableLocations :: (
	Eq	level,
	Ord	locationId,
	Ord	teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Size.NStudents		-- ^ Required /capacity/.
	-> Data.Subject.Subject level	-- ^ The /subject/ to be taught.
	-> teacherId			-- ^ The /teacher/ who will be taking the /class/.
	-> [locationId]
findSuitableLocations ProblemConfiguration.ProblemParameters.MkProblemParameters {
	ProblemConfiguration.ProblemParameters.getLocationCatalogue	= locationCatalogue,
	ProblemConfiguration.ProblemParameters.getTeacherRegister	= teacherRegister
} requiredCapacity subject teacherId
	| Just ownLocationId	<- maybeOwnLocationId
	, Data.Location.isSuitable requiredCapacity requiredFacilityNames $ locationCatalogue ! ownLocationId	= [ownLocationId]	-- Confirm suitability of teacher's own location.
	| otherwise		= filter (
		`Data.Set.notMember` Aggregate.TeacherRegister.extractDistinctOwnLocationIds teacherRegister	-- Prevent squatting on another teacher's own location.
	) . Aggregate.LocationCatalogue.getLocationIds $ Aggregate.LocationCatalogue.findSuitableLocations requiredCapacity requiredFacilityNames locationCatalogue
	where
		(maybeOwnLocationId, requiredFacilityNames)	= Data.Teacher.getMaybeOwnLocationId . (teacherRegister !) &&& Data.Course.getRequiredFacilityNames . (Aggregate.TeacherRegister.findSuitableCourseByTeacherId requiredCapacity subject teacherRegister !) $ teacherId

{- |
	* Gets the combinations of possible /lesson/s, required by each /student-body/.

	* CAVEAT: though several /booking/s of a /lesson/ are typically required to satisfy the requirements of a single /course/,
	only one instance for each viable combination of locationId & teacherId, is returned.
-}
generateRequiredLessonCombinationsByStudentBody :: (
	Ord	level,
	Ord	locationId,
	Ord	teacherId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> LessonsByStudentBody locationId teacherId level
generateRequiredLessonCombinationsByStudentBody problemParameters	= Data.Map.mapWithKey (
	\studentBody studentProfile -> let
		requiredCapacity	= Aggregate.StudentBody.getSize studentBody
	in [
		Model.Lesson.MkLesson {
			Model.Lesson.getResourceIds	= StudentView.LessonResourceIds.MkLessonResourceIds locationId teacherId,
			Model.Lesson.getSubject		= subject
		} |
			subject				<- Data.Set.toList $ Data.Student.deriveAmalgamatedKnowledgeRequirement studentProfile,
			(teacherId, teacherProfile)	<- Data.Map.toList . Data.Map.filter (Data.Teacher.offersSuitableCourse requiredCapacity subject) $ ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters,
			locationId			<- findSuitableLocations problemParameters requiredCapacity subject teacherId,
			Data.Resource.isAvailable (
				studentProfile,
				teacherProfile,
				ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters ! locationId
			) -- Confirm mutual availability at some unspecified time.
	] -- A list-comprehension, which composes lessons from combinations of subjects, teacherIds & locationIds.
 ) $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters

{- |
	* Calls 'generateRequiredLessonCombinationsByStudentBody' to get the combinations of possible /lesson/s, required by each /student-body/.

	* The list of /lesson/s for each /student-body/, is replicated & filtered for compatibility with each specific /day/.

	* /Day/s on which there're zero available /student-bodies/, & /student-bodies/ for which there're zero /lesson/s, are included to facilitate lookup.
-}
generateRequiredLessonCombinationsByStudentBodyAvailableByDay :: (
	Ord	level,
	Ord	locationId,
	Ord	synchronisationId,
	Ord	teacherId,
	Ord	timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> LessonsByStudentBodyByDay locationId teacherId level
generateRequiredLessonCombinationsByStudentBodyAvailableByDay problemParameters problemAnalysis	= Data.Map.fromList $ map (
	\day -> (
		day,
		Data.Map.mapWithKey (
			\studentBody -> filter (
				uncurry (&&) . (
					areAvailableResources problemParameters day studentBody &&& Data.Maybe.maybe False {-no course found for lesson-} (
						uncurry (&&) . (
							Data.Maybe.maybe True {-not a synchronised course-} (
								\synchronisationId -> Data.Foldable.all (
									uncurry (||) . (
										not . Data.Student.requiresAnySubjectBy (
											`Data.Set.member` Data.Set.map Data.Course.getSubject (
												Aggregate.TeacherRegister.findDistinctCoursesBySynchronisationId (getCoursesByTeacherIdBySynchronisationId problemAnalysis) ! synchronisationId
											)
										) &&& Data.Resource.isAvailableOn day
									) -- Check that all students who require this synchronised course are available.
								) $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters
							) . Data.Course.getMaybeSynchronisationId &&& uncurry (||) . (
								not . Data.Course.hasRigidlySpecifiedDays &&& Data.Course.isASpecifiedDay day
							) -- Ensure that the course can be booked today.
						) {-course-}
					) . lookupCourseFor problemParameters
				) {-lesson-}
			) -- Filter the list of lessons for this student-body.
		) $ generateRequiredLessonCombinationsByStudentBody problemParameters
	) -- Pair.
 ) Temporal.Day.range

-- | True if the specified /student-body/, & the /location/ & /teacher/ referenced in the proposed /lesson/, are regularly available on the specified /day/.
areAvailableResources
	:: (Ord locationId, Ord teacherId)
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Temporal.Day.Day					-- ^ The /day/ on which we want to book a /lesson/.
	-> Aggregate.StudentBody.StudentBody
	-> StudentView.Lesson.Lesson locationId teacherId level	-- ^ The rendezvous we want to book.
	-> Bool
areAvailableResources problemParameters day studentBody studentLesson	= Data.Resource.isAvailableOn day (
	ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters ! StudentView.LessonResourceIds.getTeacherId resourceIds,
	ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters ! studentBody,
	ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters ! StudentView.LessonResourceIds.getLocationId resourceIds
 ) where
	resourceIds	= Model.Lesson.getResourceIds studentLesson

-- | True if the specified /human-resource/ is /available/ & not booked for a /meeting/ at the specified /time/.
isFree :: (
	Data.HumanResource.HumanResource	humanResource,
	Data.Resource.Resource			humanResource,
	Ord					timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Temporal.Time.Time timeslotId
	-> humanResource
	-> Bool
isFree problemParameters time	= uncurry (&&) . (
	Data.Resource.isAvailableOn (
		Temporal.Time.getDay time
	) &&& Data.Set.notMember time . Aggregate.GroupCatalogue.getMeetingTimes (
		ProblemConfiguration.ProblemParameters.getGroupCatalogue problemParameters
	) . Data.HumanResource.getGroupMembership
 )

-- | Get the number of /student/-places, when accounting for the capacity-limits imposed by both the /course/ & /location/; but not for any /student/s already booked.
countStudentPlaces
	:: Ord locationId
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Data.Course.Course synchronisationId level timeslotId	-- ^ The /course/ one wishes to query.
	-> locationId							-- ^ A specific /location/ in which the /course/ may be held.
	-> Size.NStudents
countStudentPlaces problemParameters course	= Data.Course.countStudentPlaces course . (ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters !)

-- | Lookup the /course/ corresponding to the specified /lesson/.
lookupCourseFor
	:: (Eq level, Ord teacherId)
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Lesson.Lesson locationId teacherId level
	-> Maybe (Data.Course.Course synchronisationId level timeslotId)	-- ^ The /course/ of which the /lesson/ may be a part.
lookupCourseFor problemParameters	= uncurry Data.Teacher.lookupCourseIn . (
	Model.Lesson.getSubject &&& (ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters !) . StudentView.LessonResourceIds.getTeacherId . Model.Lesson.getResourceIds
 )

{- |
	* Find the unique /course/ corresponding to the specified /studentViewLesson/.

	* CAVEAT: the specified /lesson/ is assumed to exist in the /timetable/, & therefore must have a corresponding /course/.
-}
findCourseFor
	:: (Eq level, Ord teacherId)
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> StudentView.Lesson.Lesson locationId teacherId level		-- ^ The /studentViewLesson/ extracted from a /studentViewTimetable/.
	-> Data.Course.Course synchronisationId level timeslotId	-- ^ The /course/ of which the /lesson/ was a part.
findCourseFor problemParameters	= Data.Maybe.fromJust . lookupCourseFor problemParameters

-- | Find the unique /course/ corresponding to the specified /locationViewLesson/.
findCourseForLocationViewLesson
	:: (Eq level, Ord teacherId)
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> locationId
	-> LocationView.Lesson.Lesson teacherId level
	-> Data.Course.Course synchronisationId level timeslotId
findCourseForLocationViewLesson problemParameters locationId	= findCourseFor	problemParameters . LocationView.Lesson.toStudentView locationId

-- | Find the unique /course/ corresponding to the specified /teacherViewLesson/.
findCourseForTeacherViewLesson
	:: (Eq level, Ord teacherId)
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> teacherId
	-> TeacherView.Lesson.Lesson locationId level
	-> Data.Course.Course synchronisationId level timeslotId
findCourseForTeacherViewLesson problemParameters teacherId	= findCourseFor	problemParameters . TeacherView.Lesson.toStudentView teacherId

-- | Find the set of distinct run-lengths of consecutive specified /time/s, occuring within any single /day/, for each distinct /course/.
findDistinctRunlengthsOfSpecifiedTimesByCourse :: (
	Enum	timeslotId,
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 ) => ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> DistinctTimeslotRunlengthsByCourse synchronisationId level timeslotId
findDistinctRunlengthsOfSpecifiedTimesByCourse	= Data.Map.fromList . map (
	id &&& Temporal.TimeslotRequest.findDistinctRunlengthsOfSpecifiedTimes . Data.Course.getTimeslotRequest
 ) . Data.Set.toList . Aggregate.TeacherRegister.extractDistinctCourses . ProblemConfiguration.ProblemParameters.getTeacherRegister

{- |
	Some /course/s may specify /time/s separated within a single /day/,
	which must be discovered to avoid erroneously downgrading those /timetable/s correctly booking /lesson/s in these /course/s.
-}
findCoursesRequestingSeparatedLessonsWithinAnyDay :: (
	Enum	timeslotId,
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 ) => ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> Data.Set.Set (Data.Course.Course synchronisationId level timeslotId)
findCoursesRequestingSeparatedLessonsWithinAnyDay	= Data.Map.foldr (
	Data.Set.union . Data.Set.filter Data.Course.requestsSeparatedTimelotsWithinAnyDay . Data.Teacher.getService
 ) Data.Set.empty . ProblemConfiguration.ProblemParameters.getTeacherRegister

-- | Find the /subject/s offered by those /course/s which have an /ideal timeslot-request/.
findSubjectsWithAnIdealTimeslotRequest :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 ) => ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> Data.Set.Set (Data.Subject.Subject level) {-coincidentally similar to Data.Subject.Knowledge-}
findSubjectsWithAnIdealTimeslotRequest	= Data.Set.map Data.Course.getSubject . Data.Set.filter (
	Temporal.TimeslotRequest.isIdeally . Data.Course.getTimeslotRequest
 ) . Aggregate.TeacherRegister.extractDistinctCourses . ProblemConfiguration.ProblemParameters.getTeacherRegister

{- |
	* Returns a map by /synchronisationId/, of /student-bodies/ who require a /subject/ from one of the corresponding /course/s.

	* If zero /student-bodies/ require any of the /subject/s offered with a specific /synchronisationId/, then an empty set exists in the returned map, rather than no entry at all.
-}
findStudentBodiesBySynchronisationId
	:: Ord level
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentBodiesBySynchronisationId synchronisationId
findStudentBodiesBySynchronisationId problemParameters	= Data.Map.map (
	\synchronisedCoursesByTeacherId -> Data.Map.keysSet $ Data.Map.filter (
		Data.Foldable.any (
			`Data.Set.member` Data.Set.fromList (map Data.Course.getSubject $ Data.Map.elems synchronisedCoursesByTeacherId)
		) . Data.Student.deriveAmalgamatedKnowledgeRequirement
	) $ ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters
 ) . getCoursesByTeacherIdBySynchronisationId

-- | True if the specified /timeslotId/ falls within the range defined for a /day/.
isValidTimeslotId
	:: Eq timeslotId
	=> ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> timeslotId
	-> Bool
isValidTimeslotId problemAnalysis	= (`elem` getTimeslotIdRange problemAnalysis)

-- | Returns the list of distinct /synchronisationId/s, specified amongst all /course/s.
findDistinctSynchronisationIds :: ProblemAnalysis level locationId synchronisationId teacherId timeslotId -> [synchronisationId]
findDistinctSynchronisationIds	= Data.Map.keys . getCoursesByTeacherIdBySynchronisationId

