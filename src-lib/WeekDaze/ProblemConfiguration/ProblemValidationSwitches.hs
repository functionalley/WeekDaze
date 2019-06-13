{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Defines the toggle-switches used to enable/disable checks on the validity of /ProblemParameters/.
-}

module WeekDaze.ProblemConfiguration.ProblemValidationSwitches(
-- * Types
-- ** Data-types
	ProblemValidationSwitches(..),
-- * Constants
--	tag,
	checkAvailabilityOfAnyGroupMemberTag,
	checkCapacityOfLocationsForMeetingsTag,
	checkCoursesForSynchronousSpecifiedTimesTag,
	checkDuplicateMeetingLocationIdsTag,
	checkDuplicateOwnLocationIdsTag,
	checkForAlternativesToSynchronisedCoursesTag,
	checkForDuplicateStudentIdsTag,
	checkForIdleStudentsTag,
	checkForIdleTeachersTag,
	checkForInvalidMeetingTimesTag,
	checkForMultipleCoursesPerTeacherPerSynchronisationIdTag,
	checkForNonExistentFacilitiesTag,
	checkForNonExistentGroupIdsTag,
	checkForNonExistentMeetingLocationIdsTag,
	checkForNonExistentOwnLocationIdsTag,
	checkForOverloadedStudentsTag,
	checkForSingletonSynchronisedCoursesTag,
	checkForStudentsRequiringMultipleSynchronisedSubjectsTag,
	checkForStudentsWithUnrealisableFreePeriodPreferenceTag,
	checkForSynchronisedCoursesWithDifferentIdealTimeslotsTag,
	checkForSynchronisedCoursesWithDifferentLessonsPerWeekTag,
	checkForSynchronisedCoursesWithExcessLessonsPerWeekTag,
	checkForSynchronisedCoursesWithExcessSpecifiedTimesTag,
	checkForSynchronisedCoursesWithExcessTimeslotRequestsTag,
	checkForSynchronisedCoursesWithoutSuitableLocationsTag,
	checkForSynchronisedCoursesWithUnavailableSpecifiedDaysTag,
	checkForTeachersWithUnrealisableFreePeriodPreferenceTag,
	checkIfStudentBodiesExceedTeachersTag,
	checkIfStudentBodySizeExceedsCapacityOfAllLocationsTag,
	checkIfStudentBodySizeExceedsLocationCapacityTag,
	checkIndependenceOfStudentTimeslotsRequestsAndMeetingsTag,
	checkIndependenceOfTeacherTimeslotsRequestsAndMeetingsTag,
	checkLocationsAvailabilityToSupportCoursesTag,
	checkLocationsForSynchronousSpecifiedTimesTag,
	checkMeetingLocationsAvailabilityTag,
	checkMinimumConsecutiveLessonsTag,
	checkNullGroupIdTag,
	checkNullLocationCatalogueTag,
	checkNullStudentBodyRegisterTag,
	checkNullTeacherRegisterTag,
	checkOwnLocationsAvailabilityTag,
	checkRequiredLessonsPerWeekTag,
	checkSimultaneousAvailabilityOfGroupMembersTag,
	checkStudentsAvailabilityForMandatoryMeetingsTag,
	checkStudentsAvailabilityForSpecifiedTimesTag,
	checkStudentsForMultipleLevelsOfSameTopicTag,
	checkStudentsForSynchronousMeetingsTag,
	checkStudentsLowerWorkloadBoundTag,
	checkStudentsUpperWorkloadBoundTag,
	checkSubjectExistenceTag,
	checkSuitableLocationsForKnowledgeRequirementsTag,
	checkTeachersAvailabilityForMandatoryMeetingsTag,
	checkTeachersForSynchronousMeetingsTag,
	checkTeachersUpperWorkloadBoundTag,
	checkTeachingCapacityBySubjectTag,
	checkTimeslotIdBoundsTag,
	checkTimeslotRequestsTag
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT
import qualified	WeekDaze.Enhanced.EnhancedTuple			as Enhanced.EnhancedTuple
import qualified	WeekDaze.ProblemConfiguration.ValidationSwitch	as ProblemConfiguration.ValidationSwitch
import			WeekDaze.Enhanced.EnhancedBool()

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Maybe
import qualified	WeekDaze.Database.Selector			as Database.Selector

-- Some checks are irrelevant given the known integrity of the database.
instance Database.Selector.Selector ProblemValidationSwitches where
	fromDatabase connection	projectIdSql	= let
		tableName :: Database.Selector.TableName
		tableName	= Database.Selector.tablePrefix ++ tag
	 in do
		switchesRows	<- map (
			map $ Data.Maybe.fromMaybe ProblemConfiguration.ValidationSwitch.defaultCheck . either (
				error . showString "WeekDaze.ProblemConfiguration.ProblemValidationSwitches.fromDatabase:\tfailed to parse the value for a switch read from the database; " . show
			) id . Database.HDBC.safeFromSql
		 ) `fmap` Database.Selector.select connection [
			checkAvailabilityOfAnyGroupMemberTag,
			checkCapacityOfLocationsForMeetingsTag,
			checkCoursesForSynchronousSpecifiedTimesTag,
			checkDuplicateMeetingLocationIdsTag,
--			checkDuplicateOwnLocationIdsTag,
			checkForAlternativesToSynchronisedCoursesTag,
--			checkForDuplicateStudentIdsTag,
			checkForIdleStudentsTag,
			checkForIdleTeachersTag,
--			checkForInvalidMeetingTimesTag,
			checkForMultipleCoursesPerTeacherPerSynchronisationIdTag,
			checkForNonExistentFacilitiesTag,
--			checkForNonExistentGroupIdsTag,
--			checkForNonExistentMeetingLocationIdsTag,
--			checkForNonExistentOwnLocationIdsTag,
			checkForOverloadedStudentsTag,
			checkForSingletonSynchronisedCoursesTag,
			checkForStudentsRequiringMultipleSynchronisedSubjectsTag,
			checkForStudentsWithUnrealisableFreePeriodPreferenceTag,
			checkForSynchronisedCoursesWithDifferentIdealTimeslotsTag,
			checkForSynchronisedCoursesWithDifferentLessonsPerWeekTag,
			checkForSynchronisedCoursesWithExcessLessonsPerWeekTag,
			checkForSynchronisedCoursesWithExcessSpecifiedTimesTag,
			checkForSynchronisedCoursesWithExcessTimeslotRequestsTag,
			checkForSynchronisedCoursesWithoutSuitableLocationsTag,
			checkForSynchronisedCoursesWithUnavailableSpecifiedDaysTag,
			checkForTeachersWithUnrealisableFreePeriodPreferenceTag,
			checkIfStudentBodiesExceedTeachersTag,
			checkIfStudentBodySizeExceedsCapacityOfAllLocationsTag,
			checkIfStudentBodySizeExceedsLocationCapacityTag,
			checkIndependenceOfStudentTimeslotsRequestsAndMeetingsTag,
			checkIndependenceOfTeacherTimeslotsRequestsAndMeetingsTag,
			checkLocationsAvailabilityToSupportCoursesTag,
			checkLocationsForSynchronousSpecifiedTimesTag,
			checkMeetingLocationsAvailabilityTag,
--			checkMinimumConsecutiveLessonsTag,
--			checkNullGroupIdTag,
			checkNullLocationCatalogueTag,
			checkNullStudentBodyRegisterTag,
			checkNullTeacherRegisterTag,
			checkOwnLocationsAvailabilityTag,
			checkRequiredLessonsPerWeekTag,
			checkSimultaneousAvailabilityOfGroupMembersTag,
			checkStudentsAvailabilityForMandatoryMeetingsTag,
			checkStudentsAvailabilityForSpecifiedTimesTag,
--			checkStudentsForMultipleLevelsOfSameTopicTag,
			checkStudentsForSynchronousMeetingsTag,
			checkStudentsLowerWorkloadBoundTag,
			checkStudentsUpperWorkloadBoundTag,
--			checkSubjectExistenceTag,
			checkSuitableLocationsForKnowledgeRequirementsTag,
			checkTeachersAvailabilityForMandatoryMeetingsTag,
			checkTeachersForSynchronousMeetingsTag,
			checkTeachersUpperWorkloadBoundTag,
			checkTeachingCapacityBySubjectTag
--			checkTimeslotIdBoundsTag,
--			checkTimeslotRequestsTag
		 ] [tableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		return {-to IO-Monad-} $ case switchesRows of
			[]		-> Data.Default.def
			[switchesRow]	-> case switchesRow of
				[
					checkAvailabilityOfAnyGroupMember,
					checkCapacityOfLocationsForMeetings,
					checkCoursesForSynchronousSpecifiedTimes,
					checkDuplicateMeetingLocationIds,
--					checkDuplicateOwnLocationIds,
					checkForAlternativesToSynchronisedCourses,
--					checkForDuplicateStudentIds,
					checkForIdleStudents,
					checkForIdleTeachers,
--					checkForInvalidMeetingTimes,
					checkForMultipleCoursesPerTeacherPerSynchronisationId,
					checkForNonExistentFacilities,
--					checkForNonExistentGroupIds,
--					checkForNonExistentMeetingLocationIds,
--					checkForNonExistentOwnLocationIds,
					checkForOverloadedStudents,
					checkForSingletonSynchronisedCourses,
					checkForStudentsRequiringMultipleSynchronisedSubjects,
					checkForStudentsWithUnrealisableFreePeriodPreference,
					checkForSynchronisedCoursesWithDifferentIdealTimeslots,
					checkForSynchronisedCoursesWithDifferentLessonsPerWeek,
					checkForSynchronisedCoursesWithExcessLessonsPerWeek,
					checkForSynchronisedCoursesWithExcessSpecifiedTimes,
					checkForSynchronisedCoursesWithExcessTimeslotRequests,
					checkForSynchronisedCoursesWithoutSuitableLocations,
					checkForSynchronisedCoursesWithUnavailableSpecifiedDays,
					checkForTeachersWithUnrealisableFreePeriodPreference,
					checkIfStudentBodiesExceedTeachers,
					checkIfStudentBodySizeExceedsCapacityOfAllLocations,
					checkIfStudentBodySizeExceedsLocationCapacity,
					checkIndependenceOfStudentTimeslotsRequestsAndMeetings,
					checkIndependenceOfTeacherTimeslotsRequestsAndMeetings,
					checkLocationsAvailabilityToSupportCourses,
					checkLocationsForSynchronousSpecifiedTimes,
					checkMeetingLocationsAvailability,
--					checkMinimumConsecutiveLessons,
--					checkNullGroupId,
					checkNullLocationCatalogue,
					checkNullStudentBodyRegister,
					checkNullTeacherRegister,
					checkOwnLocationsAvailability,
					checkRequiredLessonsPerWeek,
					checkSimultaneousAvailabilityOfGroupMembers,
					checkStudentsAvailabilityForMandatoryMeetings,
					checkStudentsAvailabilityForSpecifiedTimes,
--					checkStudentsForMultipleLevelsOfSameTopic,
					checkStudentsForSynchronousMeetings,
					checkStudentsLowerWorkloadBound,
					checkStudentsUpperWorkloadBound,
--					checkSubjectExistence,
					checkSuitableLocationsForKnowledgeRequirements,
					checkTeachersAvailabilityForMandatoryMeetings,
					checkTeachersForSynchronousMeetings,
					checkTeachersUpperWorkloadBound,
					checkTeachingCapacityBySubject
--					checkTimeslotIdBounds,
--					checkTimeslotRequests
				 ] -> Data.Default.def {
					getCheckAvailabilityOfAnyGroupMember				= checkAvailabilityOfAnyGroupMember,
					getCheckCapacityOfLocationsForMeetings				= checkCapacityOfLocationsForMeetings,
					getCheckCoursesForSynchronousSpecifiedTimes			= checkCoursesForSynchronousSpecifiedTimes,
					getCheckDuplicateMeetingLocationIds				= checkDuplicateMeetingLocationIds,
--					getCheckDuplicateOwnLocationIds					= checkDuplicateOwnLocationIds,
					getCheckForAlternativesToSynchronisedCourses			= checkForAlternativesToSynchronisedCourses,
--					getCheckForDuplicateStudentIds					= checkForDuplicateStudentIds,
					getCheckForIdleStudents						= checkForIdleStudents,
					getCheckForIdleTeachers						= checkForIdleTeachers,
--					getCheckForInvalidMeetingTimes					= checkForInvalidMeetingTimes,
					getCheckForMultipleCoursesPerTeacherPerSynchronisationId	= checkForMultipleCoursesPerTeacherPerSynchronisationId,
					getCheckForNonExistentFacilities				= checkForNonExistentFacilities,
--					getCheckForNonExistentGroupIds					= checkForNonExistentGroupIds,
--					getCheckForNonExistentMeetingLocationIds			= checkForNonExistentMeetingLocationIds,
--					getCheckForNonExistentOwnLocationIds				= checkForNonExistentOwnLocationIds,
					getCheckForOverloadedStudents					= checkForOverloadedStudents,
					getCheckForSingletonSynchronisedCourses				= checkForSingletonSynchronisedCourses,
					getCheckForStudentsRequiringMultipleSynchronisedSubjects	= checkForStudentsRequiringMultipleSynchronisedSubjects,
					getCheckForStudentsWithUnrealisableFreePeriodPreference		= checkForStudentsWithUnrealisableFreePeriodPreference,
					getCheckForSynchronisedCoursesWithDifferentIdealTimeslots	= checkForSynchronisedCoursesWithDifferentIdealTimeslots,
					getCheckForSynchronisedCoursesWithDifferentLessonsPerWeek	= checkForSynchronisedCoursesWithDifferentLessonsPerWeek,
					getCheckForSynchronisedCoursesWithExcessLessonsPerWeek		= checkForSynchronisedCoursesWithExcessLessonsPerWeek,
					getCheckForSynchronisedCoursesWithExcessSpecifiedTimes		= checkForSynchronisedCoursesWithExcessSpecifiedTimes,
					getCheckForSynchronisedCoursesWithExcessTimeslotRequests	= checkForSynchronisedCoursesWithExcessTimeslotRequests,
					getCheckForSynchronisedCoursesWithoutSuitableLocations		= checkForSynchronisedCoursesWithoutSuitableLocations,
					getCheckForSynchronisedCoursesWithUnavailableSpecifiedDays	= checkForSynchronisedCoursesWithUnavailableSpecifiedDays,
					getCheckForTeachersWithUnrealisableFreePeriodPreference		= checkForTeachersWithUnrealisableFreePeriodPreference,
					getCheckIfStudentBodiesExceedTeachers				= checkIfStudentBodiesExceedTeachers,
					getCheckIfStudentBodySizeExceedsCapacityOfAllLocations		= checkIfStudentBodySizeExceedsCapacityOfAllLocations,
					getCheckIfStudentBodySizeExceedsLocationCapacity		= checkIfStudentBodySizeExceedsLocationCapacity,
					getCheckIndependenceOfStudentTimeslotsRequestsAndMeetings	= checkIndependenceOfStudentTimeslotsRequestsAndMeetings,
					getCheckIndependenceOfTeacherTimeslotsRequestsAndMeetings	= checkIndependenceOfTeacherTimeslotsRequestsAndMeetings,
					getCheckLocationsAvailabilityToSupportCourses			= checkLocationsAvailabilityToSupportCourses,
					getCheckLocationsForSynchronousSpecifiedTimes			= checkLocationsForSynchronousSpecifiedTimes,
					getCheckMeetingLocationsAvailability				= checkMeetingLocationsAvailability,
--					getCheckMinimumConsecutiveLessons				= checkMinimumConsecutiveLessons,
--					getCheckNullGroupId						= checkNullGroupId,
					getCheckNullLocationCatalogue					= checkNullLocationCatalogue,
					getCheckNullStudentBodyRegister					= checkNullStudentBodyRegister,
					getCheckNullTeacherRegister					= checkNullTeacherRegister,
					getCheckOwnLocationsAvailability				= checkOwnLocationsAvailability,
					getCheckRequiredLessonsPerWeek					= checkRequiredLessonsPerWeek,
					getCheckSimultaneousAvailabilityOfGroupMembers			= checkSimultaneousAvailabilityOfGroupMembers,
					getCheckStudentsAvailabilityForMandatoryMeetings		= checkStudentsAvailabilityForMandatoryMeetings,
					getCheckStudentsAvailabilityForSpecifiedTimes			= checkStudentsAvailabilityForSpecifiedTimes,
--					getCheckStudentsForMultipleLevelsOfSameTopic			= checkStudentsForMultipleLevelsOfSameTopic,
					getCheckStudentsForSynchronousMeetings				= checkStudentsForSynchronousMeetings,
					getCheckStudentsLowerWorkloadBound				= checkStudentsLowerWorkloadBound,
					getCheckStudentsUpperWorkloadBound				= checkStudentsUpperWorkloadBound,
--					getCheckSubjectExistence					= checkSubjectExistence,
					getCheckSuitableLocationsForKnowledgeRequirements		= checkSuitableLocationsForKnowledgeRequirements,
					getCheckTeachersAvailabilityForMandatoryMeetings		= checkTeachersAvailabilityForMandatoryMeetings,
					getCheckTeachersForSynchronousMeetings				= checkTeachersForSynchronousMeetings,
					getCheckTeachersUpperWorkloadBound				= checkTeachersUpperWorkloadBound,
					getCheckTeachingCapacityBySubject				= checkTeachingCapacityBySubject
--					getCheckTimeslotIdBounds					= checkTimeslotIdBounds,
--					getCheckTimeslotRequests					= checkTimeslotRequests
				}
				_ -> error $ "WeekDaze.ProblemConfiguration.ProblemValidationSwitches.fromDatabase:\tunexpected number of columns=" ++ show (length switchesRow) ++ " in row of table " ++ show tableName ++ "."
			_		-> error $ "WeekDaze.ProblemConfiguration.ProblemValidationSwitches.fromDatabase:\tunexpected number of rows=" ++ show (length switchesRows) ++ " selected from table " ++ show tableName ++ "."
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag								= "problemValidationSwitches"

-- | Used to qualify SQL & XML.
checkAvailabilityOfAnyGroupMemberTag :: String
checkAvailabilityOfAnyGroupMemberTag				= "checkAvailabilityOfAnyGroupMember"

-- | Used to qualify SQL & XML.
checkCapacityOfLocationsForMeetingsTag :: String
checkCapacityOfLocationsForMeetingsTag				= "checkCapacityOfLocationsForMeetings"

-- | Used to qualify SQL & XML.
checkCoursesForSynchronousSpecifiedTimesTag :: String
checkCoursesForSynchronousSpecifiedTimesTag			= "checkCoursesForSynchronousSpecifiedTimes"

-- | Used to qualify SQL & XML.
checkDuplicateMeetingLocationIdsTag :: String
checkDuplicateMeetingLocationIdsTag				= "checkDuplicateMeetingLocationIds"

-- | Used to qualify XML.
checkDuplicateOwnLocationIdsTag :: String
checkDuplicateOwnLocationIdsTag					= "checkDuplicateOwnLocationIds"

-- | Used to qualify SQL & XML.
checkForAlternativesToSynchronisedCoursesTag :: String
checkForAlternativesToSynchronisedCoursesTag			= "checkForAlternativesToSynchronisedCourses"

-- | Used to qualify XML.
checkForDuplicateStudentIdsTag :: String
checkForDuplicateStudentIdsTag					= "checkForDuplicateStudentIds"

-- | Used to qualify SQL & XML.
checkForIdleStudentsTag :: String
checkForIdleStudentsTag						= "checkForIdleStudents"

-- | Used to qualify SQL & XML.
checkForIdleTeachersTag :: String
checkForIdleTeachersTag						= "checkForIdleTeachers"

-- | Used to qualify XML.
checkForInvalidMeetingTimesTag :: String
checkForInvalidMeetingTimesTag					= "checkForInvalidMeetingTimes"

-- | Used to qualify SQL & XML.
checkForMultipleCoursesPerTeacherPerSynchronisationIdTag :: String
checkForMultipleCoursesPerTeacherPerSynchronisationIdTag	= "checkForMultipleCoursesPerTeacherPerSynchronisationId"

-- | Used to qualify SQL & XML.
checkForNonExistentFacilitiesTag :: String
checkForNonExistentFacilitiesTag				= "checkForNonExistentFacilities"

-- | Used to qualify XML.
checkForNonExistentGroupIdsTag :: String
checkForNonExistentGroupIdsTag					= "checkForNonExistentGroupIds"

-- | Used to qualify XML.
checkForNonExistentMeetingLocationIdsTag :: String
checkForNonExistentMeetingLocationIdsTag			= "checkForNonExistentMeetingLocationIds"

-- | Used to qualify XML.
checkForNonExistentOwnLocationIdsTag :: String
checkForNonExistentOwnLocationIdsTag				= "checkForNonExistentOwnLocationIds"

-- | Used to qualify SQL & XML.
checkForOverloadedStudentsTag :: String
checkForOverloadedStudentsTag					= "checkForOverloadedStudents"

-- | Used to qualify SQL & XML.
checkForSingletonSynchronisedCoursesTag :: String
checkForSingletonSynchronisedCoursesTag				= "checkForSingletonSynchronisedCourses"

-- | Used to qualify SQL & XML.
checkForStudentsRequiringMultipleSynchronisedSubjectsTag :: String
checkForStudentsRequiringMultipleSynchronisedSubjectsTag	= "checkForStudentsRequiringMultipleSynchronisedSubjects"

-- | Used to qualify SQL & XML.
checkForStudentsWithUnrealisableFreePeriodPreferenceTag :: String
checkForStudentsWithUnrealisableFreePeriodPreferenceTag		= "checkForStudentsWithUnrealisableFreePeriodPreference"

-- | Used to qualify SQL & XML.
checkForSynchronisedCoursesWithDifferentIdealTimeslotsTag :: String
checkForSynchronisedCoursesWithDifferentIdealTimeslotsTag	= "checkForSynchronisedCoursesWithDifferentIdealTimeslots"

-- | Used to qualify SQL & XML.
checkForSynchronisedCoursesWithDifferentLessonsPerWeekTag :: String
checkForSynchronisedCoursesWithDifferentLessonsPerWeekTag	= "checkForSynchronisedCoursesWithDifferentLessonsPerWeek"

-- | Used to qualify SQL & XML.
checkForSynchronisedCoursesWithExcessLessonsPerWeekTag :: String
checkForSynchronisedCoursesWithExcessLessonsPerWeekTag		= "checkForSynchronisedCoursesWithExcessLessonsPerWeek"

-- | Used to qualify SQL & XML.
checkForSynchronisedCoursesWithExcessSpecifiedTimesTag :: String
checkForSynchronisedCoursesWithExcessSpecifiedTimesTag		= "checkForSynchronisedCoursesWithExcessSpecifiedTimes"

-- | Used to qualify SQL & XML.
checkForSynchronisedCoursesWithExcessTimeslotRequestsTag :: String
checkForSynchronisedCoursesWithExcessTimeslotRequestsTag	= "checkForSynchronisedCoursesWithExcessTimeslotRequests"

-- | Used to qualify SQL & XML.
checkForSynchronisedCoursesWithoutSuitableLocationsTag :: String
checkForSynchronisedCoursesWithoutSuitableLocationsTag		= "checkForSynchronisedCoursesWithoutSuitableLocations"

-- | Used to qualify SQL & XML.
checkForSynchronisedCoursesWithUnavailableSpecifiedDaysTag :: String
checkForSynchronisedCoursesWithUnavailableSpecifiedDaysTag	= "checkForSynchronisedCoursesWithUnavailableSpecifiedDays"

-- | Used to qualify SQL & XML.
checkForTeachersWithUnrealisableFreePeriodPreferenceTag :: String
checkForTeachersWithUnrealisableFreePeriodPreferenceTag		= "checkForTeachersWithUnrealisableFreePeriodPreference"

-- | Used to qualify SQL & XML.
checkIfStudentBodiesExceedTeachersTag :: String
checkIfStudentBodiesExceedTeachersTag				= "checkIfStudentBodiesExceedTeachers"

-- | Used to qualify SQL & XML.
checkIfStudentBodySizeExceedsCapacityOfAllLocationsTag :: String
checkIfStudentBodySizeExceedsCapacityOfAllLocationsTag		= "checkIfStudentBodySizeExceedsCapacityOfAllLocations"

-- | Used to qualify SQL & XML.
checkIfStudentBodySizeExceedsLocationCapacityTag :: String
checkIfStudentBodySizeExceedsLocationCapacityTag		= "checkIfStudentBodySizeExceedsLocationCapacity"

-- | Used to qualify SQL & XML.
checkIndependenceOfStudentTimeslotsRequestsAndMeetingsTag :: String
checkIndependenceOfStudentTimeslotsRequestsAndMeetingsTag	= "checkIndependenceOfStudentTimeslotsRequestsAndMeetings"

-- | Used to qualify SQL & XML.
checkIndependenceOfTeacherTimeslotsRequestsAndMeetingsTag :: String
checkIndependenceOfTeacherTimeslotsRequestsAndMeetingsTag	= "checkIndependenceOfTeacherTimeslotsRequestsAndMeetings"

-- | Used to qualify SQL & XML.
checkLocationsAvailabilityToSupportCoursesTag :: String
checkLocationsAvailabilityToSupportCoursesTag			= "checkLocationsAvailabilityToSupportCourses"

-- | Used to qualify SQL & XML.
checkLocationsForSynchronousSpecifiedTimesTag :: String
checkLocationsForSynchronousSpecifiedTimesTag			= "checkLocationsForSynchronousSpecifiedTimes"

-- | Used to qualify SQL & XML.
checkMeetingLocationsAvailabilityTag :: String
checkMeetingLocationsAvailabilityTag				= "checkMeetingLocationsAvailability"

-- | Used to qualify XML.
checkMinimumConsecutiveLessonsTag :: String
checkMinimumConsecutiveLessonsTag				= "checkMinimumConsecutiveLessons"

-- | Used to qualify XML.
checkNullGroupIdTag :: String
checkNullGroupIdTag						= "checkNullGroupId"

-- | Used to qualify SQL & XML.
checkNullLocationCatalogueTag :: String
checkNullLocationCatalogueTag					= "checkNullLocationCatalogue"

-- | Used to qualify SQL & XML.
checkNullStudentBodyRegisterTag :: String
checkNullStudentBodyRegisterTag					= "checkNullStudentBodyRegister"

-- | Used to qualify SQL & XML.
checkNullTeacherRegisterTag :: String
checkNullTeacherRegisterTag					= "checkNullTeacherRegister"

-- | Used to qualify SQL & XML.
checkOwnLocationsAvailabilityTag :: String
checkOwnLocationsAvailabilityTag				= "checkOwnLocationsAvailability"

-- | Used to qualify SQL & XML.
checkRequiredLessonsPerWeekTag :: String
checkRequiredLessonsPerWeekTag					= "checkRequiredLessonsPerWeek"

-- | Used to qualify SQL & XML.
checkSimultaneousAvailabilityOfGroupMembersTag :: String
checkSimultaneousAvailabilityOfGroupMembersTag			= "checkSimultaneousAvailabilityOfGroupMembers"

-- | Used to qualify SQL & XML.
checkStudentsAvailabilityForMandatoryMeetingsTag :: String
checkStudentsAvailabilityForMandatoryMeetingsTag		= "checkStudentsAvailabilityForMandatoryMeetings"

-- | Used to qualify SQL & XML.
checkStudentsAvailabilityForSpecifiedTimesTag :: String
checkStudentsAvailabilityForSpecifiedTimesTag			= "checkStudentsAvailabilityForSpecifiedTimes"

-- | Used to qualify XML.
checkStudentsForMultipleLevelsOfSameTopicTag :: String
checkStudentsForMultipleLevelsOfSameTopicTag			= "checkStudentsForMultipleLevelsOfSameTopic"

-- | Used to qualify SQL & XML.
checkStudentsForSynchronousMeetingsTag :: String
checkStudentsForSynchronousMeetingsTag				= "checkStudentsForSynchronousMeetings"

-- | Used to qualify SQL & XML.
checkStudentsLowerWorkloadBoundTag :: String
checkStudentsLowerWorkloadBoundTag				= "checkStudentsLowerWorkloadBound"

-- | Used to qualify SQL & XML.
checkStudentsUpperWorkloadBoundTag :: String
checkStudentsUpperWorkloadBoundTag				= "checkStudentsUpperWorkloadBound"

-- | Used to qualify XML.
checkSubjectExistenceTag :: String
checkSubjectExistenceTag					= "checkSubjectExistence"

-- | Used to qualify XML.
checkSuitableLocationsForKnowledgeRequirementsTag :: String
checkSuitableLocationsForKnowledgeRequirementsTag		= "checkSuitableLocationsForKnowledgeRequirements"

-- | Used to qualify SQL & XML.
checkTeachersAvailabilityForMandatoryMeetingsTag :: String
checkTeachersAvailabilityForMandatoryMeetingsTag		= "checkTeachersAvailabilityForMandatoryMeetings"

-- | Used to qualify SQL & XML.
checkTeachersForSynchronousMeetingsTag :: String
checkTeachersForSynchronousMeetingsTag				= "checkTeachersForSynchronousMeetings"

-- | Used to qualify SQL & XML.
checkTeachersUpperWorkloadBoundTag :: String
checkTeachersUpperWorkloadBoundTag				= "checkTeachersUpperWorkloadBound"

-- | Used to qualify SQL & XML.
checkTeachingCapacityBySubjectTag :: String
checkTeachingCapacityBySubjectTag				= "checkTeachingCapacityBySubject"

-- | Used to qualify XML.
checkTimeslotIdBoundsTag :: String
checkTimeslotIdBoundsTag					= "checkTimeslotIdBounds"

-- | Used to qualify XML.
checkTimeslotRequestsTag :: String
checkTimeslotRequestsTag					= "checkTimeslotRequests"

-- | Encapsulates the data which defines the problem.
data ProblemValidationSwitches	= MkProblemValidationSwitches {
	getCheckAvailabilityOfAnyGroupMember				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that at least one member, can attend each meeting of a group.
	getCheckCapacityOfLocationsForMeetings				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that any /location/, specified for the /meeting/s of each /group/, has adequate capacity.
	getCheckCoursesForSynchronousSpecifiedTimes			:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that no /course/s, required to satisfy a /student/'s /knowledge-requirements/, specify the same /booking-time/.
	getCheckDuplicateMeetingLocationIds				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that when more than one /group/ requires a /location/, that their /meeting/-times are mutually exclusive.
	getCheckDuplicateOwnLocationIds					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that when more than one /teacher/ claims a single /location/ as their own, their working-weeks are mutually exclusive.
	getCheckForAlternativesToSynchronisedCourses			:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Ensure that /student/s can migrate easily between /synchronised course/s, by prohibiting any /teacher/ from offering an alternative /course/ outside the set of /synchronised courses/, but with an identical /subject/ to one of the member-/course/s.
	getCheckForDuplicateStudentIds					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that no /student/-identifier belongs to more than one /student-body/.
	getCheckForIdleStudents						:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /student/s who require zero /subject/s, but have allocated one or more /time-slot/s for teaching.
	getCheckForIdleTeachers						:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /teacher/s who offer zero /course/s required by /student/s, but have allocated one or more /time-slot/s for teaching.
	getCheckForInvalidMeetingTimes					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that all the /timeslot-Id/s, used to define the meeting-times of groups, are within permissible bounds.
	getCheckForMultipleCoursesPerTeacherPerSynchronisationId	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that no /teacher/ offers more than one /course/ with the same /synchronisationId/.
	getCheckForNonExistentFacilities				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether all /facilities/, referenced by those /course/s offered by /teacher/s, exist in at least one /location/.
	getCheckForNonExistentGroupIds					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether all /group-id/s, referenced by /human-resource/s, exist independently.
	getCheckForNonExistentMeetingLocationIds			:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether all /location-id/s, required for the meetings of /group/s, exist.
	getCheckForNonExistentOwnLocationIds				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether all /location-id/s, referenced by /teacher/s, exist independently.
	getCheckForOverloadedStudents					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /student/s who have allocated zero /time-slot/s for teaching, but have requested one or more /subject/s.
	getCheckForSingletonSynchronisedCourses				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /teacher/s whose /course/s are members of singleton sets of synchronised courses.
	getCheckForStudentsRequiringMultipleSynchronisedSubjects	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for any /student-body/ requiring multiple /subject/s only offered within one set of /synchronised course/s.
	getCheckForStudentsWithUnrealisableFreePeriodPreference	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for any /student-body/ whose preference for the position of free /time-slot/s, within those days when they're /available/, can never be realised because of the meeting-times of /group/s of which they're members.
	getCheckForSynchronisedCoursesWithDifferentIdealTimeslots	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /synchronised course/s don't define /timeslotRequest/s with different /ideal timeslot-id/s.
	getCheckForSynchronisedCoursesWithDifferentLessonsPerWeek	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /synchronised course/s which require different numbers of /lesson/s per week.
	getCheckForSynchronisedCoursesWithExcessLessonsPerWeek		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /synchronised course/s which require more /lesson/s per week, than the number of /time-slot/s for which all the interested /student-bodies/ & required /teacher/s, are simultaneously /available/.
	getCheckForSynchronisedCoursesWithExcessSpecifiedTimes		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /synchronised course/s which have fewer /lesson/ per week than specified /booking-time/s.
	getCheckForSynchronisedCoursesWithExcessTimeslotRequests	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /synchronised course/s which define both /ideal timeslotIds/ & specified /booking-time/s.
	getCheckForSynchronisedCoursesWithoutSuitableLocations		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /synchronised course/s which lack sufficient /locations/ offering the required /facilities/, which are simultaneously /available/ to the /teacher/s & all interested /student/s.
	getCheckForSynchronisedCoursesWithUnavailableSpecifiedDays	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /synchronised course/s which specify /time/s, on /day/s when not all the interested student-bodies & required /teacher/s, are /available/.
	getCheckForTeachersWithUnrealisableFreePeriodPreference	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for any /teacher/ whose preference for the position of free /time-slot/s, within those days when they're /available/, can never be realised because of the /meeting/-times of /group/s of which they're members.
	getCheckIfStudentBodiesExceedTeachers				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether there are sufficient /teacher/s for the number of /student-bodies/, on each day.
	getCheckIfStudentBodySizeExceedsCapacityOfAllLocations		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether each /student-body/ can be accommodated within the largest /available/ /location/.
	getCheckIfStudentBodySizeExceedsLocationCapacity		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether all /student-bodies/ can be accommodated within the /available/ /location/s, on each day.
	getCheckIndependenceOfStudentTimeslotsRequestsAndMeetings	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /student-bodies/ are members of /group/s which specify meeting-times (even those which don't mandate attendance), which match any of the specified booking-times of /course/s they must attend.
	getCheckIndependenceOfTeacherTimeslotsRequestsAndMeetings	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /teacher/s are members of /group/s which specify meeting-times (even those which don't mandate attendance), which match any of the specified booking-times of /course/s they must attend.
	getCheckLocationsAvailabilityToSupportCourses			:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that there are sufficient /location/s, simultaneously available with at least one /student-body/ & at least one /teacher/, to support the total number of /time-slot/s required for the /course/s offered by all /teacher/s.
	getCheckLocationsForSynchronousSpecifiedTimes			:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check there are enough /locations/, in which to book /lesson/s, at those /time/s specified by different /teachers/.
	getCheckMeetingLocationsAvailability				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that any /location/ required for a /group/, is /available/ at all /meeting/-times.
	getCheckMinimumConsecutiveLessons				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that a /course/'s /minimumConsecutiveLessons/ doesn't exceed the number of /time-slot/s per /day/.
	getCheckNullGroupId						:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for null identifiers in the /group-catalogue/.
	getCheckNullLocationCatalogue					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether the /location-catalogue/ is empty.
	getCheckNullStudentBodyRegister					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether the /student-body register/ is empty.
	getCheckNullTeacherRegister					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check whether the /teacher register/ is empty.
	getCheckOwnLocationsAvailability				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that if /teacher/s specify their own /location/, that there's some common /availability/.
	getCheckRequiredLessonsPerWeek					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that no /course/ is offered, which requires more /lesson/s per week, than the /teacher/ works.
	getCheckSimultaneousAvailabilityOfGroupMembers			:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that all the members of each /group/, are simultaneously /available/.
	getCheckStudentsAvailabilityForMandatoryMeetings		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /student-bodies/ are /available/ for the /meeting/-times of the /group/s of which they're members.
	getCheckStudentsAvailabilityForSpecifiedTimes			:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that no /course/, required to satisfy a /student/'s /knowledge-requirements/, specifies any /time/, on a /day/ when that /student/'s un/available/.
	getCheckStudentsForMultipleLevelsOfSameTopic			:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that the /knowledge-requirements/ of each /student-body/, don't specify more than one /level/ in any /topic/. This might occur legitimately where studying one /subject/ for two different exams e.g. Scottish Higher & A-level.
	getCheckStudentsForSynchronousMeetings				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /student-bodies/ aren't members of /group/s with synchronous meeting-times.
	getCheckStudentsLowerWorkloadBound				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /student-bodies/ book sufficient /subject/s.
	getCheckStudentsUpperWorkloadBound				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /student-bodies/ don't book excess /subject/s.
	getCheckSubjectExistence					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for the existence of the requested /subject/s.
	getCheckSuitableLocationsForKnowledgeRequirements		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that there are suitable /location/s for each /knowledge-requirement/ of each /student-body/.
	getCheckTeachersAvailabilityForMandatoryMeetings		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /teacher/s are /available/ for the /meeting/-times of the /group/s of which they're members.
	getCheckTeachersForSynchronousMeetings				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /teacher/s aren't members of /group/s with synchronous /meeting/-times.
	getCheckTeachersUpperWorkloadBound				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /teacher/s aren't members of too many /group/s.
	getCheckTeachingCapacityBySubject				:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that there's sufficient teaching-capacity, to satisfy the demand by /student/s for each /subject/.
	getCheckTimeslotIdBounds					:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check the range of /timeslot-id/s defined, is sufficient.
	getCheckTimeslotRequests					:: ProblemConfiguration.ValidationSwitch.Check	-- ^ Check that all /timeslot-request/s reference a valid /timeslot-id/.
} deriving (Eq, Show)

instance HXT.XmlPickler ProblemValidationSwitches where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(
			a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56
		) -> MkProblemValidationSwitches a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56,	-- Construct from a tuple.
		\MkProblemValidationSwitches {
			getCheckAvailabilityOfAnyGroupMember				= checkAvailabilityOfAnyGroupMember,
			getCheckCapacityOfLocationsForMeetings				= checkCapacityOfLocationsForMeetings,
			getCheckCoursesForSynchronousSpecifiedTimes			= checkCoursesForSynchronousSpecifiedTimes,
			getCheckDuplicateMeetingLocationIds				= checkDuplicateMeetingLocationIds,
			getCheckDuplicateOwnLocationIds					= checkDuplicateOwnLocationIds,
			getCheckForAlternativesToSynchronisedCourses			= checkForAlternativesToSynchronisedCourses,
			getCheckForDuplicateStudentIds					= checkForDuplicateStudentIds,
			getCheckForIdleStudents						= checkForIdleStudents,
			getCheckForIdleTeachers						= checkForIdleTeachers,
			getCheckForInvalidMeetingTimes					= checkForInvalidMeetingTimes,
			getCheckForMultipleCoursesPerTeacherPerSynchronisationId	= checkForMultipleCoursesPerTeacherPerSynchronisationId,
			getCheckForNonExistentFacilities				= checkForNonExistentFacilities,
			getCheckForNonExistentGroupIds					= checkForNonExistentGroupIds,
			getCheckForNonExistentMeetingLocationIds			= checkForNonExistentMeetingLocationIds,
			getCheckForNonExistentOwnLocationIds				= checkForNonExistentOwnLocationIds,
			getCheckForOverloadedStudents					= checkForOverloadedStudents,
			getCheckForSingletonSynchronisedCourses				= checkForSingletonSynchronisedCourses,
			getCheckForStudentsRequiringMultipleSynchronisedSubjects	= checkForStudentsRequiringMultipleSynchronisedSubjects,
			getCheckForStudentsWithUnrealisableFreePeriodPreference		= checkForStudentsWithUnrealisableFreePeriodPreference,
			getCheckForSynchronisedCoursesWithDifferentIdealTimeslots	= checkForSynchronisedCoursesWithDifferentIdealTimeslots,
			getCheckForSynchronisedCoursesWithDifferentLessonsPerWeek	= checkForSynchronisedCoursesWithDifferentLessonsPerWeek,
			getCheckForSynchronisedCoursesWithExcessLessonsPerWeek		= checkForSynchronisedCoursesWithExcessLessonsPerWeek,
			getCheckForSynchronisedCoursesWithExcessSpecifiedTimes		= checkForSynchronisedCoursesWithExcessSpecifiedTimes,
			getCheckForSynchronisedCoursesWithExcessTimeslotRequests	= checkForSynchronisedCoursesWithExcessTimeslotRequests,
			getCheckForSynchronisedCoursesWithoutSuitableLocations		= checkForSynchronisedCoursesWithoutSuitableLocations,
			getCheckForSynchronisedCoursesWithUnavailableSpecifiedDays	= checkForSynchronisedCoursesWithUnavailableSpecifiedDays,
			getCheckForTeachersWithUnrealisableFreePeriodPreference		= checkForTeachersWithUnrealisableFreePeriodPreference,
			getCheckIfStudentBodiesExceedTeachers				= checkIfStudentBodiesExceedTeachers,
			getCheckIfStudentBodySizeExceedsCapacityOfAllLocations		= checkIfStudentBodySizeExceedsCapacityOfAllLocations,
			getCheckIfStudentBodySizeExceedsLocationCapacity		= checkIfStudentBodySizeExceedsLocationCapacity,
			getCheckIndependenceOfStudentTimeslotsRequestsAndMeetings	= checkIndependenceOfStudentTimeslotsRequestsAndMeetings,
			getCheckIndependenceOfTeacherTimeslotsRequestsAndMeetings	= checkIndependenceOfTeacherTimeslotsRequestsAndMeetings,
			getCheckLocationsAvailabilityToSupportCourses			= checkLocationsAvailabilityToSupportCourses,
			getCheckLocationsForSynchronousSpecifiedTimes			= checkLocationsForSynchronousSpecifiedTimes,
			getCheckMeetingLocationsAvailability				= checkMeetingLocationsAvailability,
			getCheckMinimumConsecutiveLessons				= checkMinimumConsecutiveLessons,
			getCheckNullGroupId						= checkNullGroupId,
			getCheckNullLocationCatalogue					= checkNullLocationCatalogue,
			getCheckNullStudentBodyRegister					= checkNullStudentBodyRegister,
			getCheckNullTeacherRegister					= checkNullTeacherRegister,
			getCheckOwnLocationsAvailability				= checkOwnLocationsAvailability,
			getCheckRequiredLessonsPerWeek					= checkRequiredLessonsPerWeek,
			getCheckSimultaneousAvailabilityOfGroupMembers			= checkSimultaneousAvailabilityOfGroupMembers,
			getCheckStudentsAvailabilityForMandatoryMeetings		= checkStudentsAvailabilityForMandatoryMeetings,
			getCheckStudentsAvailabilityForSpecifiedTimes			= checkStudentsAvailabilityForSpecifiedTimes,
			getCheckStudentsForMultipleLevelsOfSameTopic			= checkStudentsForMultipleLevelsOfSameTopic,
			getCheckStudentsForSynchronousMeetings				= checkStudentsForSynchronousMeetings,
			getCheckStudentsLowerWorkloadBound				= checkStudentsLowerWorkloadBound,
			getCheckStudentsUpperWorkloadBound				= checkStudentsUpperWorkloadBound,
			getCheckSubjectExistence					= checkSubjectExistence,
			getCheckSuitableLocationsForKnowledgeRequirements		= checkSuitableLocationsForKnowledgeRequirements,
			getCheckTeachersAvailabilityForMandatoryMeetings		= checkTeachersAvailabilityForMandatoryMeetings,
			getCheckTeachersForSynchronousMeetings				= checkTeachersForSynchronousMeetings,
			getCheckTeachersUpperWorkloadBound				= checkTeachersUpperWorkloadBound,
			getCheckTeachingCapacityBySubject				= checkTeachingCapacityBySubject,
			getCheckTimeslotIdBounds					= checkTimeslotIdBounds,
			getCheckTimeslotRequests					= checkTimeslotRequests
		} -> (
			checkAvailabilityOfAnyGroupMember,
			checkCapacityOfLocationsForMeetings,
			checkCoursesForSynchronousSpecifiedTimes,
			checkDuplicateMeetingLocationIds,
			checkDuplicateOwnLocationIds,
			checkForAlternativesToSynchronisedCourses,
			checkForDuplicateStudentIds,
			checkForIdleStudents,
			checkForIdleTeachers,
			checkForInvalidMeetingTimes,
			checkForMultipleCoursesPerTeacherPerSynchronisationId,
			checkForNonExistentFacilities,
			checkForNonExistentGroupIds,
			checkForNonExistentMeetingLocationIds,
			checkForNonExistentOwnLocationIds,
			checkForOverloadedStudents,
			checkForSingletonSynchronisedCourses,
			checkForStudentsRequiringMultipleSynchronisedSubjects,
			checkForStudentsWithUnrealisableFreePeriodPreference,
			checkForSynchronisedCoursesWithDifferentIdealTimeslots,
			checkForSynchronisedCoursesWithDifferentLessonsPerWeek,
			checkForSynchronisedCoursesWithExcessLessonsPerWeek,
			checkForSynchronisedCoursesWithExcessSpecifiedTimes,
			checkForSynchronisedCoursesWithExcessTimeslotRequests,
			checkForSynchronisedCoursesWithoutSuitableLocations,
			checkForSynchronisedCoursesWithUnavailableSpecifiedDays,
			checkForTeachersWithUnrealisableFreePeriodPreference,
			checkIfStudentBodiesExceedTeachers,
			checkIfStudentBodySizeExceedsCapacityOfAllLocations,
			checkIfStudentBodySizeExceedsLocationCapacity,
			checkIndependenceOfStudentTimeslotsRequestsAndMeetings,
			checkIndependenceOfTeacherTimeslotsRequestsAndMeetings,
			checkLocationsAvailabilityToSupportCourses,
			checkLocationsForSynchronousSpecifiedTimes,
			checkMeetingLocationsAvailability,
			checkMinimumConsecutiveLessons,
			checkNullGroupId,
			checkNullLocationCatalogue,
			checkNullStudentBodyRegister,
			checkNullTeacherRegister,
			checkOwnLocationsAvailability,
			checkRequiredLessonsPerWeek,
			checkSimultaneousAvailabilityOfGroupMembers,
			checkStudentsAvailabilityForMandatoryMeetings,
			checkStudentsAvailabilityForSpecifiedTimes,
			checkStudentsForMultipleLevelsOfSameTopic,
			checkStudentsForSynchronousMeetings,
			checkStudentsLowerWorkloadBound,
			checkStudentsUpperWorkloadBound,
			checkSubjectExistence,
			checkSuitableLocationsForKnowledgeRequirements,
			checkTeachersAvailabilityForMandatoryMeetings,
			checkTeachersForSynchronousMeetings,
			checkTeachersUpperWorkloadBound,
			checkTeachingCapacityBySubject,
			checkTimeslotIdBounds,
			checkTimeslotRequests
		) -- Deconstruct to a tuple.
	 ) $ Enhanced.EnhancedTuple.xp57Tuple (
		getCheckAvailabilityOfAnyGroupMember def `HXT.xpDefault` HXT.xpAttr checkAvailabilityOfAnyGroupMemberTag HXT.xpickle {-Bool-}
	 ) (
		getCheckCapacityOfLocationsForMeetings def `HXT.xpDefault` HXT.xpAttr checkCapacityOfLocationsForMeetingsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckCoursesForSynchronousSpecifiedTimes def `HXT.xpDefault` HXT.xpAttr checkCoursesForSynchronousSpecifiedTimesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckDuplicateMeetingLocationIds def `HXT.xpDefault` HXT.xpAttr checkDuplicateMeetingLocationIdsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckDuplicateOwnLocationIds def `HXT.xpDefault` HXT.xpAttr checkDuplicateOwnLocationIdsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForAlternativesToSynchronisedCourses def `HXT.xpDefault` HXT.xpAttr checkForAlternativesToSynchronisedCoursesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForDuplicateStudentIds def `HXT.xpDefault` HXT.xpAttr checkForDuplicateStudentIdsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForIdleStudents def `HXT.xpDefault` HXT.xpAttr checkForIdleStudentsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForIdleTeachers def `HXT.xpDefault` HXT.xpAttr checkForIdleTeachersTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForInvalidMeetingTimes def `HXT.xpDefault` HXT.xpAttr checkForInvalidMeetingTimesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForMultipleCoursesPerTeacherPerSynchronisationId def `HXT.xpDefault` HXT.xpAttr checkForMultipleCoursesPerTeacherPerSynchronisationIdTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForNonExistentFacilities def `HXT.xpDefault` HXT.xpAttr checkForNonExistentFacilitiesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForNonExistentGroupIds def `HXT.xpDefault` HXT.xpAttr checkForNonExistentGroupIdsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForNonExistentMeetingLocationIds def `HXT.xpDefault` HXT.xpAttr checkForNonExistentMeetingLocationIdsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForNonExistentOwnLocationIds def `HXT.xpDefault` HXT.xpAttr checkForNonExistentOwnLocationIdsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForOverloadedStudents def `HXT.xpDefault` HXT.xpAttr checkForOverloadedStudentsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForSingletonSynchronisedCourses def `HXT.xpDefault` HXT.xpAttr checkForSingletonSynchronisedCoursesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForStudentsRequiringMultipleSynchronisedSubjects def `HXT.xpDefault` HXT.xpAttr checkForStudentsRequiringMultipleSynchronisedSubjectsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForStudentsWithUnrealisableFreePeriodPreference def `HXT.xpDefault` HXT.xpAttr checkForStudentsWithUnrealisableFreePeriodPreferenceTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForSynchronisedCoursesWithDifferentIdealTimeslots def `HXT.xpDefault` HXT.xpAttr checkForSynchronisedCoursesWithDifferentIdealTimeslotsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForSynchronisedCoursesWithDifferentLessonsPerWeek def `HXT.xpDefault` HXT.xpAttr checkForSynchronisedCoursesWithDifferentLessonsPerWeekTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForSynchronisedCoursesWithExcessLessonsPerWeek def `HXT.xpDefault` HXT.xpAttr checkForSynchronisedCoursesWithExcessLessonsPerWeekTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForSynchronisedCoursesWithExcessSpecifiedTimes def `HXT.xpDefault` HXT.xpAttr checkForSynchronisedCoursesWithExcessSpecifiedTimesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForSynchronisedCoursesWithExcessTimeslotRequests def `HXT.xpDefault` HXT.xpAttr checkForSynchronisedCoursesWithExcessTimeslotRequestsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForSynchronisedCoursesWithoutSuitableLocations def `HXT.xpDefault` HXT.xpAttr checkForSynchronisedCoursesWithoutSuitableLocationsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForSynchronisedCoursesWithUnavailableSpecifiedDays def `HXT.xpDefault` HXT.xpAttr checkForSynchronisedCoursesWithUnavailableSpecifiedDaysTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForTeachersWithUnrealisableFreePeriodPreference def `HXT.xpDefault` HXT.xpAttr checkForTeachersWithUnrealisableFreePeriodPreferenceTag HXT.xpickle {-Bool-}
	 ) (
		getCheckIfStudentBodiesExceedTeachers def `HXT.xpDefault` HXT.xpAttr checkIfStudentBodiesExceedTeachersTag HXT.xpickle {-Bool-}
	 ) (
		getCheckIfStudentBodySizeExceedsCapacityOfAllLocations def `HXT.xpDefault` HXT.xpAttr checkIfStudentBodySizeExceedsCapacityOfAllLocationsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckIfStudentBodySizeExceedsLocationCapacity def `HXT.xpDefault` HXT.xpAttr checkIfStudentBodySizeExceedsLocationCapacityTag HXT.xpickle {-Bool-}
	 ) (
		getCheckIndependenceOfStudentTimeslotsRequestsAndMeetings def `HXT.xpDefault` HXT.xpAttr checkIndependenceOfStudentTimeslotsRequestsAndMeetingsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckIndependenceOfTeacherTimeslotsRequestsAndMeetings def `HXT.xpDefault` HXT.xpAttr checkIndependenceOfTeacherTimeslotsRequestsAndMeetingsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckLocationsAvailabilityToSupportCourses def `HXT.xpDefault` HXT.xpAttr checkLocationsAvailabilityToSupportCoursesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckLocationsForSynchronousSpecifiedTimes def `HXT.xpDefault` HXT.xpAttr checkLocationsForSynchronousSpecifiedTimesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckMeetingLocationsAvailability def `HXT.xpDefault` HXT.xpAttr checkMeetingLocationsAvailabilityTag HXT.xpickle {-Bool-}
	 ) (
		getCheckMinimumConsecutiveLessons def `HXT.xpDefault` HXT.xpAttr checkMinimumConsecutiveLessonsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckNullGroupId def `HXT.xpDefault` HXT.xpAttr checkNullGroupIdTag HXT.xpickle {-Bool-}
	 ) (
		getCheckNullLocationCatalogue def `HXT.xpDefault` HXT.xpAttr checkNullLocationCatalogueTag HXT.xpickle {-Bool-}
	 ) (
		getCheckNullStudentBodyRegister def `HXT.xpDefault` HXT.xpAttr checkNullStudentBodyRegisterTag HXT.xpickle {-Bool-}
	 ) (
		getCheckNullTeacherRegister def `HXT.xpDefault` HXT.xpAttr checkNullTeacherRegisterTag HXT.xpickle {-Bool-}
	 ) (
		getCheckOwnLocationsAvailability def `HXT.xpDefault` HXT.xpAttr checkOwnLocationsAvailabilityTag HXT.xpickle {-Bool-}
	 ) (
		getCheckRequiredLessonsPerWeek def `HXT.xpDefault` HXT.xpAttr checkRequiredLessonsPerWeekTag HXT.xpickle {-Bool-}
	 ) (
		getCheckSimultaneousAvailabilityOfGroupMembers def `HXT.xpDefault` HXT.xpAttr checkSimultaneousAvailabilityOfGroupMembersTag HXT.xpickle {-Bool-}
	 ) (
		getCheckStudentsAvailabilityForMandatoryMeetings def `HXT.xpDefault` HXT.xpAttr checkStudentsAvailabilityForMandatoryMeetingsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckStudentsAvailabilityForSpecifiedTimes def `HXT.xpDefault` HXT.xpAttr checkStudentsAvailabilityForSpecifiedTimesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckStudentsForMultipleLevelsOfSameTopic def `HXT.xpDefault` HXT.xpAttr checkStudentsForMultipleLevelsOfSameTopicTag HXT.xpickle {-Bool-}
	 ) (
		getCheckStudentsForSynchronousMeetings def `HXT.xpDefault` HXT.xpAttr checkStudentsForSynchronousMeetingsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckStudentsLowerWorkloadBound def `HXT.xpDefault` HXT.xpAttr checkStudentsLowerWorkloadBoundTag HXT.xpickle {-Bool-}
	 ) (
		getCheckStudentsUpperWorkloadBound def `HXT.xpDefault` HXT.xpAttr checkStudentsUpperWorkloadBoundTag HXT.xpickle {-Bool-}
	 ) (
		getCheckSubjectExistence def `HXT.xpDefault` HXT.xpAttr checkSubjectExistenceTag HXT.xpickle {-Bool-}
	 ) (
		getCheckSuitableLocationsForKnowledgeRequirements def `HXT.xpDefault` HXT.xpAttr checkSuitableLocationsForKnowledgeRequirementsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckTeachersAvailabilityForMandatoryMeetings def `HXT.xpDefault` HXT.xpAttr checkTeachersAvailabilityForMandatoryMeetingsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckTeachersForSynchronousMeetings def `HXT.xpDefault` HXT.xpAttr checkTeachersForSynchronousMeetingsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckTeachersUpperWorkloadBound def `HXT.xpDefault` HXT.xpAttr checkTeachersUpperWorkloadBoundTag HXT.xpickle {-Bool-}
	 ) (
		getCheckTeachingCapacityBySubject def `HXT.xpDefault` HXT.xpAttr checkTeachingCapacityBySubjectTag HXT.xpickle {-Bool-}
	 ) (
		getCheckTimeslotIdBounds def `HXT.xpDefault` HXT.xpAttr checkTimeslotIdBoundsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckTimeslotRequests def `HXT.xpDefault` HXT.xpAttr checkTimeslotRequestsTag HXT.xpickle {-Bool-}
	 ) where
		def :: ProblemValidationSwitches
		def	= Data.Default.def

instance Data.Default.Default ProblemValidationSwitches where
	def = MkProblemValidationSwitches {
		getCheckAvailabilityOfAnyGroupMember				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckCapacityOfLocationsForMeetings				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckCoursesForSynchronousSpecifiedTimes			= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckDuplicateMeetingLocationIds				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckDuplicateOwnLocationIds					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForAlternativesToSynchronisedCourses			= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForDuplicateStudentIds					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForIdleStudents						= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForIdleTeachers						= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForInvalidMeetingTimes					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForMultipleCoursesPerTeacherPerSynchronisationId	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForNonExistentFacilities				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForNonExistentGroupIds					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForNonExistentMeetingLocationIds			= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForNonExistentOwnLocationIds				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForOverloadedStudents					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForSingletonSynchronisedCourses				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForStudentsRequiringMultipleSynchronisedSubjects	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForStudentsWithUnrealisableFreePeriodPreference		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForSynchronisedCoursesWithDifferentIdealTimeslots	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForSynchronisedCoursesWithDifferentLessonsPerWeek	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForSynchronisedCoursesWithExcessLessonsPerWeek		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForSynchronisedCoursesWithExcessSpecifiedTimes		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForSynchronisedCoursesWithExcessTimeslotRequests	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForSynchronisedCoursesWithoutSuitableLocations		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForSynchronisedCoursesWithUnavailableSpecifiedDays	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForTeachersWithUnrealisableFreePeriodPreference		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckIfStudentBodiesExceedTeachers				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckIfStudentBodySizeExceedsCapacityOfAllLocations		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckIfStudentBodySizeExceedsLocationCapacity		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckIndependenceOfStudentTimeslotsRequestsAndMeetings	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckIndependenceOfTeacherTimeslotsRequestsAndMeetings	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckLocationsAvailabilityToSupportCourses			= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckLocationsForSynchronousSpecifiedTimes			= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckMeetingLocationsAvailability				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckMinimumConsecutiveLessons				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckNullGroupId						= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckNullLocationCatalogue					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckNullStudentBodyRegister					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckNullTeacherRegister					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckOwnLocationsAvailability				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckRequiredLessonsPerWeek					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckSimultaneousAvailabilityOfGroupMembers			= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckStudentsAvailabilityForMandatoryMeetings		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckStudentsAvailabilityForSpecifiedTimes			= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckStudentsForMultipleLevelsOfSameTopic			= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckStudentsForSynchronousMeetings				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckStudentsLowerWorkloadBound				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckStudentsUpperWorkloadBound				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckSubjectExistence					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckSuitableLocationsForKnowledgeRequirements		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckTeachersAvailabilityForMandatoryMeetings		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckTeachersForSynchronousMeetings				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckTeachersUpperWorkloadBound				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckTeachingCapacityBySubject				= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckTimeslotIdBounds					= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckTimeslotRequests					= ProblemConfiguration.ValidationSwitch.defaultCheck
	}

instance ProblemConfiguration.ValidationSwitch.ValidationSwitchSet ProblemValidationSwitches where
	areAllOff problemValidationSwitches	= not $ any ($ problemValidationSwitches) [
		getCheckAvailabilityOfAnyGroupMember,
		getCheckCapacityOfLocationsForMeetings,
		getCheckCoursesForSynchronousSpecifiedTimes,
		getCheckDuplicateMeetingLocationIds,
		getCheckDuplicateOwnLocationIds,
		getCheckForAlternativesToSynchronisedCourses,
		getCheckForDuplicateStudentIds,
		getCheckForIdleStudents,
		getCheckForIdleTeachers,
		getCheckForInvalidMeetingTimes,
		getCheckForMultipleCoursesPerTeacherPerSynchronisationId,
		getCheckForNonExistentFacilities,
		getCheckForNonExistentGroupIds,
		getCheckForNonExistentMeetingLocationIds,
		getCheckForNonExistentOwnLocationIds,
		getCheckForOverloadedStudents,
		getCheckForSingletonSynchronisedCourses,
		getCheckForStudentsRequiringMultipleSynchronisedSubjects,
		getCheckForStudentsWithUnrealisableFreePeriodPreference,
		getCheckForSynchronisedCoursesWithDifferentIdealTimeslots,
		getCheckForSynchronisedCoursesWithDifferentLessonsPerWeek,
		getCheckForSynchronisedCoursesWithExcessLessonsPerWeek,
		getCheckForSynchronisedCoursesWithExcessSpecifiedTimes,
		getCheckForSynchronisedCoursesWithExcessTimeslotRequests,
		getCheckForSynchronisedCoursesWithoutSuitableLocations,
		getCheckForSynchronisedCoursesWithUnavailableSpecifiedDays,
		getCheckForTeachersWithUnrealisableFreePeriodPreference,
		getCheckIfStudentBodiesExceedTeachers,
		getCheckIfStudentBodySizeExceedsCapacityOfAllLocations,
		getCheckIfStudentBodySizeExceedsLocationCapacity,
		getCheckIndependenceOfStudentTimeslotsRequestsAndMeetings,
		getCheckIndependenceOfTeacherTimeslotsRequestsAndMeetings,
		getCheckLocationsForSynchronousSpecifiedTimes,
		getCheckMeetingLocationsAvailability,
		getCheckMinimumConsecutiveLessons,
		getCheckNullGroupId,
		getCheckNullLocationCatalogue,
		getCheckNullStudentBodyRegister,
		getCheckNullTeacherRegister,
		getCheckOwnLocationsAvailability,
		getCheckRequiredLessonsPerWeek,
		getCheckSimultaneousAvailabilityOfGroupMembers,
		getCheckStudentsAvailabilityForMandatoryMeetings,
		getCheckStudentsAvailabilityForSpecifiedTimes,
		getCheckStudentsForMultipleLevelsOfSameTopic,
		getCheckStudentsForSynchronousMeetings,
		getCheckStudentsLowerWorkloadBound,
		getCheckStudentsUpperWorkloadBound,
		getCheckSubjectExistence,
		getCheckSuitableLocationsForKnowledgeRequirements,
		getCheckTeachersAvailabilityForMandatoryMeetings,
		getCheckTeachersForSynchronousMeetings,
		getCheckTeachersUpperWorkloadBound,
		getCheckTeachingCapacityBySubject,
		getCheckTimeslotIdBounds,
		getCheckTimeslotRequests
	 ]

instance Control.DeepSeq.NFData ProblemValidationSwitches where
	rnf (MkProblemValidationSwitches x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56)	= Control.DeepSeq.rnf [
		x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56
	 ]

