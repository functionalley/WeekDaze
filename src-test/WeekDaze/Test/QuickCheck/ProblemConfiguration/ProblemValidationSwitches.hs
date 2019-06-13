{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'ProblemConfiguration.ProblemValidationSwitches.ProblemValidationSwitches'.

 [@CAVEAT@]	Actually the implementation isn't arbitrary at all, rather just a constant record of off switches; which facilitates the generation of arbitrary /ProblemParameters/.
-}

module WeekDaze.Test.QuickCheck.ProblemConfiguration.ProblemValidationSwitches() where

import qualified	Test.QuickCheck
import qualified	WeekDaze.ProblemConfiguration.ProblemValidationSwitches	as ProblemConfiguration.ProblemValidationSwitches

instance Test.QuickCheck.Arbitrary ProblemConfiguration.ProblemValidationSwitches.ProblemValidationSwitches where
	arbitrary	= return {-to Gen-monad-} ProblemConfiguration.ProblemValidationSwitches.MkProblemValidationSwitches {
		ProblemConfiguration.ProblemValidationSwitches.getCheckAvailabilityOfAnyGroupMember				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckCapacityOfLocationsForMeetings				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckCoursesForSynchronousSpecifiedTimes			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckDuplicateMeetingLocationIds				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckDuplicateOwnLocationIds					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForAlternativesToSynchronisedCourses			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForDuplicateStudentIds					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForIdleStudents						= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForIdleTeachers						= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForInvalidMeetingTimes					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForMultipleCoursesPerTeacherPerSynchronisationId		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForNonExistentFacilities					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForNonExistentGroupIds					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForNonExistentMeetingLocationIds				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForNonExistentOwnLocationIds				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForOverloadedStudents					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForSingletonSynchronisedCourses				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForStudentsRequiringMultipleSynchronisedSubjects		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForStudentsWithUnrealisableFreePeriodPreference		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithDifferentIdealTimeslots	= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithDifferentLessonsPerWeek	= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithExcessLessonsPerWeek		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithExcessSpecifiedTimes		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithExcessTimeslotRequests		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithoutSuitableLocations		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithUnavailableSpecifiedDays	= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckForTeachersWithUnrealisableFreePeriodPreference		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodiesExceedTeachers				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodySizeExceedsCapacityOfAllLocations		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodySizeExceedsLocationCapacity			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckIndependenceOfStudentTimeslotsRequestsAndMeetings	= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckIndependenceOfTeacherTimeslotsRequestsAndMeetings	= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckLocationsAvailabilityToSupportCourses			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckLocationsForSynchronousSpecifiedTimes			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckMeetingLocationsAvailability				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckMinimumConsecutiveLessons				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckNullGroupId						= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckNullLocationCatalogue					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckNullStudentBodyRegister					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckNullTeacherRegister					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckOwnLocationsAvailability					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckRequiredLessonsPerWeek					= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckSimultaneousAvailabilityOfGroupMembers			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsAvailabilityForMandatoryMeetings			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsAvailabilityForSpecifiedTimes			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsForMultipleLevelsOfSameTopic			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsForSynchronousMeetings				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsLowerWorkloadBound				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsUpperWorkloadBound				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckSubjectExistence						= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckSuitableLocationsForKnowledgeRequirements		= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckTeachersAvailabilityForMandatoryMeetings			= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckTeachersForSynchronousMeetings				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckTeachersUpperWorkloadBound				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckTeachingCapacityBySubject				= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckTimeslotIdBounds						= False,
		ProblemConfiguration.ProblemValidationSwitches.getCheckTimeslotRequests						= False
	}

