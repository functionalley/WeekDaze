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

 [@DESCRIPTION@]	Defines the toggle-switches used to enable/disable checks on the validity of /StudentView.Timetable.Timetable/.
-}

module WeekDaze.ProblemConfiguration.TimetableValidationSwitches(
-- * Types
-- ** Data-types
	TimetableValidationSwitches(..),
-- * Constants
--	tag,
	checkBookedSubjectsAreRequiredTag,
	checkBookedSubjectsHaveServiceTag,
	checkExistenceOfLocationIdsTag,
	checkExistenceOfStudentBodiesTag,
	checkExistenceOfTeacherIdsTag,
	checkForBookingMeetingClashTag,
	checkForMisplacedLessonsTag,
	checkForOverbookedSubjectsTag,
	checkForOverloadedStudentBodiesTag,
	checkForOverloadedTeachersTag,
	checkForResourceConflictsTag,
	checkMinimumConsecutiveLessonsTag,
	checkResourceAvailabilityTag,
	checkSynchronisedCoursesTag,
	checkTimeslotsPerDayTag
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT
import qualified	WeekDaze.ProblemConfiguration.ValidationSwitch	as ProblemConfiguration.ValidationSwitch
import			WeekDaze.Enhanced.EnhancedBool()

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Maybe
import qualified	WeekDaze.Database.Selector			as Database.Selector

instance Database.Selector.Selector TimetableValidationSwitches where
	fromDatabase connection	projectIdSql	= let
		tableName :: Database.Selector.TableName
		tableName	= showString Database.Selector.tablePrefix tag
	 in do
		switchesRows	<- map (
			map $ Data.Maybe.fromMaybe ProblemConfiguration.ValidationSwitch.defaultCheck . either (
				error . showString "WeekDaze.ProblemConfiguration.TimetableValidationSwitches.fromDatabase:\tfailed to parse the value for a switch read from the database; " . show
			) id . Database.HDBC.safeFromSql
		 ) `fmap` Database.Selector.select connection [
			checkBookedSubjectsAreRequiredTag,
			checkBookedSubjectsHaveServiceTag,
			checkExistenceOfLocationIdsTag,
			checkExistenceOfStudentBodiesTag,
			checkExistenceOfTeacherIdsTag,
			checkForBookingMeetingClashTag,
			checkForMisplacedLessonsTag,
			checkForOverbookedSubjectsTag,
			checkForOverloadedStudentBodiesTag,
			checkForOverloadedTeachersTag,
			checkForResourceConflictsTag,
			checkMinimumConsecutiveLessonsTag,
			checkResourceAvailabilityTag,
			checkSynchronisedCoursesTag,
			checkTimeslotsPerDayTag
		 ] [tableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		return {-to IO-Monad-} $ case switchesRows of
			[]		-> Data.Default.def
			[switchesRow]	-> case switchesRow of
				[
					checkBookedSubjectsAreRequired,
					checkBookedSubjectsHaveService,
					checkSynchronisedCourses,
					checkExistenceOfLocationIds,
					checkExistenceOfStudentBodies,
					checkExistenceOfTeacherIds,
					checkForBookingMeetingClash,
					checkForMisplacedLessons,
					checkForOverbookedSubjects,
					checkForOverloadedStudentBodies,
					checkForOverloadedTeachers,
					checkForResourceConflicts,
					checkMinimumConsecutiveLessons,
					checkResourceAvailability,
					checkTimeslotsPerDay
				 ] -> MkTimetableValidationSwitches {
					getCheckBookedSubjectsAreRequired	= checkBookedSubjectsAreRequired,
					getCheckBookedSubjectsHaveService	= checkBookedSubjectsHaveService,
					getCheckSynchronisedCourses		= checkSynchronisedCourses,
					getCheckExistenceOfLocationIds		= checkExistenceOfLocationIds,
					getCheckExistenceOfStudentBodies	= checkExistenceOfStudentBodies,
					getCheckExistenceOfTeacherIds		= checkExistenceOfTeacherIds,
					getCheckForBookingMeetingClash		= checkForBookingMeetingClash,
					getCheckForMisplacedLessons		= checkForMisplacedLessons,
					getCheckForOverbookedSubjects		= checkForOverbookedSubjects,
					getCheckForOverloadedStudentBodies	= checkForOverloadedStudentBodies,
					getCheckForOverloadedTeachers		= checkForOverloadedTeachers,
					getCheckForResourceConflicts		= checkForResourceConflicts,
					getCheckMinimumConsecutiveLessons	= checkMinimumConsecutiveLessons,
					getCheckResourceAvailability		= checkResourceAvailability,
					getCheckTimeslotsPerDay			= checkTimeslotsPerDay
				}
				_ -> error $ "WeekDaze.ProblemConfiguration.TimetableValidationSwitches.fromDatabase:\tunexpected number of columns=" ++ show (length switchesRow) ++ " in row of table " ++ show tableName ++ "."
			_		-> error $ "WeekDaze.ProblemConfiguration.TimetableValidationSwitches.fromDatabase:\tunexpected number of rows=" ++ show (length switchesRows) ++ " selected from table " ++ show tableName ++ "."
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag					= "timetableValidationSwitches"

-- | Used to qualify SQL & XML.
checkBookedSubjectsAreRequiredTag :: String
checkBookedSubjectsAreRequiredTag	= "checkBookedSubjectsAreRequired"

-- | Used to qualify SQL & XML.
checkBookedSubjectsHaveServiceTag :: String
checkBookedSubjectsHaveServiceTag	= "checkBookedSubjectsHaveService"

-- | Used to qualify SQL & XML.
checkExistenceOfLocationIdsTag :: String
checkExistenceOfLocationIdsTag		= "checkExistenceOfLocationIds"

-- | Used to qualify SQL & XML.
checkExistenceOfStudentBodiesTag :: String
checkExistenceOfStudentBodiesTag	= "checkExistenceOfStudentBodies"

-- | Used to qualify SQL & XML.
checkExistenceOfTeacherIdsTag :: String
checkExistenceOfTeacherIdsTag		= "checkExistenceOfTeacherIds"

-- | Used to qualify SQL & XML.
checkForBookingMeetingClashTag :: String
checkForBookingMeetingClashTag		= "checkForBookingMeetingClash"

-- | Used to qualify SQL & XML.
checkForMisplacedLessonsTag :: String
checkForMisplacedLessonsTag		= "checkForMisplacedLessons"

-- | Used to qualify SQL & XML.
checkForOverbookedSubjectsTag :: String
checkForOverbookedSubjectsTag		= "checkForOverbookedSubjects"

-- | Used to qualify SQL & XML.
checkForOverloadedStudentBodiesTag :: String
checkForOverloadedStudentBodiesTag	= "checkForOverloadedStudentBodies"

-- | Used to qualify SQL & XML.
checkForOverloadedTeachersTag :: String
checkForOverloadedTeachersTag		= "checkForOverloadedTeachers"

-- | Used to qualify SQL & XML.
checkForResourceConflictsTag :: String
checkForResourceConflictsTag		= "checkForResourceConflicts"

-- | Used to qualify SQL & XML.
checkMinimumConsecutiveLessonsTag :: String
checkMinimumConsecutiveLessonsTag	= "checkMinimumConsecutiveLessons"

-- | Used to qualify SQL & XML.
checkResourceAvailabilityTag :: String
checkResourceAvailabilityTag		= "checkResourceAvailability"

-- | Used to qualify SQL & XML.
checkSynchronisedCoursesTag :: String
checkSynchronisedCoursesTag		= "checkSynchronisedCourses"

-- | Used to qualify SQL & XML.
checkTimeslotsPerDayTag :: String
checkTimeslotsPerDayTag			= "checkTimeslotsPerDay"

-- | Encapsulates the data which defines the problem.
data TimetableValidationSwitches	= MkTimetableValidationSwitches {
	getCheckBookedSubjectsAreRequired	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that all /subject/s for which there's a /booking/, are required by the /student-body/.
	getCheckBookedSubjectsHaveService	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that all /subject/s for which there's a booking, have been offered as a /course/, by the referenced /teacher/.
	getCheckExistenceOfLocationIds		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that all /locationId/s referenced in an imported /studentViewTimetable/, exist in the /location-catalogue/.
	getCheckExistenceOfStudentBodies	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that all /student-bodies/ referenced in an imported /studentViewTimetable/, exist in the /studentBody-register/.
	getCheckExistenceOfTeacherIds		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that all /teacherId/s referenced in an imported /studentViewTimetable/, exist in the /teacher-register/.
	getCheckForBookingMeetingClash		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /lesson/s booked at the same /time/ as a /meeting/, with a common /resource/-requirement.
	getCheckForMisplacedLessons		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /lesson/s booked at /location/s lacking either adequate /capacity/ or /facilities/ for their /course/.
	getCheckForOverbookedSubjects		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /student-bodies/ for whom more /lesson/s have been booked, in a specific /subject/, that required by the /course/ of which they're a part.
	getCheckForOverloadedStudentBodies	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /student-bodies/ for whom more /lesson/s have been booked, than allocated for tuition in one week.
	getCheckForOverloadedTeachers		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /teacher/s for whom more /lesson/s have been booked, than allocated for tuition in one week.
	getCheckForResourceConflicts		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /time/s at which /resource/s have been allocated in more than one /location/.
	getCheckMinimumConsecutiveLessons	:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check for /lesson/s which have been booked in runlengths shorter than required by the /course/ of which they're a part.
	getCheckResourceAvailability		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that /resource/s have been booked only on /day/s when they're /available/.
	getCheckSynchronisedCourses		:: ProblemConfiguration.ValidationSwitch.Check,	-- ^ Check that all the /teacher/s of a set of /synchronised course/s, have booked synchronous /lesson/s.
	getCheckTimeslotsPerDay			:: ProblemConfiguration.ValidationSwitch.Check	-- ^ Check that all /day/s have the correct number of /time-slot/s.
} deriving (Eq, Show)

instance () => HXT.XmlPickler TimetableValidationSwitches where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, h, i, j, k, l, m, n, o, p)	-> MkTimetableValidationSwitches a b c d e f h i j k l m n o p,	-- Construct from a tuple.
		\MkTimetableValidationSwitches {
			getCheckBookedSubjectsAreRequired	= checkBookedSubjectsAreRequired,
			getCheckBookedSubjectsHaveService	= checkBookedSubjectsHaveService,
			getCheckSynchronisedCourses		= checkSynchronisedCourses,
			getCheckExistenceOfLocationIds		= checkExistenceOfLocationIds,
			getCheckExistenceOfStudentBodies	= checkExistenceOfStudentBodies,
			getCheckExistenceOfTeacherIds		= checkExistenceOfTeacherIds,
			getCheckForBookingMeetingClash		= checkForBookingMeetingClash,
			getCheckForMisplacedLessons		= checkForMisplacedLessons,
			getCheckForOverbookedSubjects		= checkForOverbookedSubjects,
			getCheckForOverloadedStudentBodies	= checkForOverloadedStudentBodies,
			getCheckForOverloadedTeachers		= checkForOverloadedTeachers,
			getCheckForResourceConflicts		= checkForResourceConflicts,
			getCheckMinimumConsecutiveLessons	= checkMinimumConsecutiveLessons,
			getCheckResourceAvailability		= checkResourceAvailability,
			getCheckTimeslotsPerDay			= checkTimeslotsPerDay
		} -> (
			checkBookedSubjectsAreRequired,
			checkBookedSubjectsHaveService,
			checkSynchronisedCourses,
			checkExistenceOfLocationIds,
			checkExistenceOfStudentBodies,
			checkExistenceOfTeacherIds,
			checkForBookingMeetingClash,
			checkForMisplacedLessons,
			checkForOverbookedSubjects,
			checkForOverloadedStudentBodies,
			checkForOverloadedTeachers,
			checkForResourceConflicts,
			checkMinimumConsecutiveLessons,
			checkResourceAvailability,
			checkTimeslotsPerDay
		) -- Deconstruct to a tuple.
	 ) $ HXT.xp15Tuple (
		getCheckBookedSubjectsAreRequired def `HXT.xpDefault` HXT.xpAttr checkBookedSubjectsAreRequiredTag HXT.xpickle {-Bool-}
	 ) (
		getCheckBookedSubjectsHaveService def `HXT.xpDefault` HXT.xpAttr checkBookedSubjectsHaveServiceTag HXT.xpickle {-Bool-}
	 ) (
		getCheckExistenceOfLocationIds def `HXT.xpDefault` HXT.xpAttr checkExistenceOfLocationIdsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckExistenceOfStudentBodies def `HXT.xpDefault` HXT.xpAttr checkExistenceOfStudentBodiesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckExistenceOfTeacherIds def `HXT.xpDefault` HXT.xpAttr checkExistenceOfTeacherIdsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForBookingMeetingClash def `HXT.xpDefault` HXT.xpAttr checkForBookingMeetingClashTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForMisplacedLessons def `HXT.xpDefault` HXT.xpAttr checkForMisplacedLessonsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForOverbookedSubjects def `HXT.xpDefault` HXT.xpAttr checkForOverbookedSubjectsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForOverloadedStudentBodies def `HXT.xpDefault` HXT.xpAttr checkForOverloadedStudentBodiesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForOverloadedTeachers def `HXT.xpDefault` HXT.xpAttr checkForOverloadedTeachersTag HXT.xpickle {-Bool-}
	 ) (
		getCheckForResourceConflicts def `HXT.xpDefault` HXT.xpAttr checkForResourceConflictsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckMinimumConsecutiveLessons def `HXT.xpDefault` HXT.xpAttr checkMinimumConsecutiveLessonsTag HXT.xpickle {-Bool-}
	 ) (
		getCheckResourceAvailability def `HXT.xpDefault` HXT.xpAttr checkResourceAvailabilityTag HXT.xpickle {-Bool-}
	 ) (
		getCheckSynchronisedCourses def `HXT.xpDefault` HXT.xpAttr checkSynchronisedCoursesTag HXT.xpickle {-Bool-}
	 ) (
		getCheckTimeslotsPerDay def `HXT.xpDefault` HXT.xpAttr checkTimeslotsPerDayTag HXT.xpickle {-Bool-}
	 ) where
		def :: TimetableValidationSwitches
		def	= Data.Default.def

instance Data.Default.Default TimetableValidationSwitches where
	def = MkTimetableValidationSwitches {
		getCheckBookedSubjectsAreRequired	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckBookedSubjectsHaveService	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckSynchronisedCourses		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckExistenceOfLocationIds		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckExistenceOfStudentBodies	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckExistenceOfTeacherIds		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForBookingMeetingClash		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForMisplacedLessons		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForOverbookedSubjects		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForOverloadedStudentBodies	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForOverloadedTeachers		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckForResourceConflicts		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckMinimumConsecutiveLessons	= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckResourceAvailability		= ProblemConfiguration.ValidationSwitch.defaultCheck,
		getCheckTimeslotsPerDay			= ProblemConfiguration.ValidationSwitch.defaultCheck
	}

instance ProblemConfiguration.ValidationSwitch.ValidationSwitchSet TimetableValidationSwitches where
	areAllOff timetableValidationSwitches	= not $ any ($ timetableValidationSwitches) [
		getCheckBookedSubjectsAreRequired,
		getCheckBookedSubjectsHaveService,
		getCheckSynchronisedCourses,
		getCheckExistenceOfLocationIds,
		getCheckExistenceOfStudentBodies,
		getCheckExistenceOfTeacherIds,
		getCheckForBookingMeetingClash,
		getCheckForMisplacedLessons,
		getCheckForOverbookedSubjects,
		getCheckForOverloadedStudentBodies,
		getCheckForOverloadedTeachers,
		getCheckForResourceConflicts,
		getCheckMinimumConsecutiveLessons,
		getCheckResourceAvailability,
		getCheckTimeslotsPerDay
	 ]

instance Control.DeepSeq.NFData TimetableValidationSwitches where
	rnf (MkTimetableValidationSwitches x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)	= Control.DeepSeq.rnf [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14]

