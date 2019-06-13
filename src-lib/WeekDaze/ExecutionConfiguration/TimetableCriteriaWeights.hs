{-# LANGUAGE CPP, FlexibleContexts, UndecidableInstances #-}
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

	* Defines the weightings, associated with each of the /soft/-constraints used to compare & select between alternative /timetable/s.

	* /Hard/-constraints (e.g. that a /student/ or /teacher/ can't be in more than one place at a /time/,
	or that the capacity of a /location/ can't be exceeded),
	are never violated, & so don't need to be quantified for subsequent comparison.
-}

module WeekDaze.ExecutionConfiguration.TimetableCriteriaWeights(
-- * Types
-- ** Type-synonyms
--	Mutator,
-- ** Data-types
	TimetableCriteriaWeights(..),
-- * Constants
	associationList,
	tag,
	weightOfMaximiseComplianceWithFreePeriodPreferencesTag,
	weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacityTag,
	weightOfMaximiseMeanStudentClassSizeTag,
	weightOfMaximiseSynchronisationOfSynchronisedCoursesTag,
	weightOfMaximiseWeightedMeanStudentBodyUtilisationRatioTag,
	weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequestTag,
	weightOfMinimiseDispersionOfStudentFreePeriodsPerDayTag,
	weightOfMinimiseDispersionOfTeacherFreePeriodsPerDayTag,
	weightOfMinimiseDispersionOfTeacherWorkloadTag,
	weightOfMinimiseMeanInterCampusMigrationsOfStudentsTag,
	weightOfMinimiseMeanInterCampusMigrationsOfTeachersTag,
	weightOfMinimiseMeanLocationChangesOfTeachersTag,
	weightOfMinimiseMeanLocusOperandiOfTeachersTag,
	weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledgeTag,
	weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledgeTag,
	weightOfMinimiseMeanStudentBodyCombinationsPerLessonTag,
	weightOfMinimiseRatioOfConsecutiveEqualLessonsTag,
	weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDayTag,
-- * Functions
	calculateWeightedMean,
-- ** Mutators
	zeroWeightOfMaximiseComplianceWithFreePeriodPreferences,
--	zeroWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity,
--	zeroWeightOfMaximiseMeanStudentClassSize,
	zeroWeightOfMaximiseSynchronisationOfSynchronisedCourses,
--	zeroWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio,
	zeroWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
	zeroWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay,
--	zeroWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay,
--	zeroWeightOfMinimiseDispersionOfTeacherWorkload,
	zeroWeightOfMinimiseMeanInterCampusMigrationsOfStudents,
	zeroWeightOfMinimiseMeanInterCampusMigrationsOfTeachers,
	zeroWeightOfMinimiseMeanLocationChangesOfTeachers,
	zeroWeightOfMinimiseMeanLocusOperandiOfTeachers,
--	zeroWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge,
	zeroWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
	zeroWeightOfMinimiseMeanStudentBodyCombinationsPerLesson,
	zeroWeightOfMinimiseRatioOfConsecutiveEqualLessons,
	zeroWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay
) where

import qualified	Control.DeepSeq
import qualified	Control.Monad.Writer
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.ExecutionConfiguration.Criterion	as ExecutionConfiguration.Criterion
import qualified	WeekDaze.ExecutionConfiguration.CriterionWeight	as ExecutionConfiguration.CriterionWeight

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Maybe
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector			as Database.Selector

instance (
	Data.Convertible.Convertible	Database.HDBC.SqlValue w,	-- Flexible context & Undecidable instance.
	Data.Typeable.Typeable		w,
	RealFrac			w
 ) => Database.Selector.Selector (TimetableCriteriaWeights w) where
	fromDatabase connection	projectIdSql	= let
		tableName :: Database.Selector.TableName
		tableName	= Database.Selector.tablePrefix ++ tag
	 in do
		criterionWeightRows	<- map (
			map $ Data.Maybe.fromMaybe Data.Default.def . either (
				error . showString "WeekDaze.ExecutionConfiguration.TimetableCriteriaWeights.fromDatabase:\tfailed to parse the value of a weight read from the database; " . show
			) id . Database.HDBC.safeFromSql
		 ) `fmap` Database.Selector.select connection (
			map fst associationList
		 ) [tableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		return {-to IO-Monad-} $ case criterionWeightRows of
			[]		-> Data.Default.def
			[weightRow]	-> case weightRow of
				[
					weightOfMaximiseComplianceWithFreePeriodPreferences,
					weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity,
					weightOfMaximiseMeanStudentClassSize,
					weightOfMaximiseSynchronisationOfSynchronisedCourses,
					weightOfMaximiseWeightedMeanStudentBodyUtilisationRatio,
					weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
					weightOfMinimiseDispersionOfStudentFreePeriodsPerDay,
					weightOfMinimiseDispersionOfTeacherFreePeriodsPerDay,
					weightOfMinimiseDispersionOfTeacherWorkload,
					weightOfMinimiseMeanInterCampusMigrationsOfStudents,
					weightOfMinimiseMeanInterCampusMigrationsOfTeachers,
					weightOfMinimiseMeanLocationChangesOfTeachers,
					weightOfMinimiseMeanLocusOperandiOfTeachers,
					weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge,
					weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
					weightOfMinimiseMeanStudentBodyCombinationsPerLesson,
					weightOfMinimiseRatioOfConsecutiveEqualLessons,
					weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay
				 ] -> MkTimetableCriteriaWeights {
					getWeightOfMaximiseComplianceWithFreePeriodPreferences			= weightOfMaximiseComplianceWithFreePeriodPreferences,
					getWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity	= weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity,
					getWeightOfMaximiseMeanStudentClassSize					= weightOfMaximiseMeanStudentClassSize,
					getWeightOfMaximiseSynchronisationOfSynchronisedCourses			= weightOfMaximiseSynchronisationOfSynchronisedCourses,
					getWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio		= weightOfMaximiseWeightedMeanStudentBodyUtilisationRatio,
					getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest	= weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
					getWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay			= weightOfMinimiseDispersionOfStudentFreePeriodsPerDay,
					getWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay			= weightOfMinimiseDispersionOfTeacherFreePeriodsPerDay,
					getWeightOfMinimiseDispersionOfTeacherWorkload				= weightOfMinimiseDispersionOfTeacherWorkload,
					getWeightOfMinimiseMeanInterCampusMigrationsOfStudents			= weightOfMinimiseMeanInterCampusMigrationsOfStudents,
					getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers			= weightOfMinimiseMeanInterCampusMigrationsOfTeachers,
					getWeightOfMinimiseMeanLocationChangesOfTeachers			= weightOfMinimiseMeanLocationChangesOfTeachers,
					getWeightOfMinimiseMeanLocusOperandiOfTeachers				= weightOfMinimiseMeanLocusOperandiOfTeachers,
					getWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge		= weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge,
					getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge	= weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
					getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson			= weightOfMinimiseMeanStudentBodyCombinationsPerLesson,
					getWeightOfMinimiseRatioOfConsecutiveEqualLessons			= weightOfMinimiseRatioOfConsecutiveEqualLessons,
					getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay		= weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay
				}
				_ -> error $ "WeekDaze.ExecutionConfiguration.TimetableCriteriaWeights.fromDatabase:\tunexpected number of columns=" ++ show (length weightRow) ++ " in row of table " ++ show tableName ++ "."
			_		-> error $ "WeekDaze.ExecutionConfiguration.TimetableCriteriaWeights.fromDatabase:\tunexpected number of rows=" ++ show (length criterionWeightRows) ++ " selected from table " ++ show tableName ++ "."
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag									= "timetableCriteriaWeights"

-- | Used to qualify XML.
weightOfMaximiseComplianceWithFreePeriodPreferencesTag :: String
weightOfMaximiseComplianceWithFreePeriodPreferencesTag			= "maximiseComplianceWithFreePeriodPreferences"

-- | Used to qualify XML.
weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacityTag :: String
weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacityTag	= "maximiseMeanRatioOfStudentClassSizeToLocationCapacity"

-- | Used to qualify XML.
weightOfMaximiseMeanStudentClassSizeTag :: String
weightOfMaximiseMeanStudentClassSizeTag					= "maximiseMeanStudentClassSize"

-- | Used to qualify XML.
weightOfMaximiseSynchronisationOfSynchronisedCoursesTag :: String
weightOfMaximiseSynchronisationOfSynchronisedCoursesTag			= "maximiseSynchronisationOfSynchronisedCourses"

-- | Used to qualify XML.
weightOfMaximiseWeightedMeanStudentBodyUtilisationRatioTag :: String
weightOfMaximiseWeightedMeanStudentBodyUtilisationRatioTag		= "maximiseWeightedMeanStudentBodyUtilisationRatio"

-- | Used to qualify XML.
weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequestTag :: String
weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequestTag	= "minimiseAverageAbsoluteDeviationFromIdealTimeslotRequest"

-- | Used to qualify XML.
weightOfMinimiseDispersionOfStudentFreePeriodsPerDayTag :: String
weightOfMinimiseDispersionOfStudentFreePeriodsPerDayTag			= "minimiseDispersionOfStudentFreePeriodsPerDay"

-- | Used to qualify XML.
weightOfMinimiseDispersionOfTeacherFreePeriodsPerDayTag :: String
weightOfMinimiseDispersionOfTeacherFreePeriodsPerDayTag			= "minimiseDispersionOfTeacherFreePeriodsPerDay"

-- | Used to qualify XML.
weightOfMinimiseDispersionOfTeacherWorkloadTag :: String
weightOfMinimiseDispersionOfTeacherWorkloadTag				= "minimiseDispersionOfTeacherWorkload"

-- | Used to qualify XML.
weightOfMinimiseMeanInterCampusMigrationsOfStudentsTag :: String
weightOfMinimiseMeanInterCampusMigrationsOfStudentsTag			= "minimiseMeanInterCampusMigrationsOfStudents"

-- | Used to qualify XML.
weightOfMinimiseMeanInterCampusMigrationsOfTeachersTag :: String
weightOfMinimiseMeanInterCampusMigrationsOfTeachersTag			= "minimiseMeanInterCampusMigrationsOfTeachers"

-- | Used to qualify XML.
weightOfMinimiseMeanLocationChangesOfTeachersTag :: String
weightOfMinimiseMeanLocationChangesOfTeachersTag			= "minimiseMeanLocationChangesOfTeachers"

-- | Used to qualify XML.
weightOfMinimiseMeanLocusOperandiOfTeachersTag :: String
weightOfMinimiseMeanLocusOperandiOfTeachersTag				= "minimiseMeanLocusOperandiOfTeachers"

-- | Used to qualify XML.
weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledgeTag :: String
weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledgeTag		= "minimiseMeanRatioOfIncompletelyBookedCoreKnowledge"

-- | Used to qualify XML.
weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledgeTag :: String
weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledgeTag	= "minimiseMeanRatioOfIncompletelyBookedOptionalKnowledge"

-- | Used to qualify XML.
weightOfMinimiseMeanStudentBodyCombinationsPerLessonTag :: String
weightOfMinimiseMeanStudentBodyCombinationsPerLessonTag			= "minimiseMeanStudentBodyCombinationsPerLesson"

-- | Used to qualify XML.
weightOfMinimiseRatioOfConsecutiveEqualLessonsTag :: String
weightOfMinimiseRatioOfConsecutiveEqualLessonsTag			= "minimiseRatioOfConsecutiveEqualLessons"

-- | Used to qualify XML.
weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDayTag :: String
weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDayTag		= "minimiseRatioOfSeparatedEqualLessonsWithinAnyDay"

-- | The ordered list of tags & accessors.
associationList :: [(String, TimetableCriteriaWeights w -> ExecutionConfiguration.CriterionWeight.CriterionWeight w)]
associationList	= [
	(weightOfMaximiseComplianceWithFreePeriodPreferencesTag,		getWeightOfMaximiseComplianceWithFreePeriodPreferences),
	(weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacityTag,	getWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity),
	(weightOfMaximiseMeanStudentClassSizeTag,				getWeightOfMaximiseMeanStudentClassSize),
	(weightOfMaximiseSynchronisationOfSynchronisedCoursesTag,		getWeightOfMaximiseSynchronisationOfSynchronisedCourses),
	(weightOfMaximiseWeightedMeanStudentBodyUtilisationRatioTag,		getWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio),
	(weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequestTag,	getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest),
	(weightOfMinimiseDispersionOfStudentFreePeriodsPerDayTag,		getWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay),
	(weightOfMinimiseDispersionOfTeacherFreePeriodsPerDayTag,		getWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay),
	(weightOfMinimiseDispersionOfTeacherWorkloadTag,			getWeightOfMinimiseDispersionOfTeacherWorkload),
	(weightOfMinimiseMeanInterCampusMigrationsOfStudentsTag,		getWeightOfMinimiseMeanInterCampusMigrationsOfStudents),
	(weightOfMinimiseMeanInterCampusMigrationsOfTeachersTag,		getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers),
	(weightOfMinimiseMeanLocationChangesOfTeachersTag,			getWeightOfMinimiseMeanLocationChangesOfTeachers),
	(weightOfMinimiseMeanLocusOperandiOfTeachersTag,			getWeightOfMinimiseMeanLocusOperandiOfTeachers),
	(weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledgeTag,		getWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge),
	(weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledgeTag,	getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge),
	(weightOfMinimiseMeanStudentBodyCombinationsPerLessonTag,		getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson),
	(weightOfMinimiseRatioOfConsecutiveEqualLessonsTag,			getWeightOfMinimiseRatioOfConsecutiveEqualLessons),
	(weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDayTag,		getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay)
 ]

-- | The weight of criteria used to select a /timetable/ from alternatives.
data TimetableCriteriaWeights w	= MkTimetableCriteriaWeights {
	getWeightOfMaximiseComplianceWithFreePeriodPreferences			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when it complies with any /free time-slot preference/, of /student-bodies/ & /teacher/s.
	getWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the mean over all /booking/s, of the ratio of the size of the /student-class/ to the /location/'s capacity, is greatest.
	getWeightOfMaximiseMeanStudentClassSize					:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the mean over the size of /student-class/es, relative to the total number of /student/s, is greatest.
	getWeightOfMaximiseSynchronisationOfSynchronisedCourses			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the /lesson/s of /synchronised course/s are consistently synchronised.
	getWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the mean over all /student-bodies/, of their utilisation-ratio, is maximised. It doesn't distinguish between /core/ & /optional/ subjects like /minimiseMeanRatioOfIncompletelyBookedCoreKnowledge/ & /minimiseMeanRatioOfIncompletelyBookedOptionalKnowledge/, but it does value partially booked /course/s.
	getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when /lesson/s are booked close to any /ideal timeslot-request/ for their /course/. CAVEAT: excessive weight makes it preferable to take the penalty from 'getWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge' or 'getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge', by not scheduling a /lesson/ if the available /time-slot/s are far from any specified ideal.
	getWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when /student/s' free /period/s are spread evenly across their working-week.
	getWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when /teacher/s' free /period/s are spread evenly across their working-week.
	getWeightOfMinimiseDispersionOfTeacherWorkload				:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the relative workload between all /teacher/s, is similar.
	getWeightOfMinimiseMeanInterCampusMigrationsOfStudents			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the average over all /student/s, of the number of inter-/campus/ migrations in a week, is minimised.
	getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the average over all /teacher/s, of the number of inter-/campus/ migrations in a week, is minimised.
	getWeightOfMinimiseMeanLocationChangesOfTeachers			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the average over all /teacher/s, of the number of changes in their /location/ in a week, is minimised; cf. /minimiseMeanLocusOperandiOfTeachers/, which merely counts the number of distinct /location/s used.
	getWeightOfMinimiseMeanLocusOperandiOfTeachers				:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the average over all /teacher/s, of the size of their locus-operandi, is minimised; based merely on the number of /location/s, rather than the distance or the number of changes, between them.
	getWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the average over all /student-bodies/, of the ratio of incompletely booked /core/ /knowledge-requirements/, to all requirements, is minimised. All incomplete /course/s are valued equally, regardless of the extent.
	getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the average over all /student-bodies/, of the ratio of incompletely booked /optional/ /knowledge-requirements/, to all requirements, is minimised. All incomplete /course/s are valued equally, regardless of the extent; cf. /maximiseWeightedMeanStudentBodyUtilisationRatio/.
	getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when the average number of combinations of /student-class/ per /lesson/-definition, is minimised; ideally a /student-body/ always studies a /subject/, in a class composed from the same other /student-bodies/.
	getWeightOfMinimiseRatioOfConsecutiveEqualLessons			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /timetable/ is preferred, when blocks of consecutive identical /lesson/s are booked, whose length most closely corresponds, to that requested for the /course/ to which they belong.
	getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w	-- ^ A /timetable/ is preferred, when the /lesson/s of any /course/, are on any /day/, booked in one contiguous block.
} deriving (Eq, Show)

instance (Eq w, Num w) => ToolShed.SelfValidate.SelfValidator (TimetableCriteriaWeights w) where
	getErrors timetableCriteriaWeights	= ToolShed.SelfValidate.extractErrors [
		(
			ExecutionConfiguration.CriterionWeight.areAllZero timetableCriteriaWeights,
			"The weight of @ least one timetable-criterion must be non-zero"
		) -- Pair.
	 ]


instance Num w => Data.Default.Default (TimetableCriteriaWeights w) where
	def = MkTimetableCriteriaWeights {
		getWeightOfMaximiseComplianceWithFreePeriodPreferences			= Data.Default.def,
		getWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity	= Data.Default.def,
		getWeightOfMaximiseMeanStudentClassSize					= Data.Default.def,
		getWeightOfMaximiseSynchronisationOfSynchronisedCourses			= Data.Default.def,
		getWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio		= Data.Default.def,
		getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest	= Data.Default.def,
		getWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay			= Data.Default.def,
		getWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay			= Data.Default.def,
		getWeightOfMinimiseDispersionOfTeacherWorkload				= Data.Default.def,
		getWeightOfMinimiseMeanInterCampusMigrationsOfStudents			= Data.Default.def,
		getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers			= Data.Default.def,
		getWeightOfMinimiseMeanLocationChangesOfTeachers			= Data.Default.def,
		getWeightOfMinimiseMeanLocusOperandiOfTeachers				= Data.Default.def,
		getWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge		= Data.Default.def,
		getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge	= Data.Default.def,
		getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson			= Data.Default.def,
		getWeightOfMinimiseRatioOfConsecutiveEqualLessons			= Data.Default.def,
		getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay		= Data.Default.def
	}

instance (HXT.XmlPickler w, Ord w, Real w) => HXT.XmlPickler (TimetableCriteriaWeights w) where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag $ HXT.xpWrap (
		\(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)	-> MkTimetableCriteriaWeights a b c d e f g h i j k l m n o p q r,	-- Construct from a tuple.
		\MkTimetableCriteriaWeights {
			getWeightOfMaximiseComplianceWithFreePeriodPreferences			= weightOfMaximiseComplianceWithFreePeriodPreferences,
			getWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity	= weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity,
			getWeightOfMaximiseMeanStudentClassSize					= weightOfMaximiseMeanStudentClassSize,
			getWeightOfMaximiseSynchronisationOfSynchronisedCourses			= weightOfMaximiseSynchronisationOfSynchronisedCourses,
			getWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio		= weightOfMaximiseWeightedMeanStudentBodyUtilisationRatio,
			getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest	= weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
			getWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay			= weightOfMinimiseDispersionOfStudentFreePeriodsPerDay,
			getWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay			= weightOfMinimiseDispersionOfTeacherFreePeriodsPerDay,
			getWeightOfMinimiseDispersionOfTeacherWorkload				= weightOfMinimiseDispersionOfTeacherWorkload,
			getWeightOfMinimiseMeanInterCampusMigrationsOfStudents			= weightOfMinimiseMeanInterCampusMigrationsOfStudents,
			getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers			= weightOfMinimiseMeanInterCampusMigrationsOfTeachers,
			getWeightOfMinimiseMeanLocationChangesOfTeachers			= weightOfMinimiseMeanLocationChangesOfTeachers,
			getWeightOfMinimiseMeanLocusOperandiOfTeachers				= weightOfMinimiseMeanLocusOperandiOfTeachers,
			getWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge		= weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge,
			getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge	= weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
			getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson			= weightOfMinimiseMeanStudentBodyCombinationsPerLesson,
			getWeightOfMinimiseRatioOfConsecutiveEqualLessons			= weightOfMinimiseRatioOfConsecutiveEqualLessons,
			getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay		= weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay
		} -> (
			weightOfMaximiseComplianceWithFreePeriodPreferences,
			weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity,
			weightOfMaximiseMeanStudentClassSize,
			weightOfMaximiseSynchronisationOfSynchronisedCourses,
			weightOfMaximiseWeightedMeanStudentBodyUtilisationRatio,
			weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
			weightOfMinimiseDispersionOfStudentFreePeriodsPerDay,
			weightOfMinimiseDispersionOfTeacherFreePeriodsPerDay,
			weightOfMinimiseDispersionOfTeacherWorkload,
			weightOfMinimiseMeanInterCampusMigrationsOfStudents,
			weightOfMinimiseMeanInterCampusMigrationsOfTeachers,
			weightOfMinimiseMeanLocationChangesOfTeachers,
			weightOfMinimiseMeanLocusOperandiOfTeachers,
			weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge,
			weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
			weightOfMinimiseMeanStudentBodyCombinationsPerLesson,
			weightOfMinimiseRatioOfConsecutiveEqualLessons,
			weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay
		) -- Deconstruct into a tuple.
	 ) $ HXT.xp18Tuple (
		xpickle' weightOfMaximiseComplianceWithFreePeriodPreferencesTag
	 ) (
		xpickle' weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacityTag
	 ) (
		xpickle' weightOfMaximiseMeanStudentClassSizeTag
	 ) (
		xpickle' weightOfMaximiseSynchronisationOfSynchronisedCoursesTag
	 ) (
		xpickle' weightOfMaximiseWeightedMeanStudentBodyUtilisationRatioTag
	 ) (
		xpickle' weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequestTag
	 ) (
		xpickle' weightOfMinimiseDispersionOfStudentFreePeriodsPerDayTag
	 ) (
		xpickle' weightOfMinimiseDispersionOfTeacherFreePeriodsPerDayTag
	 ) (
		xpickle' weightOfMinimiseDispersionOfTeacherWorkloadTag
	 ) (
		xpickle' weightOfMinimiseMeanInterCampusMigrationsOfStudentsTag
	 ) (
		xpickle' weightOfMinimiseMeanInterCampusMigrationsOfTeachersTag
	 ) (
		xpickle' weightOfMinimiseMeanLocationChangesOfTeachersTag
	 ) (
		xpickle' weightOfMinimiseMeanLocusOperandiOfTeachersTag
	 ) (
		xpickle' weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledgeTag
	 ) (
		xpickle' weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledgeTag
	 ) (
		xpickle' weightOfMinimiseMeanStudentBodyCombinationsPerLessonTag
	 ) (
		xpickle' weightOfMinimiseRatioOfConsecutiveEqualLessonsTag
	 ) (
		xpickle' weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDayTag
	 ) where
		xpickle'	= HXT.xpDefault Data.Default.def . (`HXT.xpAttr` HXT.xpickle)

instance (Eq criterionWeight, Num criterionWeight) => ExecutionConfiguration.CriterionWeight.CriterionWeights (TimetableCriteriaWeights criterionWeight) where
	areAllZero timetableCriteriaWeights	= ExecutionConfiguration.CriterionWeight.areAllZero $ map (($ timetableCriteriaWeights) . snd) associationList

instance Control.DeepSeq.NFData criterionWeight => Control.DeepSeq.NFData (TimetableCriteriaWeights criterionWeight) where
	rnf (MkTimetableCriteriaWeights x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17)	= Control.DeepSeq.rnf [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17]

{- |
	* Returns the weighted sum of the specified criteria, divided by the sum of the weights.

	* The magnitude of each /criterion/ should be in the same range, so that none dominates the total, thus making the total a clear measure of the value attributed to each.

	* The magnitude of the value of each /criterion/ should be independent of the dimensions of the /timetable/ (/observerId/s, /day/s per week, /time-slot/s per day),
	so that should any of these change, the balance between criteria doesn't shift; i.e. they should be dimensionless.
-}
calculateWeightedMean :: (
	Fractional	weightedMean,
	Real		criterionValue,
	Real		criterionWeight
 )
	=> TimetableCriteriaWeights criterionWeight
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /complianceWithFreePeriodPreferences/: maximum when each unallocated /time-slot/ complies with the preferences of both /student-bodies/ & /teacher/s.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /classSizeOverLocationCapacity/: maximum when /location/ which has been booked, is completely filled by the /student-class/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ A /timetable/ is preferred, when the mean over the size of /student-class/es, relative to the total number of /student/s, is greatest.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /synchronisationOfSynchronisedCourses/: maximum if the /lesson/s for all /synchronised course/s, are synchronised.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /weightedMeanStudentBodyUtilisationRatio/: maximum if all /student-bodies/ are fully utilised.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /averageAbsoluteDeviationFromIdealTimeslotRequest/: maximum if all /lesson/s, whose /course/ specifies an ideal /time-slot/, have been booked exactly on it, falling towards minimum as the mean absolute deviation increases.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /dispersionOfStudentFreePeriodsPerDay/: maximum when the deviation in the number of free /student-period/s per day is zero, falling towards minimum when they all fall on one /day/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /dispersionOfTeacherFreePeriodsPerDay/: maximum when the deviation in the number of free /teacher-period/s per day is zero, falling towards minimum when they all fall on one /day/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /dispersionOfTeacherWorkload/: maximum when the deviation in the /workload/ between /teacher/s is zero, falling towards minimum as their workloads move away from the mean.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /meanInterCampusMigrationsOfStudents/: maximum when each /student/ migrates to a different /campus/ between each /timeslot/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /meanInterCampusMigrationsOfTeachers/: maximum when each /teacher/ migrates to a different /campus/ between each /timeslot/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /meanLocationChangesOfTeachers/: maximum when the location of each /teacher/ changes at every /time-slot/, falling towards minimum as each /lesson/ is taught at the same /location/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /meanLocusOperandiOfTeachers/: maximum when the locus-operandi of each /teacher/ is just one location, falling towards minimum as each /lesson/ taught is in a different /location/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /meanRatioOfIncompletelyBookedCoreKnowledge/: maximum when all of a /student-body/'s required core /subject/s are completely booked, falling towards minimum when none are met.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /meanRatioOfIncompletelyBookedOptionalKnowledge/: maximum when all of a /student-body/'s required optional /subject/s are completely booked, falling towards minimum when none are met.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /meanStudentBodyCombinationsPerLesson/: maximum when each /student-body/ studies a /subject/, as a member of a constant /student-class/, falling towards minimum as the mean number of different /student-body/-combinations per /subject/ rises.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /ratioOfConsecutiveEqualLessons/: maximum when blocks of consecutive identical /lesson/s are booked, whose length equals that requested for the /course/ to which they belong.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /ratioOfSeparatedEqualLessonsWithinAnyDay/: maximum when only one contiguous block of /lesson/s in any /subject/, has been booked each /day/, falling towards minimum where separated blocks of equal /lesson/s occur within any one /day/.
	-> Control.Monad.Writer.Writer [Maybe criterionValue] weightedMean	-- ^ The individual /criteria/ values, & their /weighted mean/.
calculateWeightedMean MkTimetableCriteriaWeights {
	getWeightOfMaximiseComplianceWithFreePeriodPreferences			= weightOfMaximiseComplianceWithFreePeriodPreferences,
	getWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity	= weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity,
	getWeightOfMaximiseMeanStudentClassSize					= weightOfMaximiseMeanStudentClassSize,
	getWeightOfMaximiseSynchronisationOfSynchronisedCourses			= weightOfMaximiseSynchronisationOfSynchronisedCourses,
	getWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio		= weightOfMaximiseWeightedMeanStudentBodyUtilisationRatio,
	getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest	= weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
	getWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay			= weightOfMinimiseDispersionOfStudentFreePeriodsPerDay,
	getWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay			= weightOfMinimiseDispersionOfTeacherFreePeriodsPerDay,
	getWeightOfMinimiseDispersionOfTeacherWorkload				= weightOfMinimiseDispersionOfTeacherWorkload,
	getWeightOfMinimiseMeanInterCampusMigrationsOfStudents			= weightOfMinimiseMeanInterCampusMigrationsOfStudents,
	getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers			= weightOfMinimiseMeanInterCampusMigrationsOfTeachers,
	getWeightOfMinimiseMeanLocationChangesOfTeachers			= weightOfMinimiseMeanLocationChangesOfTeachers,
	getWeightOfMinimiseMeanLocusOperandiOfTeachers				= weightOfMinimiseMeanLocusOperandiOfTeachers,
	getWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge		= weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge,
	getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge	= weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
	getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson			= weightOfMinimiseMeanStudentBodyCombinationsPerLesson,
	getWeightOfMinimiseRatioOfConsecutiveEqualLessons			= weightOfMinimiseRatioOfConsecutiveEqualLessons,
	getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay		= weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay
} complianceWithFreePeriodPreferences meanRatioOfStudentClassSizeToLocationCapacity meanStudentClassSize synchronisationOfSynchronisedCourses weightedMeanStudentBodyUtilisationRatio averageAbsoluteDeviationFromIdealTimeslotRequest dispersionOfStudentFreePeriodsPerDay dispersionOfTeacherFreePeriodsPerDay dispersionOfTeacherWorkload meanInterCampusMigrationsOfStudents meanInterCampusMigrationsOfTeachers meanLocationChangesOfTeachers meanLocusOperandiOfTeachers meanRatioOfIncompletelyBookedCoreKnowledge meanRatioOfIncompletelyBookedOptionalKnowledge meanStudentBodyCombinationsPerLesson ratioOfConsecutiveEqualLessons ratioOfSeparatedEqualLessonsWithinAnyDay	= ExecutionConfiguration.Criterion.calculateWeightedMean [
	(complianceWithFreePeriodPreferences,			weightOfMaximiseComplianceWithFreePeriodPreferences),
	(meanRatioOfStudentClassSizeToLocationCapacity,		weightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity),
	(meanStudentClassSize,					weightOfMaximiseMeanStudentClassSize),
	(synchronisationOfSynchronisedCourses,			weightOfMaximiseSynchronisationOfSynchronisedCourses),
	(weightedMeanStudentBodyUtilisationRatio,		weightOfMaximiseWeightedMeanStudentBodyUtilisationRatio),
	(averageAbsoluteDeviationFromIdealTimeslotRequest,	weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest),
	(dispersionOfStudentFreePeriodsPerDay,			weightOfMinimiseDispersionOfStudentFreePeriodsPerDay),
	(dispersionOfTeacherFreePeriodsPerDay,			weightOfMinimiseDispersionOfTeacherFreePeriodsPerDay),
	(dispersionOfTeacherWorkload,				weightOfMinimiseDispersionOfTeacherWorkload),
	(meanInterCampusMigrationsOfStudents,			weightOfMinimiseMeanInterCampusMigrationsOfStudents),
	(meanInterCampusMigrationsOfTeachers,			weightOfMinimiseMeanInterCampusMigrationsOfTeachers),
	(meanLocationChangesOfTeachers,				weightOfMinimiseMeanLocationChangesOfTeachers),
	(meanLocusOperandiOfTeachers,				weightOfMinimiseMeanLocusOperandiOfTeachers),
	(meanRatioOfIncompletelyBookedCoreKnowledge,		weightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge),
	(meanRatioOfIncompletelyBookedOptionalKnowledge,	weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge),
	(meanStudentBodyCombinationsPerLesson,			weightOfMinimiseMeanStudentBodyCombinationsPerLesson),
	(ratioOfConsecutiveEqualLessons,			weightOfMinimiseRatioOfConsecutiveEqualLessons),
	(ratioOfSeparatedEqualLessonsWithinAnyDay,		weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay)
 ]

-- | The type of a function which mutates timetable-criteria weights.
type Mutator w	= TimetableCriteriaWeights w -> TimetableCriteriaWeights w

-- Mutator.
zeroWeightOfMaximiseComplianceWithFreePeriodPreferences :: Num w => Mutator w
zeroWeightOfMaximiseComplianceWithFreePeriodPreferences lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMaximiseComplianceWithFreePeriodPreferences	= minBound
}

-- Mutator.
zeroWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity :: Num w => Mutator w
zeroWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMaximiseMeanRatioOfStudentClassSizeToLocationCapacity	= minBound
}

-- Mutator.
zeroWeightOfMaximiseMeanStudentClassSize :: Num w => Mutator w
zeroWeightOfMaximiseMeanStudentClassSize lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMaximiseMeanStudentClassSize	= minBound
}

-- Mutator.
zeroWeightOfMaximiseSynchronisationOfSynchronisedCourses :: Num w => Mutator w
zeroWeightOfMaximiseSynchronisationOfSynchronisedCourses lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMaximiseSynchronisationOfSynchronisedCourses	= minBound
}

-- Mutator.
zeroWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio :: Num w => Mutator w
zeroWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMaximiseWeightedMeanStudentBodyUtilisationRatio	= minBound
}

-- Mutator.
zeroWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest :: Num w => Mutator w
zeroWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest	= minBound
}

-- Mutator.
zeroWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay :: Num w => Mutator w
zeroWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay	= minBound
}

-- Mutator.
zeroWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay :: Num w => Mutator w
zeroWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseDispersionOfTeacherFreePeriodsPerDay	= minBound
}

-- Mutator.
zeroWeightOfMinimiseDispersionOfTeacherWorkload :: Num w => Mutator w
zeroWeightOfMinimiseDispersionOfTeacherWorkload lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseDispersionOfTeacherWorkload	= minBound
}

-- Mutator.
zeroWeightOfMinimiseMeanInterCampusMigrationsOfStudents :: Num w => Mutator w
zeroWeightOfMinimiseMeanInterCampusMigrationsOfStudents lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseMeanInterCampusMigrationsOfStudents	= minBound
}

-- Mutator.
zeroWeightOfMinimiseMeanInterCampusMigrationsOfTeachers :: Num w => Mutator w
zeroWeightOfMinimiseMeanInterCampusMigrationsOfTeachers lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers	= minBound
}

-- Mutator.
zeroWeightOfMinimiseMeanLocationChangesOfTeachers :: Num w => Mutator w
zeroWeightOfMinimiseMeanLocationChangesOfTeachers lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseMeanLocationChangesOfTeachers	= minBound
}

-- Mutator.
zeroWeightOfMinimiseMeanLocusOperandiOfTeachers :: Num w => Mutator w
zeroWeightOfMinimiseMeanLocusOperandiOfTeachers lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseMeanLocusOperandiOfTeachers	= minBound
}

-- Mutator.
zeroWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge :: Num w => Mutator w
zeroWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseMeanRatioOfIncompletelyBookedCoreKnowledge	= minBound
}

-- Mutator.
zeroWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge :: Num w => Mutator w
zeroWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge	= minBound
}

-- Mutator.
zeroWeightOfMinimiseMeanStudentBodyCombinationsPerLesson :: Num w => Mutator w
zeroWeightOfMinimiseMeanStudentBodyCombinationsPerLesson lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson	= minBound
}

-- Mutator.
zeroWeightOfMinimiseRatioOfConsecutiveEqualLessons :: Num w => Mutator w
zeroWeightOfMinimiseRatioOfConsecutiveEqualLessons lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseRatioOfConsecutiveEqualLessons	= minBound
}

-- Mutator.
zeroWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay :: Num w => Mutator w
zeroWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay	= minBound
}

