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

	* Defines the weightings, associated with each of the /soft/-constraints, used to compare & select between alternative /lesson/-definitions,
	at each stage in the construction of a /timetable/.

	* /Hard/-constraints (e.g. that a /student/ or /teacher/ can't be in more than one place at a time,
	or that the capacity of a /location/ can't be exceeded),
	are never violated, & so don't need to be quantified for subsequent comparison.
-}

module WeekDaze.ExecutionConfiguration.LessonCriteriaWeights(
-- * Types
-- ** Type-synonyms
--	Mutator,
-- ** Data-types
	LessonCriteriaWeights(..),
-- * Constants
	associationList,
	tag,
	weightOfAreResourcesReusedTag,
	weightOfGreatestMinimumConsecutiveLessonsTag,
	weightOfGreatestRemainingCourseLessonsTag,
	weightOfGreatestSynchronisedCourseSetSizeTag,
	weightOfIsCoreKnowledgeRequirementTag,
	weightOfIsSpecialistInTopicTag,
	weightOfMatchCourseClassSizeToLocationCapacityTag,
	weightOfMaximiseRelativeFacilityUtilisationTag,
	weightOfMaximiseStudentClassSizeOverCourseClassSizeTag,
	weightOfMaximiseStudentClassSizeOverLocationCapacityTag,
	weightOfMinimiseBookingAtAnotherCoursesSpecifiedTimeTag,
	weightOfMinimiseBookingOfLocationByOtherTeachersTag,
	weightOfMinimiseDeviationFromTimeslotRequestTag,
	weightOfMinimiseInterCampusMigrationsOfStudentsTag,
	weightOfMinimiseInterCampusMigrationsOfTeachersTag,
	weightOfMinimiseStudentBodyCombinationsTag,
	weightOfMinimiseTeachersLocusOperandiTag,
	weightOfMinimiseWasteOfScarceFacilitiesTag,
-- * Functions
	calculateWeightedMean,
-- ** Mutators
--	normalise,
	perturbWeights,
--	zeroWeightOfAreResourcesReused,
	zeroWeightOfGreatestMinimumConsecutiveLessons,
--	zeroWeightOfGreatestRemainingCourseLessons,
	zeroWeightOfGreatestSynchronisedCourseSetSize,
	zeroWeightOfIsCoreKnowledgeRequirement,
	zeroWeightOfIsSpecialistInTopic,
	zeroWeightOfMatchCourseClassSizeToLocationCapacity,
	zeroWeightOfMaximiseRelativeFacilityUtilisation,
	zeroWeightOfMaximiseStudentClassSizeOverCourseClassSize,
	zeroWeightOfMaximiseStudentClassSizeOverLocationCapacity,
	zeroWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
	zeroWeightOfMinimiseBookingOfLocationByOtherTeachers,
	zeroWeightOfMinimiseDeviationFromTimeslotRequest,
	zeroWeightOfMinimiseInterCampusMigrationsOfStudents,
	zeroWeightOfMinimiseInterCampusMigrationsOfTeachers,
--	zeroWeightOfMinimiseStudentBodyCombinations,
	zeroWeightOfMinimiseTeachersLocusOperandi,
	zeroWeightOfMinimiseWasteOfScarceFacilities
) where

import qualified	Control.DeepSeq
import qualified	Control.Monad.Writer
import qualified	Data.Default
import qualified	System.Random
import qualified	Text.XML.HXT.Arrow.Pickle					as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.ExecutionConfiguration.Criterion			as ExecutionConfiguration.Criterion
import qualified	WeekDaze.ExecutionConfiguration.CriterionWeight			as ExecutionConfiguration.CriterionWeight
import qualified	WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights	as ExecutionConfiguration.OptimiseLessonCriteriaWeights

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Maybe
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector	as Database.Selector

instance (
	Data.Convertible.Convertible	Database.HDBC.SqlValue w,	-- Flexible context & Undecidable instance.
	Data.Typeable.Typeable		w,
	RealFrac			w
 ) => Database.Selector.Selector (LessonCriteriaWeights w) where
	fromDatabase connection	projectIdSql	= let
		tableName :: Database.Selector.TableName
		tableName	= Database.Selector.tablePrefix ++ tag
	 in do
		criterionWeightRows	<- map (
			map $ Data.Maybe.fromMaybe Data.Default.def . either (
				error . showString "WeekDaze.ExecutionConfiguration.LessonCriteriaWeights.fromDatabase:\tfailed to parse the value of a weight read from the database; " . show
			) id . Database.HDBC.safeFromSql
		 ) `fmap` Database.Selector.select connection (
			map fst associationList
		 ) [tableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		return {-to IO-Monad-} $ case criterionWeightRows of
			[]		-> Data.Default.def
			[weightRow]	-> case weightRow of
				[
					weightOfAreResourcesReused,
					weightOfGreatestMinimumConsecutiveLessons,
					weightOfGreatestRemainingCourseLessons,
					weightOfGreatestSynchronisedCourseSetSize,
					weightOfIsCoreKnowledgeRequirement,
					weightOfIsSpecialistInTopic,
					weightOfMatchCourseClassSizeToLocationCapacity,
					weightOfMaximiseRelativeFacilityUtilisation,
					weightOfMaximiseStudentClassSizeOverCourseClassSize,
					weightOfMaximiseStudentClassSizeOverLocationCapacity,
					weightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
					weightOfMinimiseBookingOfLocationByOtherTeachers,
					weightOfMinimiseDeviationFromTimeslotRequest,
					weightOfMinimiseInterCampusMigrationsOfStudents,
					weightOfMinimiseInterCampusMigrationsOfTeachers,
					weightOfMinimiseStudentBodyCombinations,
					weightOfMinimiseTeachersLocusOperandi,
					weightOfMinimiseWasteOfScarceFacilities
				 ] -> MkLessonCriteriaWeights {
					getWeightOfAreResourcesReused				= weightOfAreResourcesReused,
					getWeightOfGreatestMinimumConsecutiveLessons		= weightOfGreatestMinimumConsecutiveLessons,
					getWeightOfGreatestRemainingCourseLessons		= weightOfGreatestRemainingCourseLessons,
					getWeightOfGreatestSynchronisedCourseSetSize		= weightOfGreatestSynchronisedCourseSetSize,
					getWeightOfIsCoreKnowledgeRequirement			= weightOfIsCoreKnowledgeRequirement,
					getWeightOfIsSpecialistInTopic				= weightOfIsSpecialistInTopic,
					getWeightOfMatchCourseClassSizeToLocationCapacity	= weightOfMatchCourseClassSizeToLocationCapacity,
					getWeightOfMaximiseRelativeFacilityUtilisation		= weightOfMaximiseRelativeFacilityUtilisation,
					getWeightOfMaximiseStudentClassSizeOverCourseClassSize	= weightOfMaximiseStudentClassSizeOverCourseClassSize,
					getWeightOfMaximiseStudentClassSizeOverLocationCapacity	= weightOfMaximiseStudentClassSizeOverLocationCapacity,
					getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	= weightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
					getWeightOfMinimiseBookingOfLocationByOtherTeachers	= weightOfMinimiseBookingOfLocationByOtherTeachers,
					getWeightOfMinimiseDeviationFromTimeslotRequest		= weightOfMinimiseDeviationFromTimeslotRequest,
					getWeightOfMinimiseInterCampusMigrationsOfStudents	= weightOfMinimiseInterCampusMigrationsOfStudents,
					getWeightOfMinimiseInterCampusMigrationsOfTeachers	= weightOfMinimiseInterCampusMigrationsOfTeachers,
					getWeightOfMinimiseStudentBodyCombinations		= weightOfMinimiseStudentBodyCombinations,
					getWeightOfMinimiseTeachersLocusOperandi		= weightOfMinimiseTeachersLocusOperandi,
					getWeightOfMinimiseWasteOfScarceFacilities		= weightOfMinimiseWasteOfScarceFacilities
				}
				_ -> error $ "WeekDaze.ExecutionConfiguration.LessonCriteriaWeights.fromDatabase:\tunexpected number of columns=" ++ show (length weightRow) ++ " in row of table " ++ show tableName ++ "."
			_		-> error $ "WeekDaze.ExecutionConfiguration.LessonCriteriaWeights.fromDatabase:\tunexpected number of rows=" ++ show (length criterionWeightRows) ++ " selected from table " ++ show tableName ++ "."
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag							= "lessonCriteriaWeights"

-- | Used to qualify XML.
weightOfAreResourcesReusedTag :: String
weightOfAreResourcesReusedTag				= "areResourcesReused"

-- | Used to qualify XML.
weightOfGreatestMinimumConsecutiveLessonsTag :: String
weightOfGreatestMinimumConsecutiveLessonsTag		= "greatestMinimumConsecutiveLessons"

-- | Used to qualify XML.
weightOfGreatestRemainingCourseLessonsTag :: String
weightOfGreatestRemainingCourseLessonsTag		= "greatestRemainingCourseLessons"

-- | Used to qualify XML.
weightOfGreatestSynchronisedCourseSetSizeTag :: String
weightOfGreatestSynchronisedCourseSetSizeTag		= "greatestSynchronisedCourseSetSize"

-- | Used to qualify XML.
weightOfIsCoreKnowledgeRequirementTag :: String
weightOfIsCoreKnowledgeRequirementTag			= "isCoreKnowledgeRequirement"

-- | Used to qualify XML.
weightOfIsSpecialistInTopicTag :: String
weightOfIsSpecialistInTopicTag				= "isSpecialistInTopic"

-- | Used to qualify XML.
weightOfMatchCourseClassSizeToLocationCapacityTag :: String
weightOfMatchCourseClassSizeToLocationCapacityTag	= "matchCourseClassSizeToLocationCapacity"

-- | Used to qualify XML.
weightOfMaximiseRelativeFacilityUtilisationTag :: String
weightOfMaximiseRelativeFacilityUtilisationTag		= "maximiseRelativeFacilityUtilisation"

-- | Used to qualify XML.
weightOfMaximiseStudentClassSizeOverCourseClassSizeTag :: String
weightOfMaximiseStudentClassSizeOverCourseClassSizeTag	= "maximiseStudentClassSizeOverCourseClassSize"

-- | Used to qualify XML.
weightOfMaximiseStudentClassSizeOverLocationCapacityTag :: String
weightOfMaximiseStudentClassSizeOverLocationCapacityTag	= "maximiseStudentClassSizeOverLocationCapacity"

-- | Used to qualify XML.
weightOfMinimiseBookingAtAnotherCoursesSpecifiedTimeTag :: String
weightOfMinimiseBookingAtAnotherCoursesSpecifiedTimeTag	= "minimiseBookingAtAnotherCoursesSpecifiedTime"

-- | Used to qualify XML.
weightOfMinimiseBookingOfLocationByOtherTeachersTag :: String
weightOfMinimiseBookingOfLocationByOtherTeachersTag	= "minimiseBookingOfLocationByOtherTeachers"

-- | Used to qualify XML.
weightOfMinimiseDeviationFromTimeslotRequestTag :: String
weightOfMinimiseDeviationFromTimeslotRequestTag		= "minimiseDeviationFromTimeslotRequest"

-- | Used to qualify XML.
weightOfMinimiseInterCampusMigrationsOfStudentsTag :: String
weightOfMinimiseInterCampusMigrationsOfStudentsTag	= "minimiseInterCampusMigrationsOfStudents"

-- | Used to qualify XML.
weightOfMinimiseInterCampusMigrationsOfTeachersTag :: String
weightOfMinimiseInterCampusMigrationsOfTeachersTag	= "minimiseInterCampusMigrationsOfTeachers"

-- | Used to qualify XML.
weightOfMinimiseStudentBodyCombinationsTag :: String
weightOfMinimiseStudentBodyCombinationsTag		= "minimiseStudentBodyCombinations"

-- | Used to qualify XML.
weightOfMinimiseTeachersLocusOperandiTag :: String
weightOfMinimiseTeachersLocusOperandiTag		= "minimiseTeachersLocusOperandi"

-- | Used to qualify XML.
weightOfMinimiseWasteOfScarceFacilitiesTag :: String
weightOfMinimiseWasteOfScarceFacilitiesTag		= "minimiseWasteOfScarceFacilities"

-- | The ordered list of tags & accessors.
associationList :: [(String, LessonCriteriaWeights w -> ExecutionConfiguration.CriterionWeight.CriterionWeight w)]
associationList	= [
	(weightOfAreResourcesReusedTag,					getWeightOfAreResourcesReused),
	(weightOfGreatestMinimumConsecutiveLessonsTag,			getWeightOfGreatestMinimumConsecutiveLessons),
	(weightOfGreatestRemainingCourseLessonsTag,			getWeightOfGreatestRemainingCourseLessons),
	(weightOfGreatestSynchronisedCourseSetSizeTag,			getWeightOfGreatestSynchronisedCourseSetSize),
	(weightOfIsCoreKnowledgeRequirementTag,				getWeightOfIsCoreKnowledgeRequirement),
	(weightOfIsSpecialistInTopicTag,				getWeightOfIsSpecialistInTopic),
	(weightOfMatchCourseClassSizeToLocationCapacityTag,		getWeightOfMatchCourseClassSizeToLocationCapacity),
	(weightOfMaximiseRelativeFacilityUtilisationTag,		getWeightOfMaximiseRelativeFacilityUtilisation),
	(weightOfMaximiseStudentClassSizeOverCourseClassSizeTag,	getWeightOfMaximiseStudentClassSizeOverCourseClassSize),
	(weightOfMaximiseStudentClassSizeOverLocationCapacityTag,	getWeightOfMaximiseStudentClassSizeOverLocationCapacity),
	(weightOfMinimiseBookingAtAnotherCoursesSpecifiedTimeTag,	getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime),
	(weightOfMinimiseBookingOfLocationByOtherTeachersTag,		getWeightOfMinimiseBookingOfLocationByOtherTeachers),
	(weightOfMinimiseDeviationFromTimeslotRequestTag,		getWeightOfMinimiseDeviationFromTimeslotRequest),
	(weightOfMinimiseInterCampusMigrationsOfStudentsTag,		getWeightOfMinimiseInterCampusMigrationsOfStudents),
	(weightOfMinimiseInterCampusMigrationsOfTeachersTag,		getWeightOfMinimiseInterCampusMigrationsOfTeachers),
	(weightOfMinimiseStudentBodyCombinationsTag,			getWeightOfMinimiseStudentBodyCombinations),
	(weightOfMinimiseTeachersLocusOperandiTag,			getWeightOfMinimiseTeachersLocusOperandi),
	(weightOfMinimiseWasteOfScarceFacilitiesTag,			getWeightOfMinimiseWasteOfScarceFacilities)
 ]

{- |
	* The weight of various criteria used to select a /lesson/ from alternatives, at specific coordinates in the /timetable/.

	* These criteria relate only to the attributes of the /lesson/ rather than its coordinates in the /timetable/; since the latter is common to all alternatives.
-}
data LessonCriteriaWeights w	= MkLessonCriteriaWeights {
	getWeightOfAreResourcesReused				:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /lesson/ is preferred, if the required /resource/s (/location/s & /teacher/s) are shared with other /student-bodies/, by merging them into a single larger /student-class/.
	getWeightOfGreatestMinimumConsecutiveLessons		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /lesson/ is preferred, if the /course/ to which it belongs, has a greater /minimum consecutive lessons/; since this makes it harder to book later, into the smaller spaces remaining.
	getWeightOfGreatestRemainingCourseLessons		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /course/ is preferred, if it has greater unbooked /lesson/s, since a /course/ requiring only one /lesson/ can be booked on any /day/.
	getWeightOfGreatestSynchronisedCourseSetSize		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /course/ is preferred, if it is a member of a larger set of /synchronised course/s; it seems prudent to book the most constrained /course/'s /lesson/s, early.
	getWeightOfIsCoreKnowledgeRequirement			:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /subject/ is preferred, if it is categorised by the /student/, as a /core knowledge-requirement/.
	getWeightOfIsSpecialistInTopic				:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /teacher/ is preferred, if their /specialtyTopic/ is the proposed /topic/.
	getWeightOfMatchCourseClassSizeToLocationCapacity	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /location/ is preferred, when its /capacity/ matches the /course/'s optional /maximum class-size/.
	getWeightOfMaximiseRelativeFacilityUtilisation		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /location/ is preferred, the more its /facilities/ are used; cf. 'getWeightOfMinimiseWasteOfScarceFacilities', which tries to make a distinction based on the type of the wasted /facility/.
	getWeightOfMaximiseStudentClassSizeOverCourseClassSize	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /course/ is preferred, the more any /maximum class-size/ is filled by the proposed /student-class/. CAVEAT: this can change retrospectively given 'ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger'.
	getWeightOfMaximiseStudentClassSizeOverLocationCapacity	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /location/ is preferred, the more its /capacity/ is filled by the proposed /student-class/. CAVEAT: this can change retrospectively given 'ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger'.
	getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A booking-/time/ is preferred, if there's minimal probability that /course/s for other the /knowledge-requirement/s of this /student-body/ & any studying /synchronised courses/, will specifically request that /time/; the probability becomes a certainty for those /knowledge-requirement/s which can only be satisfied by one /course/, allowing corresponding /lesson/s to be filtered from all candidate-lessons, before this evaluation.
	getWeightOfMinimiseBookingOfLocationByOtherTeachers	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /location/ is preferred, when booked by the fewest other /teacher/s.
	getWeightOfMinimiseDeviationFromTimeslotRequest		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /course/ is preferred, if it is booked closer to any /timeslot-request/.
	getWeightOfMinimiseInterCampusMigrationsOfStudents	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /course/ is preferred, if inter-/campus/ migrations of /student/s are minimised.
	getWeightOfMinimiseInterCampusMigrationsOfTeachers	:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /course/ is preferred, if inter-/campus/ migrations of /teacher/s are minimised.
	getWeightOfMinimiseStudentBodyCombinations		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /lesson/ is preferred, when the number of /student-body combination/s is minimised. CAVEAT: potential conflict with 'getWeightOfMaximiseStudentClassSizeOverLocationCapacity' & 'getWeightOfMaximiseStudentClassSizeOverCourseClassSize'.
	getWeightOfMinimiseTeachersLocusOperandi		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w,	-- ^ A /location/ is preferred, if the size of /teacher/'s locus-operandi is minimised; based merely on the number of distinct /location/s booked, rather than the distance between them.
	getWeightOfMinimiseWasteOfScarceFacilities		:: ExecutionConfiguration.CriterionWeight.CriterionWeight w	-- ^ A /lesson/ is preferred, if the /location/'s unused /facilities/, are commonly available elsewhere; cf. 'getWeightOfMaximiseRelativeFacilityUtilisation', which treats all /facilities/ equally.
} deriving (Eq, Show)

instance (Eq w, Num w) => ToolShed.SelfValidate.SelfValidator (LessonCriteriaWeights w) where
	getErrors lessonCriteriaWeights	= ToolShed.SelfValidate.extractErrors [
		(
			ExecutionConfiguration.CriterionWeight.areAllZero lessonCriteriaWeights,
			"The weight of @ least one lesson-criterion must be non-zero"
		) -- Pair.
	 ]

instance Num w => Data.Default.Default (LessonCriteriaWeights w) where
	def = MkLessonCriteriaWeights {
		getWeightOfAreResourcesReused				= Data.Default.def,
		getWeightOfGreatestMinimumConsecutiveLessons		= Data.Default.def,
		getWeightOfGreatestRemainingCourseLessons		= Data.Default.def,
		getWeightOfGreatestSynchronisedCourseSetSize		= Data.Default.def,
		getWeightOfIsCoreKnowledgeRequirement			= Data.Default.def,
		getWeightOfIsSpecialistInTopic				= Data.Default.def,
		getWeightOfMatchCourseClassSizeToLocationCapacity	= Data.Default.def,
		getWeightOfMaximiseRelativeFacilityUtilisation		= Data.Default.def,
		getWeightOfMaximiseStudentClassSizeOverCourseClassSize	= Data.Default.def,
		getWeightOfMaximiseStudentClassSizeOverLocationCapacity	= Data.Default.def,
		getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	= Data.Default.def,
		getWeightOfMinimiseBookingOfLocationByOtherTeachers	= Data.Default.def,
		getWeightOfMinimiseDeviationFromTimeslotRequest		= Data.Default.def,
		getWeightOfMinimiseInterCampusMigrationsOfStudents	= Data.Default.def,
		getWeightOfMinimiseInterCampusMigrationsOfTeachers	= Data.Default.def,
		getWeightOfMinimiseStudentBodyCombinations		= Data.Default.def,
		getWeightOfMinimiseTeachersLocusOperandi		= Data.Default.def,
		getWeightOfMinimiseWasteOfScarceFacilities		= Data.Default.def
	}

instance (HXT.XmlPickler w, Ord w, Real w) => HXT.XmlPickler (LessonCriteriaWeights w) where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag $ HXT.xpWrap (
		\(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)	-> MkLessonCriteriaWeights a b c d e f g h i j k l m n o p q r,	-- Construct from a tuple.
		\MkLessonCriteriaWeights {
			getWeightOfAreResourcesReused				= weightOfAreResourcesReused,
			getWeightOfGreatestMinimumConsecutiveLessons		= weightOfGreatestMinimumConsecutiveLessons,
			getWeightOfGreatestRemainingCourseLessons		= weightOfGreatestRemainingCourseLessons,
			getWeightOfGreatestSynchronisedCourseSetSize		= weightOfGreatestSynchronisedCourseSetSize,
			getWeightOfIsCoreKnowledgeRequirement			= weightOfIsCoreKnowledgeRequirement,
			getWeightOfIsSpecialistInTopic				= weightOfIsSpecialistInTopic,
			getWeightOfMatchCourseClassSizeToLocationCapacity	= weightOfMatchCourseClassSizeToLocationCapacity,
			getWeightOfMaximiseRelativeFacilityUtilisation		= weightOfMaximiseRelativeFacilityUtilisation,
			getWeightOfMaximiseStudentClassSizeOverCourseClassSize	= weightOfMaximiseStudentClassSizeOverCourseClassSize,
			getWeightOfMaximiseStudentClassSizeOverLocationCapacity	= weightOfMaximiseStudentClassSizeOverLocationCapacity,
			getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	= weightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
			getWeightOfMinimiseBookingOfLocationByOtherTeachers	= weightOfMinimiseBookingOfLocationByOtherTeachers,
			getWeightOfMinimiseDeviationFromTimeslotRequest		= weightOfMinimiseDeviationFromTimeslotRequest,
			getWeightOfMinimiseInterCampusMigrationsOfStudents	= weightOfMinimiseInterCampusMigrationsOfStudents,
			getWeightOfMinimiseInterCampusMigrationsOfTeachers	= weightOfMinimiseInterCampusMigrationsOfTeachers,
			getWeightOfMinimiseStudentBodyCombinations		= weightOfMinimiseStudentBodyCombinations,
			getWeightOfMinimiseTeachersLocusOperandi		= weightOfMinimiseTeachersLocusOperandi,
			getWeightOfMinimiseWasteOfScarceFacilities		= weightOfMinimiseWasteOfScarceFacilities
		} -> (
			weightOfAreResourcesReused,
			weightOfGreatestMinimumConsecutiveLessons,
			weightOfGreatestRemainingCourseLessons,
			weightOfGreatestSynchronisedCourseSetSize,
			weightOfIsCoreKnowledgeRequirement,
			weightOfIsSpecialistInTopic,
			weightOfMatchCourseClassSizeToLocationCapacity,
			weightOfMaximiseRelativeFacilityUtilisation,
			weightOfMaximiseStudentClassSizeOverCourseClassSize,
			weightOfMaximiseStudentClassSizeOverLocationCapacity,
			weightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
			weightOfMinimiseBookingOfLocationByOtherTeachers,
			weightOfMinimiseDeviationFromTimeslotRequest,
			weightOfMinimiseInterCampusMigrationsOfStudents,
			weightOfMinimiseInterCampusMigrationsOfTeachers,
			weightOfMinimiseStudentBodyCombinations,
			weightOfMinimiseTeachersLocusOperandi,
			weightOfMinimiseWasteOfScarceFacilities
		) -- Deconstruct into a tuple.
	 ) $ HXT.xp18Tuple (
		xpickle' weightOfAreResourcesReusedTag
	 ) (
		xpickle' weightOfGreatestMinimumConsecutiveLessonsTag
	 ) (
		xpickle' weightOfGreatestRemainingCourseLessonsTag
	 ) (
		xpickle' weightOfGreatestSynchronisedCourseSetSizeTag
	 ) (
		xpickle' weightOfIsCoreKnowledgeRequirementTag
	 ) (
		xpickle' weightOfIsSpecialistInTopicTag
	 ) (
		xpickle' weightOfMatchCourseClassSizeToLocationCapacityTag
	 ) (
		xpickle' weightOfMaximiseRelativeFacilityUtilisationTag
	 ) (
		xpickle' weightOfMaximiseStudentClassSizeOverCourseClassSizeTag
	 ) (
		xpickle' weightOfMaximiseStudentClassSizeOverLocationCapacityTag
	 ) (
		xpickle' weightOfMinimiseBookingAtAnotherCoursesSpecifiedTimeTag
	 ) (
		xpickle' weightOfMinimiseBookingOfLocationByOtherTeachersTag
	 ) (
		xpickle' weightOfMinimiseDeviationFromTimeslotRequestTag
	 ) (
		xpickle' weightOfMinimiseInterCampusMigrationsOfStudentsTag
	 ) (
		xpickle' weightOfMinimiseInterCampusMigrationsOfTeachersTag
	 ) (
		xpickle' weightOfMinimiseStudentBodyCombinationsTag
	 ) (
		xpickle' weightOfMinimiseTeachersLocusOperandiTag
	 ) (
		xpickle' weightOfMinimiseWasteOfScarceFacilitiesTag
	 ) where
		xpickle'	= HXT.xpDefault Data.Default.def . (`HXT.xpAttr` HXT.xpickle)

instance (Eq criterionWeight, Num criterionWeight) => ExecutionConfiguration.CriterionWeight.CriterionWeights (LessonCriteriaWeights criterionWeight) where
	areAllZero lessonCriteriaWeights	= ExecutionConfiguration.CriterionWeight.areAllZero $ map (($ lessonCriteriaWeights) . snd) associationList

instance Control.DeepSeq.NFData criterionWeight => Control.DeepSeq.NFData (LessonCriteriaWeights criterionWeight) where
	rnf (MkLessonCriteriaWeights x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17)	= Control.DeepSeq.rnf [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17]

{- |
	* Returns the weighted sum of the specified criteria, divided by the sum of the weights.

	* Each criterion increases in proportion to some desirable attribute of the proposed /lesson/.

	* Each criterion should be in the same range of magnitudes, so that none dominates the total, thus making the total a clear measure of the value attributed to each.

	* The magnitude of the value of each criterion should be independent of the dimensions of the problem, such that the balance isn't disturbed when it's changed;
	i.e. they should be dimensionless.

	* /Lesson/s lacking the concept being measured shouldn't be disadvantaged, but assigned a constant median magnitude.
-}
calculateWeightedMean :: (
	Fractional	weightedMean,
	Real		criterionValue,
	Real		criterionWeight
 )
	=> LessonCriteriaWeights criterionWeight
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /areResourcesReused/:	Maximum if the proposed /lesson/ already exists at the same time in other /student-body/ timetable, & therefore allows resource-reuse, by merging them into a /student-class/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /minimumConsecutiveLessons/:	Maximum when the proposed /lesson/, belongs to a /course/ which specifies the greatest possible /minimum consecutive lessons/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /remainingCourseLessons/:	Maximum when the number of /lesson/s required for the /course/ matches the number of days the /student/ & /teacher/ are mutually available, falling to minimum when there are fewer remaining /lesson/s.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /synchronisedCourseSetSize/:	Maximum if the /course/ is a member of the largest possible set of synchronised /course/s.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /isCoreKnowledgeRequirement/:	Maximum if the /subject/ is categorised as a /core/ knowledge-requirement for the /student/, otherwise minimum.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /isSpecialistInTopic/:	Maximum if the /teacher/ is a specialist in a given /topic/, otherwise minimum.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /matchOfCourseClassSizeToLocationCapacity/:	Maximum when the maximum class-size specified for the /course/ matches the capacity of the /location/, falling to minimum as the deviation increases.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /relativeFacilityUtilisation/:	The number of /facilities/ used (regardless of their nature), over the total number of /facilities/ available at the /location/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /studentClassSizeOverCourseClassSize/:	The number of /student/s in the class, over the /course/'s maximum class-size; this makes little sense if the class-size can grow.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /studentClassSizeOverLocationCapacity/:	The number of /student/s in the class, over the /location/'s capacity; this makes little sense if the class-size can grow.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /bookingAtAnotherCoursesSpecifiedTime/:	Maximum when if there's zero probability that /course/s for this /student-body/'s other /knowledge-requirements/, will specify the proposed /booking-time/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /bookingOfLocationByOtherTeachers/:	Maximum when the /location/ has been booked by zero other /teacher/s, tending to minimum as each additional /teacher/ books a /lesson/ in this /location/.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /deviationFromTimeslotRequest/:	Maximum when the proposed booking-time matches a /timeslot-request/ for the /course/, falling to minimum as the deviation rises.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /interCampusMigrationsOfStudents/:	Maximum when the /campus/ for the proposed /booking/ matches that in which the /student/ is located in adjacent /time-slot/s.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /interCampusMigrationsOfTeachers/:	Maximum when the /campus/ for the proposed /booking/ matches that in which the /teacher/ is located in adjacent /time-slot/s.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /studentBodyCombinations/:	Maximum if there's no increase in the number of /student-body/-combinations for the proposed /lesson/, falling towards minimum with each additional /student-body/-combination.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /teachersLocusOperandi/:	Maximum if either the /teacher/ has no previous bookings, or their locus operandi is unchanged, falling towards minimum with large relative increases to the locus operandi.
	-> ExecutionConfiguration.Criterion.Criterion criterionValue		-- ^ /wasteOfScarceFacilities/:	Maximum if the proposed /lesson/ either doesn't waste any /facilities/, or failing that merely wastes those which are ubiquitous.
	-> Control.Monad.Writer.Writer [Maybe criterionValue] weightedMean	-- ^ The individual /criteria/ values, & their /weighted mean/.
calculateWeightedMean MkLessonCriteriaWeights {
	getWeightOfAreResourcesReused				= weightOfAreResourcesReused,
	getWeightOfGreatestMinimumConsecutiveLessons		= weightOfGreatestMinimumConsecutiveLessons,
	getWeightOfGreatestRemainingCourseLessons		= weightOfGreatestRemainingCourseLessons,
	getWeightOfGreatestSynchronisedCourseSetSize		= weightOfGreatestSynchronisedCourseSetSize,
	getWeightOfIsCoreKnowledgeRequirement			= weightOfIsCoreKnowledgeRequirement,
	getWeightOfIsSpecialistInTopic				= weightOfIsSpecialistInTopic,
	getWeightOfMatchCourseClassSizeToLocationCapacity	= weightOfMatchCourseClassSizeToLocationCapacity,
	getWeightOfMaximiseRelativeFacilityUtilisation		= weightOfMaximiseRelativeFacilityUtilisation,
	getWeightOfMaximiseStudentClassSizeOverCourseClassSize	= weightOfMaximiseStudentClassSizeOverCourseClassSize,
	getWeightOfMaximiseStudentClassSizeOverLocationCapacity	= weightOfMaximiseStudentClassSizeOverLocationCapacity,
	getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	= weightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
	getWeightOfMinimiseBookingOfLocationByOtherTeachers	= weightOfMinimiseBookingOfLocationByOtherTeachers,
	getWeightOfMinimiseDeviationFromTimeslotRequest		= weightOfMinimiseDeviationFromTimeslotRequest,
	getWeightOfMinimiseInterCampusMigrationsOfStudents	= weightOfMinimiseInterCampusMigrationsOfStudents,
	getWeightOfMinimiseInterCampusMigrationsOfTeachers	= weightOfMinimiseInterCampusMigrationsOfTeachers,
	getWeightOfMinimiseStudentBodyCombinations		= weightOfMinimiseStudentBodyCombinations,
	getWeightOfMinimiseTeachersLocusOperandi		= weightOfMinimiseTeachersLocusOperandi,
	getWeightOfMinimiseWasteOfScarceFacilities		= weightOfMinimiseWasteOfScarceFacilities
} areResourcesReused minimumConsecutiveLessons remainingCourseLessons synchronisedCourseSetSize isCoreKnowledgeRequirement isSpecialistInTopic matchOfCourseClassSizeToLocationCapacity relativeFacilityUtilisation studentClassSizeOverCourseClassSize studentClassSizeOverLocationCapacity bookingAtAnotherCoursesSpecifiedTime bookingOfLocationByOtherTeachers deviationFromTimeslotRequest interCampusMigrationsOfStudents interCampusMigrationsOfTeachers studentBodyCombinations teachersLocusOperandi wasteOfScarceFacilities = ExecutionConfiguration.Criterion.calculateWeightedMean [
	(areResourcesReused,				weightOfAreResourcesReused),
	(minimumConsecutiveLessons,			weightOfGreatestMinimumConsecutiveLessons),
	(remainingCourseLessons,			weightOfGreatestRemainingCourseLessons),
	(synchronisedCourseSetSize,			weightOfGreatestSynchronisedCourseSetSize),
	(isCoreKnowledgeRequirement,			weightOfIsCoreKnowledgeRequirement),
	(isSpecialistInTopic,				weightOfIsSpecialistInTopic),
	(matchOfCourseClassSizeToLocationCapacity,	weightOfMatchCourseClassSizeToLocationCapacity),
	(relativeFacilityUtilisation,			weightOfMaximiseRelativeFacilityUtilisation),
	(studentClassSizeOverCourseClassSize,		weightOfMaximiseStudentClassSizeOverCourseClassSize),
	(studentClassSizeOverLocationCapacity,		weightOfMaximiseStudentClassSizeOverLocationCapacity),
	(bookingAtAnotherCoursesSpecifiedTime,		weightOfMinimiseBookingAtAnotherCoursesSpecifiedTime),
	(bookingOfLocationByOtherTeachers,		weightOfMinimiseBookingOfLocationByOtherTeachers),
	(deviationFromTimeslotRequest,			weightOfMinimiseDeviationFromTimeslotRequest),
	(interCampusMigrationsOfStudents,		weightOfMinimiseInterCampusMigrationsOfStudents),
	(interCampusMigrationsOfTeachers,		weightOfMinimiseInterCampusMigrationsOfTeachers),
	(studentBodyCombinations,			weightOfMinimiseStudentBodyCombinations),
	(teachersLocusOperandi,				weightOfMinimiseTeachersLocusOperandi),
	(wasteOfScarceFacilities,			weightOfMinimiseWasteOfScarceFacilities)
 ]

-- | The type of a function which mutates lesson-criteria weights.
type Mutator w	= LessonCriteriaWeights w -> LessonCriteriaWeights w

-- Mutator.
zeroWeightOfAreResourcesReused :: Num w => Mutator w
zeroWeightOfAreResourcesReused lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfAreResourcesReused	= minBound
}

-- Mutator.
zeroWeightOfGreatestMinimumConsecutiveLessons :: Num w => Mutator w
zeroWeightOfGreatestMinimumConsecutiveLessons lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfGreatestMinimumConsecutiveLessons	= minBound
}

-- Mutator.
zeroWeightOfGreatestRemainingCourseLessons :: Num w => Mutator w
zeroWeightOfGreatestRemainingCourseLessons lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfGreatestRemainingCourseLessons	= minBound
}

-- Mutator.
zeroWeightOfGreatestSynchronisedCourseSetSize :: Num w => Mutator w
zeroWeightOfGreatestSynchronisedCourseSetSize lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfGreatestSynchronisedCourseSetSize	= minBound
}

-- Mutator.
zeroWeightOfIsCoreKnowledgeRequirement :: Num w => Mutator w
zeroWeightOfIsCoreKnowledgeRequirement lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfIsCoreKnowledgeRequirement	= minBound
}

-- Mutator.
zeroWeightOfIsSpecialistInTopic :: Num w => Mutator w
zeroWeightOfIsSpecialistInTopic lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfIsSpecialistInTopic	= minBound
}

-- Mutator.
zeroWeightOfMatchCourseClassSizeToLocationCapacity :: Num w => Mutator w
zeroWeightOfMatchCourseClassSizeToLocationCapacity lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMatchCourseClassSizeToLocationCapacity	= minBound
}

-- Mutator.
zeroWeightOfMaximiseRelativeFacilityUtilisation :: Num w => Mutator w
zeroWeightOfMaximiseRelativeFacilityUtilisation lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMaximiseRelativeFacilityUtilisation	= minBound
}

-- Mutator.
zeroWeightOfMaximiseStudentClassSizeOverCourseClassSize :: Num w => Mutator w
zeroWeightOfMaximiseStudentClassSizeOverCourseClassSize lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMaximiseStudentClassSizeOverCourseClassSize	= minBound
}

-- Mutator.
zeroWeightOfMaximiseStudentClassSizeOverLocationCapacity :: Num w => Mutator w
zeroWeightOfMaximiseStudentClassSizeOverLocationCapacity lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMaximiseStudentClassSizeOverLocationCapacity	= minBound
}

-- Mutator.
zeroWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime :: Num w => Mutator w
zeroWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	= minBound
}

-- Mutator.
zeroWeightOfMinimiseBookingOfLocationByOtherTeachers :: Num w => Mutator w
zeroWeightOfMinimiseBookingOfLocationByOtherTeachers lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseBookingOfLocationByOtherTeachers	= minBound
}

-- Mutator.
zeroWeightOfMinimiseDeviationFromTimeslotRequest :: Num w => Mutator w
zeroWeightOfMinimiseDeviationFromTimeslotRequest lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseDeviationFromTimeslotRequest	= minBound
}

-- Mutator.
zeroWeightOfMinimiseInterCampusMigrationsOfStudents :: Num w => Mutator w
zeroWeightOfMinimiseInterCampusMigrationsOfStudents lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseInterCampusMigrationsOfStudents	= minBound
}

-- Mutator.
zeroWeightOfMinimiseInterCampusMigrationsOfTeachers :: Num w => Mutator w
zeroWeightOfMinimiseInterCampusMigrationsOfTeachers lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseInterCampusMigrationsOfTeachers	= minBound
}

-- Mutator.
zeroWeightOfMinimiseStudentBodyCombinations :: Num w => Mutator w
zeroWeightOfMinimiseStudentBodyCombinations lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseStudentBodyCombinations	= minBound
}

-- Mutator.
zeroWeightOfMinimiseTeachersLocusOperandi :: Num w => Mutator w
zeroWeightOfMinimiseTeachersLocusOperandi lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseTeachersLocusOperandi	= minBound
}

-- Mutator.
zeroWeightOfMinimiseWasteOfScarceFacilities :: Num w => Mutator w
zeroWeightOfMinimiseWasteOfScarceFacilities lessonCriteriaWeights	= lessonCriteriaWeights {
	getWeightOfMinimiseWasteOfScarceFacilities	= minBound
}

-- | Adjust the mean weight, so that the maximum weight is '1'.
normalise :: (
	Fractional	w,
	Ord		w,
	Real		w
 ) => Mutator w
normalise lessonCriteriaWeights
	| ExecutionConfiguration.CriterionWeight.areAllZero lessonCriteriaWeights	= lessonCriteriaWeights	-- CAVEAT: otherwise divide-by-zero.
	| otherwise									= MkLessonCriteriaWeights {
		getWeightOfAreResourcesReused				= normaliseCriterionWeight $ getWeightOfAreResourcesReused lessonCriteriaWeights,
		getWeightOfGreatestMinimumConsecutiveLessons		= normaliseCriterionWeight $ getWeightOfGreatestMinimumConsecutiveLessons lessonCriteriaWeights,
		getWeightOfGreatestRemainingCourseLessons		= normaliseCriterionWeight $ getWeightOfGreatestRemainingCourseLessons lessonCriteriaWeights,
		getWeightOfGreatestSynchronisedCourseSetSize		= normaliseCriterionWeight $ getWeightOfGreatestSynchronisedCourseSetSize lessonCriteriaWeights,
		getWeightOfIsCoreKnowledgeRequirement			= normaliseCriterionWeight $ getWeightOfIsCoreKnowledgeRequirement lessonCriteriaWeights,
		getWeightOfIsSpecialistInTopic				= normaliseCriterionWeight $ getWeightOfIsSpecialistInTopic lessonCriteriaWeights,
		getWeightOfMatchCourseClassSizeToLocationCapacity	= normaliseCriterionWeight $ getWeightOfMatchCourseClassSizeToLocationCapacity lessonCriteriaWeights,
		getWeightOfMaximiseRelativeFacilityUtilisation		= normaliseCriterionWeight $ getWeightOfMaximiseRelativeFacilityUtilisation lessonCriteriaWeights,
		getWeightOfMaximiseStudentClassSizeOverCourseClassSize	= normaliseCriterionWeight $ getWeightOfMaximiseStudentClassSizeOverCourseClassSize lessonCriteriaWeights,
		getWeightOfMaximiseStudentClassSizeOverLocationCapacity	= normaliseCriterionWeight $ getWeightOfMaximiseStudentClassSizeOverLocationCapacity lessonCriteriaWeights,
		getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	= normaliseCriterionWeight $ getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime lessonCriteriaWeights,
		getWeightOfMinimiseBookingOfLocationByOtherTeachers	= normaliseCriterionWeight $ getWeightOfMinimiseBookingOfLocationByOtherTeachers lessonCriteriaWeights,
		getWeightOfMinimiseDeviationFromTimeslotRequest		= normaliseCriterionWeight $ getWeightOfMinimiseDeviationFromTimeslotRequest lessonCriteriaWeights,
		getWeightOfMinimiseInterCampusMigrationsOfStudents	= normaliseCriterionWeight $ getWeightOfMinimiseInterCampusMigrationsOfStudents lessonCriteriaWeights,
		getWeightOfMinimiseInterCampusMigrationsOfTeachers	= normaliseCriterionWeight $ getWeightOfMinimiseInterCampusMigrationsOfTeachers lessonCriteriaWeights,
		getWeightOfMinimiseStudentBodyCombinations		= normaliseCriterionWeight $ getWeightOfMinimiseStudentBodyCombinations lessonCriteriaWeights,
		getWeightOfMinimiseTeachersLocusOperandi		= normaliseCriterionWeight $ getWeightOfMinimiseTeachersLocusOperandi lessonCriteriaWeights,
		getWeightOfMinimiseWasteOfScarceFacilities		= normaliseCriterionWeight $ getWeightOfMinimiseWasteOfScarceFacilities lessonCriteriaWeights
	}
	where
		normaliseCriterionWeight	= ExecutionConfiguration.CriterionWeight.mkCriterionWeight . (
			/ ExecutionConfiguration.CriterionWeight.deconstruct (
				maximum $ map (($ lessonCriteriaWeights) . snd {-accessor-}) associationList
			)
		 ) . ExecutionConfiguration.CriterionWeight.deconstruct

{- |
	* Perturb each weight by an independent random value, of configurable magnitude.

	* Under this transformation, /criterion-weights/ of zero will remain unchanged.
-}
perturbWeights :: (
	Enum			f,
	Fractional		f,
	Real			f,
	Show			f,
	System.Random.Random	f,
	System.Random.RandomGen	randomGen
 )
	=> randomGen
	-> f	-- ^ The magnitude of the random perturbation; resulting in an increase or a decrease, by a factor of up to @ (1 + x) @.
	-> Mutator f
perturbWeights _ 0 lessonCriteriaWeights	= lessonCriteriaWeights
perturbWeights randomGen changeMagnitude lessonCriteriaWeights
	| changeMagnitude < 0	= error $ "WeekDaze.ExecutionConfiguration.LessonCriteriaWeights.perturbWeights:\t" ++ show ExecutionConfiguration.OptimiseLessonCriteriaWeights.changeMagnitudeTag ++ " must be positive; " ++ show changeMagnitude ++ "."
	| otherwise		= normalise lessonCriteriaWeights {
		getWeightOfAreResourcesReused				= reduceBy a $ getWeightOfAreResourcesReused lessonCriteriaWeights,
		getWeightOfGreatestMinimumConsecutiveLessons		= reduceBy b $ getWeightOfGreatestMinimumConsecutiveLessons lessonCriteriaWeights,
		getWeightOfGreatestRemainingCourseLessons		= reduceBy c $ getWeightOfGreatestRemainingCourseLessons lessonCriteriaWeights,
		getWeightOfGreatestSynchronisedCourseSetSize		= reduceBy d $ getWeightOfGreatestSynchronisedCourseSetSize lessonCriteriaWeights,
		getWeightOfIsCoreKnowledgeRequirement			= reduceBy e $ getWeightOfIsCoreKnowledgeRequirement lessonCriteriaWeights,
		getWeightOfIsSpecialistInTopic				= reduceBy f $ getWeightOfIsSpecialistInTopic lessonCriteriaWeights,
		getWeightOfMatchCourseClassSizeToLocationCapacity	= reduceBy g $ getWeightOfMatchCourseClassSizeToLocationCapacity lessonCriteriaWeights,
		getWeightOfMaximiseRelativeFacilityUtilisation		= reduceBy h $ getWeightOfMaximiseRelativeFacilityUtilisation lessonCriteriaWeights,
		getWeightOfMaximiseStudentClassSizeOverCourseClassSize	= reduceBy i $ getWeightOfMaximiseStudentClassSizeOverCourseClassSize lessonCriteriaWeights,
		getWeightOfMaximiseStudentClassSizeOverLocationCapacity	= reduceBy j $ getWeightOfMaximiseStudentClassSizeOverLocationCapacity lessonCriteriaWeights,
		getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime	= reduceBy k $ getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime lessonCriteriaWeights,
		getWeightOfMinimiseBookingOfLocationByOtherTeachers	= reduceBy l $ getWeightOfMinimiseBookingOfLocationByOtherTeachers lessonCriteriaWeights,
		getWeightOfMinimiseDeviationFromTimeslotRequest		= reduceBy m $ getWeightOfMinimiseDeviationFromTimeslotRequest lessonCriteriaWeights,
		getWeightOfMinimiseInterCampusMigrationsOfStudents	= reduceBy n $ getWeightOfMinimiseInterCampusMigrationsOfStudents lessonCriteriaWeights,
		getWeightOfMinimiseInterCampusMigrationsOfTeachers	= reduceBy o $ getWeightOfMinimiseInterCampusMigrationsOfTeachers lessonCriteriaWeights,
		getWeightOfMinimiseStudentBodyCombinations		= reduceBy p $ getWeightOfMinimiseStudentBodyCombinations lessonCriteriaWeights,
		getWeightOfMinimiseTeachersLocusOperandi		= reduceBy q $ getWeightOfMinimiseTeachersLocusOperandi lessonCriteriaWeights,
		getWeightOfMinimiseWasteOfScarceFacilities		= reduceBy r $ getWeightOfMinimiseWasteOfScarceFacilities lessonCriteriaWeights
	}
	where
		(a : b : c : d : e : f : g : h : i : j : k : l : m : n : o : p : q : r : _)	= System.Random.randomRs (1, succ changeMagnitude) randomGen
		reduceBy randomValue								= ExecutionConfiguration.CriterionWeight.mkCriterionWeight . (/ randomValue) . ExecutionConfiguration.CriterionWeight.deconstruct

