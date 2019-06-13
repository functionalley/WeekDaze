{-# LANGUAGE CPP, FlexibleContexts #-}
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

 [@DESCRIPTION@]	Defines the data which drives the solution-mechanism, as opposed to that which either defines the problem or describes the presentation of the solution.
-}

module WeekDaze.ExecutionConfiguration.ExecutionOptions(
-- * Types
-- ** Type-synonyms
--	Hint,
--	Mutator,
	RandomSeed,
	PermitTemporaryStudentBodyMerger,
	ReduceStudentBodyRegister,
	RemoveRedundantCourses,
-- ** Data-types
	ExecutionOptions(..),
-- * Constants
--	tag,
--	filePathTag,
	inputStudentViewTimetableTag,
	permitTemporaryStudentBodyMergerTag,
	randomSeedTag,
	reduceStudentBodyRegisterTag,
	removeRedundantCoursesTag,
	removePointlessGroupsTag,
	removeUnsubscribedGroupsTag,
	zeroInappropriateOptionsTag,
-- * Functions
-- ** Mutators
	setPermitTemporaryStudentBodyMerger,
	setSynchronisedCourseMutationFecundity,
	setSynchronisedCourseByDayMutationFecundity,
	setExcessRunlengthMutationFecundity,
	setFecundityDecayRatio,
	setHomogeneousStudentViewLessonMutationFecundity,
	setIncompleteCourseMutationFecundity,
	setMinimumPopulationDiversityRatio,
	setMaybeNInitialScouts,
	setSingletonStudentClassMutationFecundity,
	setSplitSessionMutationFecundity,
	setStudentBodyCombinationMutationFecundity,
	setStudentViewTimetableForDayMutationFecundity,
	setStudentViewTimetableForWeekMutationFecundity,
	setSynchronousLessonMutationFecundity,
-- ** Predicates
	hintWasSpecified
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	System.FilePath
import qualified	Text.XML.HXT.Arrow.Pickle					as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Configuration						as Configuration
import qualified	WeekDaze.Enhanced.EnhancedEither				as Enhanced.EnhancedEither
import qualified	WeekDaze.ExecutionConfiguration.CriterionWeight			as ExecutionConfiguration.CriterionWeight
import qualified	WeekDaze.ExecutionConfiguration.EvolutionStrategies		as ExecutionConfiguration.EvolutionStrategies
import qualified	WeekDaze.ExecutionConfiguration.LessonCriteriaWeights		as ExecutionConfiguration.LessonCriteriaWeights
import qualified	WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights	as ExecutionConfiguration.OptimiseLessonCriteriaWeights
import qualified	WeekDaze.ExecutionConfiguration.TimetableBreederFecundity	as ExecutionConfiguration.TimetableBreederFecundity
import qualified	WeekDaze.ExecutionConfiguration.TimetableCriteriaWeights	as ExecutionConfiguration.TimetableCriteriaWeights
import qualified	WeekDaze.Model.TimetableAxisTriple				as Model.TimetableAxisTriple
import qualified	WeekDaze.Size							as Size
import			WeekDaze.Enhanced.EnhancedBool()

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector					as Database.Selector
import qualified	WeekDaze.Model.TimetableAxis					as Model.TimetableAxis
import qualified	WeekDaze.Model.TimetableAxisTraversal				as Model.TimetableAxisTraversal

instance (
	Control.DeepSeq.NFData		criterionWeight,
	Control.DeepSeq.NFData		fecundityDecayRatio,
	Control.DeepSeq.NFData		populationDiversityRatio,
	Data.Convertible.Convertible	Database.HDBC.SqlValue criterionWeight,			-- Flexible context.
	Data.Convertible.Convertible	Database.HDBC.SqlValue fecundityDecayRatio,		-- Flexible context.
	Data.Convertible.Convertible	Database.HDBC.SqlValue populationDiversityRatio,	-- Flexible context.
	Data.Typeable.Typeable		criterionWeight,
	Data.Typeable.Typeable		fecundityDecayRatio,
	Data.Typeable.Typeable		populationDiversityRatio,
	RealFrac			criterionWeight,
	RealFrac			fecundityDecayRatio,
	RealFrac			populationDiversityRatio
 ) => Database.Selector.Selector (ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio) where
	fromDatabase connection	projectIdSql	= let
		executionOptionsTableName, traversalOrderTableName :: Database.Selector.TableName
		executionOptionsTableName	= showString Database.Selector.tablePrefix tag
		traversalOrderTableName		= showString Database.Selector.tablePrefix Model.TimetableAxisTriple.tag
	 in do
		(evolutionStrategies, optimiseLessonCriteriaWeights, lessonCriteriaWeights, timetableCriteriaWeights)
#ifdef QUERY_DB_CONCURRENTLY
			<- Database.Selector.fromDatabaseConcurrently connection projectIdSql
#else
			<- Database.Selector.fromDatabase connection projectIdSql
#endif
		traversalOrderRows	<- Database.Selector.select connection [
			Model.TimetableAxisTraversal.senseTag,
			Model.TimetableAxis.tag
		 ] [traversalOrderTableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		executionOptionsRows	<- Database.Selector.select connection [
			randomSeedTag,
			permitTemporaryStudentBodyMergerTag,
			reduceStudentBodyRegisterTag,
			removeRedundantCoursesTag,
			removePointlessGroupsTag,
			removeUnsubscribedGroupsTag,
			zeroInappropriateOptionsTag
		 ] [executionOptionsTableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		return {-to IO-Monad-} . (
			if null traversalOrderRows
				then id
				else case traversalOrderRows of
					[row1, row2, row3]	-> let
						mkAxisTraversal :: [Database.HDBC.SqlValue] -> Model.TimetableAxisTraversal.AxisTraversal
						mkAxisTraversal row	= case row of
							[senseSql, axisSql]	-> let
								axisTraversal	= Data.Maybe.fromMaybe "" (Database.HDBC.fromSql senseSql) ++ Database.HDBC.fromSql axisSql
							 in case reads axisTraversal of
								[(x, "")]	-> x
								_		-> error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase.mkAxisTraversal:\tfailed to parse " $ shows axisTraversal "."
							_			-> error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase.mkAxisTraversal:\tunexpected number of columns=" . shows (length row) . showString " in row of table " $ shows traversalOrderTableName "; two are expected."
					 in \executionOptions -> executionOptions {
						getMaybeHint	= Just . Left $ Model.TimetableAxisTriple.mkAxes (mkAxisTraversal row1, mkAxisTraversal row2, mkAxisTraversal row3) -- Triple.
					 }
					_			-> error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tunexpected number of rows=" . shows (length traversalOrderRows) . showString " selected from table " $ shows traversalOrderTableName "; three are expected."
		 ) . (
			case executionOptionsRows of
				[]			-> id
				[executionOptionsRow]	-> case executionOptionsRow of
					[
						randomSeedSql,
						permitTemporaryStudentBodyMergerSql,
						reduceStudentBodyRegisterSql,
						removeRedundantCoursesSql,
						removePointlessGroupsSql,
						removeUnsubscribedGroupsSql,
						zeroInappropriateOptionsSql
					 ] -> (
						\executionOptions -> executionOptions {
							getMaybeRandomSeed	= either (
								error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tfailed to parse the value for " . shows randomSeedTag . showString " read from the database; " . show
							) id $ Database.HDBC.safeFromSql randomSeedSql
						}
					 ) . (
						\executionOptions -> Data.Maybe.maybe executionOptions (
							\value -> executionOptions { getPermitTemporaryStudentBodyMerger = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tfailed to parse the value for " . shows permitTemporaryStudentBodyMergerTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql permitTemporaryStudentBodyMergerSql
					 ) . (
						\executionOptions -> Data.Maybe.maybe executionOptions (
							\value -> executionOptions { getReduceStudentBodyRegister = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tfailed to parse the value for " . shows reduceStudentBodyRegisterTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql reduceStudentBodyRegisterSql
					 ) . (
						\executionOptions -> Data.Maybe.maybe executionOptions (
							\value -> executionOptions { getRemoveRedundantCourses = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tfailed to parse the value for " . shows removeRedundantCoursesTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql removeRedundantCoursesSql
					 ) . (
						\executionOptions -> Data.Maybe.maybe executionOptions (
							\value -> executionOptions { getRemovePointlessGroups = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tfailed to parse the value for " . shows removePointlessGroupsTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql removePointlessGroupsSql
					 ) . (
						\executionOptions -> Data.Maybe.maybe executionOptions (
							\value -> executionOptions { getRemoveUnsubscribedGroups = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tfailed to parse the value for " . shows removeUnsubscribedGroupsTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql removeUnsubscribedGroupsSql
					 ) . (
						\executionOptions -> Data.Maybe.maybe executionOptions (
							\value -> executionOptions { getZeroInappropriateOptions = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tfailed to parse the value for " . shows zeroInappropriateOptionsTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql zeroInappropriateOptionsSql
					 )
					_ -> error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tunexpected number of columns=" . shows (length executionOptionsRow) . showString " in row of table " $ shows executionOptionsTableName "."
				_			-> error . showString "WeekDaze.ExecutionConfiguration.ExecutionOptions.fromDatabase:\tunexpected number of rows=" . shows (length executionOptionsRows) . showString " selected from table " $ shows executionOptionsTableName "."
		 ) $ (
			Data.Default.def	:: ExecutionOptions Rational Rational Rational	-- Arbitrarily.
		 ) {
			getEvolutionStrategies			= evolutionStrategies,
			getOptimiseLessonCriteriaWeights	= optimiseLessonCriteriaWeights,
			getLessonCriteriaWeights		= lessonCriteriaWeights,
			getTimetableCriteriaWeights		= timetableCriteriaWeights
		}
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag					= "executionOptions"

-- | Used to qualify XML.
filePathTag :: String
filePathTag				= "filePath"

-- | Used to qualify XML.
inputStudentViewTimetableTag :: String
inputStudentViewTimetableTag		= "inputStudentViewTimetable"

-- | Used to qualify SQL & XML.
permitTemporaryStudentBodyMergerTag :: String
permitTemporaryStudentBodyMergerTag	= "permitTemporaryStudentBodyMerger"

-- | Used to qualify SQL & XML.
randomSeedTag :: String
randomSeedTag				= "randomSeed"

-- | Used to qualify SQL & XML.
reduceStudentBodyRegisterTag :: String
reduceStudentBodyRegisterTag		= "reduceStudentBodyRegister"

-- | Used to qualify SQL & XML.
removeRedundantCoursesTag :: String
removeRedundantCoursesTag		= "removeRedundantCourses"

-- | Used to qualify SQL & XML.
removePointlessGroupsTag :: String
removePointlessGroupsTag		= "removePointlessGroups"

-- | Used to qualify SQL & XML.
removeUnsubscribedGroupsTag :: String
removeUnsubscribedGroupsTag		= "removeUnsubscribedGroups"

-- | Used to qualify SQL & XML.
zeroInappropriateOptionsTag :: String
zeroInappropriateOptionsTag		= "zeroInappropriateOptions"

-- | One can either define the raster-scan by which the initial solution is constructed, or specify an XML-file containing the initial solution.
type Hint				= Either Model.TimetableAxisTriple.Axes System.FilePath.FilePath

-- | A seed from which to construct a pseudo-random number-generator.
type RandomSeed				= Int

-- | Whether to permit /student-bodies/ to be temporarily merged for a /lesson/.
type PermitTemporaryStudentBodyMerger	= Bool

-- | Whether to automatically merge /student-bodies/ with identical profiles.
type ReduceStudentBodyRegister		= Bool

-- | Whether to remove /course/s for which there's zero demand.
type RemoveRedundantCourses		= Bool

-- | Encapsulates the data which drives the implementation.
data ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio	= MkExecutionOptions {
	getEvolutionStrategies			:: ExecutionConfiguration.EvolutionStrategies.EvolutionStrategies fecundityDecayRatio populationDiversityRatio,	-- ^ The extent to which various strategies are applied, in an attempt to evolve the /timetable/.
	getMaybeHint				:: Maybe Hint,													-- ^ Optionally define either the order in which a /timetable/'s axes are traversed during construction of the initial deterministic solution or a path to a file from which the initial /StudentView.Timetable.Timetable/ can be read; the default is to perform all traverses, & select the best.
	getMaybeRandomSeed			:: Maybe RandomSeed,												-- ^ Optionally seed the pseudo-random number-generator to produce a repeatable sequence.
	getOptimiseLessonCriteriaWeights	:: ExecutionConfiguration.OptimiseLessonCriteriaWeights.OptimiseLessonCriteriaWeights criterionWeight,		-- ^ Whether & how to optimise the weights of lesson-criteria by maximising the weighted mean of timetable-criteria for the initial deterministic timetable.
	getLessonCriteriaWeights		:: ExecutionConfiguration.LessonCriteriaWeights.LessonCriteriaWeights criterionWeight,				-- ^ The weight associated with criteria used to select a /lesson/, at a specific time-coordinate in the /timetable/.
	getPermitTemporaryStudentBodyMerger	:: PermitTemporaryStudentBodyMerger,										-- ^ Whether to permit a temporary merger between /student-bodies/, to permit a /teacher/ & /location/ to be shared for the duration of a /course/.
	getReduceStudentBodyRegister		:: ReduceStudentBodyRegister,											-- ^ Whether to permanently merge /student-bodies/ with identical profiles, thus reducing the number of independently schedulable entities.
	getRemoveRedundantCourses		:: RemoveRedundantCourses,											-- ^ Whether to remove any /courses/ offered, which aren't required by any /student/.
	getRemovePointlessGroups		:: Bool,													-- ^ Whether to remove /groups/ from the /group-catalogue/, for which zero /meetings/ have been timetable.
	getRemoveUnsubscribedGroups		:: Bool,													-- ^ Whether to remove /groups/ from the /group-catalogue/, to which neither /student-bodies/ nor /teachers/ have subscribed.
	getTimetableCriteriaWeights		:: ExecutionConfiguration.TimetableCriteriaWeights.TimetableCriteriaWeights criterionWeight,			-- ^ The weight associated with criteria used to evaluate a /timetable/, during optimisation.
	getZeroInappropriateOptions		:: Bool														-- ^ Whether to zero the weight of each /lesson-criterion/ & /timetable-criterion/, & the /fecundity/ of each /evolution-strategy/, which is either inappropriate for, or irrelevant to, the specified /problem-parameters/.
} deriving (Eq, Show)

instance (
	Fractional	criterionWeight,
	Fractional	fecundityDecayRatio,
	Fractional	populationDiversityRatio
 ) => Data.Default.Default (ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio) where
	def = MkExecutionOptions {
		getEvolutionStrategies			= Data.Default.def,
		getMaybeHint				= Nothing,
		getMaybeRandomSeed			= Nothing,
		getOptimiseLessonCriteriaWeights	= Data.Default.def,
		getLessonCriteriaWeights		= Data.Default.def,
		getPermitTemporaryStudentBodyMerger	= True,
		getReduceStudentBodyRegister		= True,
		getRemoveRedundantCourses		= True,
		getRemovePointlessGroups		= True,
		getRemoveUnsubscribedGroups		= True,
		getTimetableCriteriaWeights		= Data.Default.def,
		getZeroInappropriateOptions		= True
	}

instance (
	Num	fecundityDecayRatio,
	Num	populationDiversityRatio,
	Ord	fecundityDecayRatio,
	Ord	populationDiversityRatio,
	Real	criterionWeight,
	Show	criterionWeight,
	Show	fecundityDecayRatio,
	Show	populationDiversityRatio
 ) => ToolShed.SelfValidate.SelfValidator (ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio) where
	getErrors MkExecutionOptions {
		getEvolutionStrategies			= evolutionStrategies,
		getMaybeHint				= maybeHint,
		getOptimiseLessonCriteriaWeights	= optimiseLessonCriteriaWeights,
		getLessonCriteriaWeights		= lessonCriteriaWeights,
		getPermitTemporaryStudentBodyMerger	= permitTemporaryStudentBodyMerger,
		getTimetableCriteriaWeights		= timetableCriteriaWeights
	}
		| not $ ToolShed.SelfValidate.isValid evolutionStrategies		= ToolShed.SelfValidate.getErrors evolutionStrategies
		| not $ ToolShed.SelfValidate.isValid optimiseLessonCriteriaWeights	= ToolShed.SelfValidate.getErrors optimiseLessonCriteriaWeights
		| not $ ToolShed.SelfValidate.isValid lessonCriteriaWeights		= ToolShed.SelfValidate.getErrors lessonCriteriaWeights
		| not $ ToolShed.SelfValidate.isValid timetableCriteriaWeights		= ToolShed.SelfValidate.getErrors timetableCriteriaWeights
		| otherwise	= ToolShed.SelfValidate.extractErrors [
			(
				Data.Maybe.maybe False (const False `either` (not . System.FilePath.isValid)) maybeHint,
				"invalid file-path; " ++ show maybeHint
			), (
				permitTemporaryStudentBodyMerger && any (/= minBound) [
					ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverLocationCapacity lessonCriteriaWeights,
					ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverCourseClassSize lessonCriteriaWeights
				],
				"when " ++ permitTemporaryStudentBodyMergerTag ++ "=" ++ show permitTemporaryStudentBodyMerger ++ ", " ++ ExecutionConfiguration.LessonCriteriaWeights.tag ++ "={" ++ show ExecutionConfiguration.LessonCriteriaWeights.weightOfMaximiseStudentClassSizeOverLocationCapacityTag ++ ", " ++ show ExecutionConfiguration.LessonCriteriaWeights.weightOfMaximiseStudentClassSizeOverCourseClassSizeTag ++ "} should be zero"
			), (
				not permitTemporaryStudentBodyMerger && any (/= minBound) [
					ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseStudentBodyCombinations lessonCriteriaWeights,
					ExecutionConfiguration.LessonCriteriaWeights.getWeightOfAreResourcesReused lessonCriteriaWeights,
					ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson timetableCriteriaWeights
				],
				"when " ++ permitTemporaryStudentBodyMergerTag ++ "=" ++ show permitTemporaryStudentBodyMerger ++ ", " ++ ExecutionConfiguration.LessonCriteriaWeights.tag ++ "={" ++ show ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseStudentBodyCombinationsTag ++ ", " ++ show ExecutionConfiguration.LessonCriteriaWeights.weightOfAreResourcesReusedTag ++ "} & " ++ ExecutionConfiguration.TimetableCriteriaWeights.tag ++ "={" ++ show ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanStudentBodyCombinationsPerLessonTag ++ "} should be zero"
			)
		]

instance (
	Fractional	criterionWeight,
	Fractional	fecundityDecayRatio,
	Fractional	populationDiversityRatio,
	HXT.XmlPickler	criterionWeight,
	HXT.XmlPickler	fecundityDecayRatio,
	HXT.XmlPickler	populationDiversityRatio,
	Ord		fecundityDecayRatio,
	Ord		populationDiversityRatio,
	Real		criterionWeight,
	Show		criterionWeight,
	Show		fecundityDecayRatio,
	Show		populationDiversityRatio
 ) => HXT.XmlPickler (ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, g, h, i, j, k, l)	-> MkExecutionOptions a b c d e f g h i j k l,	-- Construct from a tuple.
		\MkExecutionOptions {
			getEvolutionStrategies			= evolutionStrategies,
			getMaybeHint				= maybeHint,
			getMaybeRandomSeed			= maybeRandomSeed,
			getOptimiseLessonCriteriaWeights	= optimiseLessonCriteriaWeights,
			getLessonCriteriaWeights		= lessonCriteriaWeights,
			getPermitTemporaryStudentBodyMerger	= permitTemporaryStudentBodyMerger,
			getReduceStudentBodyRegister		= reduceStudentBodyRegister,
			getRemoveRedundantCourses		= removeRedundantCourses,
			getRemovePointlessGroups		= removePointlessGroups,
			getRemoveUnsubscribedGroups		= removeUnsubscribedGroups,
			getTimetableCriteriaWeights		= timetableCriteriaWeights,
			getZeroInappropriateOptions		= zeroInappropriateOptions
		} -> (
			evolutionStrategies,
			maybeHint,
			maybeRandomSeed,
			optimiseLessonCriteriaWeights,
			lessonCriteriaWeights,
			permitTemporaryStudentBodyMerger,
			reduceStudentBodyRegister,
			removeRedundantCourses,
			removePointlessGroups,
			removeUnsubscribedGroups,
			timetableCriteriaWeights,
			zeroInappropriateOptions
		) -- Deconstruct to a tuple.
	 ) $ HXT.xp12Tuple HXT.xpickle {-evolutionStrategies-} (
		HXT.xpOption . Enhanced.EnhancedEither.xpickle HXT.xpickle {-traversalOrder-} . HXT.xpElem inputStudentViewTimetableTag $ HXT.xpTextAttr filePathTag
	 ) (
		HXT.xpAttrImplied randomSeedTag HXT.xpInt
	 ) HXT.xpickle {-optimiseLessonCriteriaWeights-} HXT.xpickle {-lessonCriteriaWeights-} (
		getPermitTemporaryStudentBodyMerger defaultExecutionOptions `HXT.xpDefault` HXT.xpAttr permitTemporaryStudentBodyMergerTag HXT.xpickle {-Bool-}
	 ) (
		getReduceStudentBodyRegister defaultExecutionOptions `HXT.xpDefault` HXT.xpAttr reduceStudentBodyRegisterTag HXT.xpickle {-Bool-}
	 ) (
		getRemoveRedundantCourses defaultExecutionOptions `HXT.xpDefault` HXT.xpAttr removeRedundantCoursesTag HXT.xpickle {-Bool-}
	 ) (
		getRemovePointlessGroups defaultExecutionOptions `HXT.xpDefault` HXT.xpAttr removePointlessGroupsTag HXT.xpickle {-Bool-}
	 ) (
		getRemoveUnsubscribedGroups defaultExecutionOptions `HXT.xpDefault` HXT.xpAttr removeUnsubscribedGroupsTag HXT.xpickle {-Bool-}
	 ) HXT.xpickle {-timetableCriteriaWeights-} (
		getZeroInappropriateOptions defaultExecutionOptions `HXT.xpDefault` HXT.xpAttr zeroInappropriateOptionsTag HXT.xpickle {-Bool-}
	 ) where
		defaultExecutionOptions :: ExecutionOptions Rational Rational Rational	-- The type-parameters are irrelevant & are therefore largely arbitrarily.
		defaultExecutionOptions	= Data.Default.def

instance (
	Control.DeepSeq.NFData	criterionWeight,
	Control.DeepSeq.NFData	fecundityDecayRatio,
	Control.DeepSeq.NFData	populationDiversityRatio
 ) => Control.DeepSeq.NFData (ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio) where
	rnf MkExecutionOptions {
		getEvolutionStrategies			= evolutionStrategies,
		getMaybeHint				= maybeHint,
		getMaybeRandomSeed			= maybeRandomSeed,
		getOptimiseLessonCriteriaWeights	= optimiseLessonCriteriaWeights,
		getLessonCriteriaWeights		= lessonCriteriaWeights,
		getPermitTemporaryStudentBodyMerger	= permitTemporaryStudentBodyMerger,
		getReduceStudentBodyRegister		= reduceStudentBodyRegister,
		getRemoveRedundantCourses		= removeRedundantCourses,
		getRemovePointlessGroups		= removePointlessGroups,
		getRemoveUnsubscribedGroups		= removeUnsubscribedGroups,
		getTimetableCriteriaWeights		= timetableCriteriaWeights,
		getZeroInappropriateOptions		= zeroInappropriateOptions
	} = Control.DeepSeq.rnf (
		evolutionStrategies,
		maybeHint,
		maybeRandomSeed,
		optimiseLessonCriteriaWeights,
		lessonCriteriaWeights,
		[
			permitTemporaryStudentBodyMerger,
			reduceStudentBodyRegister,
			removeRedundantCourses,
			removePointlessGroups,
			removeUnsubscribedGroups
		],
		timetableCriteriaWeights,
		zeroInappropriateOptions
	 )

-- | The type of a function used to transform 'ExecutionOptions'.
type Mutator criterionWeight fecundityDecayRatio populationDiversityRatio	= ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio -> ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio

{- |
	* Mutator.

	* If /student-bodies/ can be temporarily merged, allowing the size of /student-class/es to grow,
	then there's no obvious advantage to fitting this transient class-size to either the capacity of the /location/ or to any maximum class-size defined for the /course/;
	so 'ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverLocationCapacity' & 'ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverCourseClassSize' aren't relevant,
	equally if student-bodies can't be merged, then synchronous identical /lesson/s are permissible, so there's no point trying to promote them via 'ExecutionConfiguration.LessonCriteriaWeights.getWeightOfAreResourcesReused'.

	* Alternatively, if /student-bodies/ can't be temporarily merged into a variety of /student-classes/,
	then there's no concept of either 'ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseStudentBodyCombinations' or 'ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson'.
-}
setPermitTemporaryStudentBodyMerger :: Num criterionWeight => PermitTemporaryStudentBodyMerger -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setPermitTemporaryStudentBodyMerger permitTemporaryStudentBodyMerger executionOptions@MkExecutionOptions {
	getLessonCriteriaWeights	= lessonCriteriaWeights,
	getTimetableCriteriaWeights	= timetableCriteriaWeights
}
	| permitTemporaryStudentBodyMerger = executionOptions' {
		getLessonCriteriaWeights	= lessonCriteriaWeights {
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverLocationCapacity	= minBound,	-- The student-class needs space for growth.
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverCourseClassSize	= minBound	-- The student-class needs space for growth.
		} -- Turn off.
	}
	| otherwise = executionOptions' {
		getLessonCriteriaWeights	= lessonCriteriaWeights {
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseStudentBodyCombinations	= minBound,	-- Each student-class, has a single constant member.
			ExecutionConfiguration.LessonCriteriaWeights.getWeightOfAreResourcesReused		= minBound	-- Resources can only be reused by merging student-classes.
		}, -- Turn off.
		getTimetableCriteriaWeights	= timetableCriteriaWeights {
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanStudentBodyCombinationsPerLesson	= minBound,	-- The mean is constant (one) for all timetable.
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseMeanStudentClassSize			= minBound	-- The size of the student-class equals the size its single student-body.
		} -- Turn off.
	}
	where
		executionOptions'	= executionOptions { getPermitTemporaryStudentBodyMerger = permitTemporaryStudentBodyMerger }	-- Mutate the original request.

-- | Mutator.
setSynchronisedCourseMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setSynchronisedCourseMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getSynchronisedCourseMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setSynchronisedCourseByDayMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setSynchronisedCourseByDayMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getSynchronisedCourseByDayMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setExcessRunlengthMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setExcessRunlengthMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getExcessRunlengthMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setFecundityDecayRatio :: fecundityDecayRatio -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setFecundityDecayRatio fecundityDecayRatio executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getFecundityDecayRatio	= fecundityDecayRatio
	}
}

-- | Mutator.
setHomogeneousStudentViewLessonMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setHomogeneousStudentViewLessonMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getHomogeneousStudentViewLessonMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setIncompleteCourseMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setIncompleteCourseMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getIncompleteCourseMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setMinimumPopulationDiversityRatio :: populationDiversityRatio -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setMinimumPopulationDiversityRatio populationDiversityRatio executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getMinimumPopulationDiversityRatio	= populationDiversityRatio
	}
}

-- | Mutator.
setMaybeNInitialScouts :: Maybe Size.NTimetables -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setMaybeNInitialScouts maybeNInitialScouts executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getMaybeNInitialScouts	= maybeNInitialScouts
	}
}

-- | Mutator.
setSingletonStudentClassMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setSingletonStudentClassMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getSingletonStudentClassMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setSplitSessionMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setSplitSessionMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getSplitSessionMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setStudentBodyCombinationMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setStudentBodyCombinationMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getStudentBodyCombinationMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setStudentViewTimetableForDayMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setStudentViewTimetableForDayMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForDayMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setStudentViewTimetableForWeekMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setStudentViewTimetableForWeekMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForWeekMutationFecundity	= timetableBreederFecundity
	}
}

-- | Mutator.
setSynchronousLessonMutationFecundity :: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity -> Mutator criterionWeight fecundityDecayRatio populationDiversityRatio
setSynchronousLessonMutationFecundity timetableBreederFecundity executionOptions	= executionOptions {
	getEvolutionStrategies	= (getEvolutionStrategies executionOptions) {
		ExecutionConfiguration.EvolutionStrategies.getSynchronousLessonMutationFecundity	= timetableBreederFecundity
	}
}

-- | True if a (/traversalOrder/, /input file-path/) was specified.
hintWasSpecified :: ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio -> (Bool, Bool)
hintWasSpecified	= Data.Maybe.maybe (False, False) (const (True, False) `either` const (False, True)) . getMaybeHint

instance (
	Eq	criterionWeight,
	Num	criterionWeight
 ) => Configuration.Configuration (ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio) where
	issueWarnings executionOptions	= [
		msg | (True, msg) <- [
			(
				ExecutionConfiguration.CriterionWeight.areAllZero $ getTimetableCriteriaWeights executionOptions, {-lazy evaluation-}
				"the weights of all timetable-criteria are zero; the relative fitness of timetables cannot be measured"
			), (
				ExecutionConfiguration.EvolutionStrategies.areAllZero $ getEvolutionStrategies executionOptions,
				"the fecundities of all evolution-strategies are zero; evolution cannot occur"
			)
		 ]
	 ]
