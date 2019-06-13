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

 [@DESCRIPTION@]

	* Defines all configured options.

	* These options are categorised into those which; define the problem, drive the implementation of the solution, & govern the required output;
	& consequently, most of this module's interface merely proxies requests to the appropriate sub-module.
-}

module WeekDaze.Input.Options(
-- * Types
-- ** Type-synonyms
	ShowLocationViewOfTimetable,
	ShowTeacherViewOfTimetable,
--	Mutator,
-- ** Data-types
	Options(..),
-- * Constants
	tag,
-- * Functions
-- ** Mutators
	appendOutputStudentViewTimetableFilePath,
--	criteriaWeightsMutator,
	setDisplayRuntimeInformation,
	setFecundityDecayRatio,
	setInputStudentViewTimetableFilePath,
	setMaybeNInitialScouts,
	setMaybeOutputConfigFilePath,
	setMaybeRandomSeed,
	setMinimumPopulationDiversityRatio,
	setNDecimalDigits,
	setOptimiseLessonCriteriaWeights,
--	setLessonCriteriaWeights,
	setPermitTemporaryStudentBodyMerger,
	setReduceStudentBodyRegister,
	setRemoveRedundantCourses,
--	setTimetableCriteriaWeights,
	setTimeslotIdBounds,
	setVerbosity,
--	setWeekend,
	zeroInappropriateExecutionOptions
) where

import			Control.Arrow((&&&))
import qualified	Control.Arrow
import qualified	Control.Monad.Writer
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Distribution.Verbosity
import qualified	Factory.Data.Interval
import qualified	System.FilePath
import qualified	Text.XML.HXT.Arrow.Pickle					as HXT
import qualified	Text.XML.HXT.DOM.Util
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Aggregate.LocationCatalogue				as Aggregate.LocationCatalogue
import qualified	WeekDaze.Aggregate.StudentBodyRegister				as Aggregate.StudentBodyRegister
import qualified	WeekDaze.Aggregate.StudentClass					as Aggregate.StudentClass
import qualified	WeekDaze.Aggregate.TeacherRegister				as Aggregate.TeacherRegister
import qualified	WeekDaze.Configuration						as Configuration
import qualified	WeekDaze.Data.Course						as Data.Course
import qualified	WeekDaze.Data.Student						as Data.Student
import qualified	WeekDaze.Data.Teacher						as Data.Teacher
import qualified	WeekDaze.ExecutionConfiguration.CriterionWeight			as ExecutionConfiguration.CriterionWeight
import qualified	WeekDaze.ExecutionConfiguration.EvolutionStrategies		as ExecutionConfiguration.EvolutionStrategies
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions		as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.ExecutionConfiguration.LessonCriteriaWeights		as ExecutionConfiguration.LessonCriteriaWeights
import qualified	WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights	as ExecutionConfiguration.OptimiseLessonCriteriaWeights
import qualified	WeekDaze.ExecutionConfiguration.TimetableBreederFecundity	as ExecutionConfiguration.TimetableBreederFecundity
import qualified	WeekDaze.ExecutionConfiguration.TimetableCriteriaWeights	as ExecutionConfiguration.TimetableCriteriaWeights
import qualified	WeekDaze.Input.ConfigVersion					as Input.ConfigVersion
import qualified	WeekDaze.OutputConfiguration.FileFormat				as OutputConfiguration.FileFormat
import qualified	WeekDaze.OutputConfiguration.Format				as OutputConfiguration.Format
import qualified	WeekDaze.OutputConfiguration.Options				as OutputConfiguration.Options
import qualified	WeekDaze.OutputConfiguration.Style				as OutputConfiguration.Style
import qualified	WeekDaze.OutputConfiguration.View				as OutputConfiguration.View
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters			as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.ProblemConfiguration.ProblemValidationSwitches		as ProblemConfiguration.ProblemValidationSwitches
import qualified	WeekDaze.Size							as Size
import qualified	WeekDaze.Temporal.Day						as Temporal.Day

#ifdef USE_HDBC
import qualified	Control.DeepSeq
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector					as Database.Selector

instance (
	Control.DeepSeq.NFData			campus,
	Control.DeepSeq.NFData			criterionWeight,
	Control.DeepSeq.NFData			level,
	Control.DeepSeq.NFData			fecundityDecayRatio,
	Control.DeepSeq.NFData			locationId,
	Control.DeepSeq.NFData			minimumContrastRatio,
	Control.DeepSeq.NFData			populationDiversityRatio,
	Control.DeepSeq.NFData			stream,
	Control.DeepSeq.NFData			synchronisationId,
	Control.DeepSeq.NFData			teacherId,
	Control.DeepSeq.NFData			teachingRatio,
	Control.DeepSeq.NFData			timeslotId,
	Data.Convertible.Convertible		Database.HDBC.SqlValue campus,				-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue criterionWeight,			-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue fecundityDecayRatio,		-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue level,				-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue locationId,			-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue populationDiversityRatio,	-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue stream,				-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue synchronisationId,		-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue teacherId,			-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue teachingRatio,			-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue timeslotId,			-- Flexible context.
	Data.Default.Default			campus,
	Data.Default.Default			stream,
	Data.Typeable.Typeable			criterionWeight,
	Data.Typeable.Typeable			fecundityDecayRatio,
	Data.Typeable.Typeable			populationDiversityRatio,
	Data.Typeable.Typeable			teachingRatio,
	Fractional				minimumContrastRatio,
	Ord					level,
	Ord					locationId,
	Ord					synchronisationId,
	Ord					teacherId,
	Ord					timeslotId,
	RealFrac				criterionWeight,
	RealFrac				fecundityDecayRatio,
	RealFrac				populationDiversityRatio,
	RealFrac				teachingRatio,
	Show					campus,
	Show					criterionWeight,
	Show					level,
	Show					locationId,
	Show					minimumContrastRatio,
	Show					synchronisationId,
	Show					timeslotId
 ) => Database.Selector.Selector (Options campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId) where
	fromDatabase connection	projectIdSql	= let
		tableName :: Database.Selector.TableName
		tableName	= Database.Selector.tablePrefix ++ tag
	 in do
		configVersionStrings	<- map head {-for each row select the only column-} `fmap` Database.Selector.select connection [Input.ConfigVersion.tag] [tableName] [(Database.Selector.projectIdColumnName, projectIdSql)]	-- Extract the configVersion for this project; there can only be zero or one row.

		(problemParameters, executionOptions, outputOptions)
#ifdef QUERY_DB_CONCURRENTLY
			<- Database.Selector.fromDatabaseConcurrently connection projectIdSql
#else
			<- Database.Selector.fromDatabase connection projectIdSql
#endif
		return {-to IO-monad-} MkOptions {
			getConfigVersion	= if null configVersionStrings
				then Input.ConfigVersion.defaultConfigVersion
				else {-defined-} Data.Maybe.maybe (
					error $ "WeekDaze.Input.Options.fromDatabase:\tnull " ++ show Input.ConfigVersion.tag ++ "."
				) Input.ConfigVersion.fromString . Database.HDBC.fromSql $ head configVersionStrings,
			getProblemParameters	= problemParameters,
			getExecutionOptions	= executionOptions,
			getOutputOptions	= outputOptions
		}
#endif /* USE_HDBC */

-- | The type of a function used to transform 'Options'.
type Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId	= Options campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId -> Options campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId

-- | Mutator.
appendOutputStudentViewTimetableFilePath
	:: Show minimumContrastRatio
	=> System.FilePath.FilePath
	-> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
appendOutputStudentViewTimetableFilePath filePath options	= options {
	getOutputOptions	= outputOptions {
		OutputConfiguration.Options.getFileFormats	= OutputConfiguration.FileFormat.mkFileFormat filePath (OutputConfiguration.Format.XML OutputConfiguration.View.StudentView) : OutputConfiguration.Options.getFileFormats outputOptions
	}
} where
	outputOptions	= getOutputOptions options

-- Used to qualify XML.
tag :: String
tag	= "options"

-- | Defines whether the /timetable/ should be shown, from the point-of-view of each /location/.
type ShowLocationViewOfTimetable	= Bool

-- | Defines whether the /timetable/ should be shown, from the point-of-view of each /teacher/.
type ShowTeacherViewOfTimetable	= Bool

-- | Defines the command-line options.
data Options campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId	= MkOptions {
	getConfigVersion	:: Input.ConfigVersion.ConfigVersion,														-- ^ The version of the semantics & structure of the configuration-file; cf. the version of XML in which it's written.
	getExecutionOptions	:: ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio,			-- ^ Defines the means by which the problem should be solved.
	getOutputOptions	:: OutputConfiguration.Options.Options minimumContrastRatio,											-- ^ Defines the presentation of the results.
	getProblemParameters	:: ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	-- ^ Defines the parameters of the problem.
} deriving (Eq, Show)

instance (
	Enum		timeslotId,
	Num		fecundityDecayRatio,
	Num		populationDiversityRatio,
	Ord		fecundityDecayRatio,
	Ord		level,
	Ord		locationId,
	Ord		populationDiversityRatio,
	Ord		stream,
	Ord		synchronisationId,
	Ord		teacherId,
	Ord		timeslotId,
	Real		criterionWeight,
	RealFrac	teachingRatio,
	Show		criterionWeight,
	Show		fecundityDecayRatio,
	Show		level,
	Show		locationId,
	Show		minimumContrastRatio,
	Show		populationDiversityRatio,
	Show		stream,
	Show		synchronisationId,
	Show		teacherId,
	Show		teachingRatio,
	Show		timeslotId
 ) => ToolShed.SelfValidate.SelfValidator (Options campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId) where
	getErrors MkOptions {
		getExecutionOptions	= executionOptions,
		getOutputOptions	= outputOptions,
		getProblemParameters	= problemParameters
	}
		| not $ ToolShed.SelfValidate.isValid executionOptions	= ToolShed.SelfValidate.getErrors executionOptions
		| not $ ToolShed.SelfValidate.isValid outputOptions	= ToolShed.SelfValidate.getErrors outputOptions
		| not $ ToolShed.SelfValidate.isValid problemParameters	= ToolShed.SelfValidate.getErrors problemParameters
		| otherwise						= ToolShed.SelfValidate.extractErrors [
			(
				ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger executionOptions && uncurry (||) (
					ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodiesExceedTeachers &&& ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodySizeExceedsLocationCapacity $ ProblemConfiguration.ProblemParameters.getProblemValidationSwitches problemParameters
				),
				show ExecutionConfiguration.ExecutionOptions.permitTemporaryStudentBodyMergerTag ++ " & any of " ++ show [ProblemConfiguration.ProblemValidationSwitches.checkIfStudentBodiesExceedTeachersTag, ProblemConfiguration.ProblemValidationSwitches.checkIfStudentBodySizeExceedsLocationCapacityTag] ++ ", are incompatible"
			),
			let
				nMutations, nTimeslotsPerWeek :: Size.NTimeslots
				nMutations		= ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForWeekMutationNTimeslots $ ExecutionConfiguration.ExecutionOptions.getEvolutionStrategies executionOptions
				nTimeslotsPerWeek	= ProblemConfiguration.ProblemParameters.calculateNTimeslotsPerDay (ProblemConfiguration.ProblemParameters.getTimeslotIdBounds problemParameters) * Temporal.Day.nDaysPerWeek
			in (
				nMutations > nTimeslotsPerWeek,
				"the specified number of time-slots " ++ show nMutations ++ ", to mutate during " ++ show ExecutionConfiguration.EvolutionStrategies.studentViewTimetableForWeekMutationTag ++ ", mustn't exceed the " ++ show nTimeslotsPerWeek ++ " available time-slots per week"
			)
		]

instance (
	Data.Default.Default	campus,
	Data.Default.Default	stream,
	Eq			campus,
	Eq			stream,
	Fractional		criterionWeight,
	Fractional		fecundityDecayRatio,
	Fractional		minimumContrastRatio,
	Fractional		populationDiversityRatio,
	HXT.XmlPickler		campus,
	HXT.XmlPickler		criterionWeight,
	HXT.XmlPickler		fecundityDecayRatio,
	HXT.XmlPickler		level,
	HXT.XmlPickler		locationId,
	HXT.XmlPickler		minimumContrastRatio,
	HXT.XmlPickler		populationDiversityRatio,
	HXT.XmlPickler		stream,
	HXT.XmlPickler		synchronisationId,
	HXT.XmlPickler		teacherId,
	HXT.XmlPickler		teachingRatio,
	HXT.XmlPickler		timeslotId,
	Ord			fecundityDecayRatio,
	Ord			level,
	Ord			locationId,
	Ord			minimumContrastRatio,
	Ord			populationDiversityRatio,
	Ord			synchronisationId,
	Ord			teacherId,
	Ord			timeslotId,
	Real			criterionWeight,
	Real			teachingRatio,
	Show			campus,
	Show			criterionWeight,
	Show			fecundityDecayRatio,
	Show			level,
	Show			minimumContrastRatio,
	Show			populationDiversityRatio,
	Show			synchronisationId,
	Show			timeslotId
 ) => HXT.XmlPickler (Options campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		Text.XML.HXT.DOM.Util.uncurry4 MkOptions,	-- Construct from a quadruple.
		\MkOptions {
			getConfigVersion	= configVersion,
			getExecutionOptions	= executionOptions,
			getOutputOptions	= outputOptions,
			getProblemParameters	= problemParameters
		} -> (
			configVersion,
			executionOptions,
			outputOptions,
			problemParameters
		) -- Deconstruct to a quadruple.
	 ) $ HXT.xp4Tuple (
		HXT.xpDefault Input.ConfigVersion.defaultConfigVersion . HXT.xpElem Input.ConfigVersion.tag . HXT.xpList1 {-the default is null-} . HXT.xpElem Input.ConfigVersion.elementTag $ HXT.xpAttr "integer" HXT.xpInt
	 ) (
		HXT.xpDefault Data.Default.def HXT.xpickle	-- executionOptions.
	 ) HXT.xpickle {-outputOptions-} HXT.xpickle {-problemParameters-}

{- |
	* Mutator.

	* Sets /displayRuntimeInformation/ in all XHTML-formats.
-}
setDisplayRuntimeInformation :: OutputConfiguration.Style.DisplayRuntimeInformation -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setDisplayRuntimeInformation b options	= options {
	getOutputOptions	= outputOptions {
		OutputConfiguration.Options.getFileFormats	= map (
			\fileFormat -> fileFormat {
				OutputConfiguration.FileFormat.getFormat	= case OutputConfiguration.FileFormat.getFormat fileFormat of
					OutputConfiguration.Format.XHTML style	-> OutputConfiguration.Format.XHTML style { OutputConfiguration.Style.getDisplayRuntimeInformation = b }
					xml					-> xml
			}
		) $ OutputConfiguration.Options.getFileFormats outputOptions
	}
} where
	outputOptions	= getOutputOptions options

-- | Mutator.
setFecundityDecayRatio :: fecundityDecayRatio -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setFecundityDecayRatio fecundityDecayRatio options	= options {
	getExecutionOptions	= ExecutionConfiguration.ExecutionOptions.setFecundityDecayRatio fecundityDecayRatio $ getExecutionOptions options
}

-- | Mutator.
setInputStudentViewTimetableFilePath :: System.FilePath.FilePath -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setInputStudentViewTimetableFilePath filePath options	= options {
	getExecutionOptions	= (getExecutionOptions options) {
		ExecutionConfiguration.ExecutionOptions.getMaybeHint	= Just $ Right filePath
	}
}

-- | Mutator.
setMaybeNInitialScouts :: Maybe Size.NTimetables -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setMaybeNInitialScouts maybeNInitialScouts options	= options {
	getExecutionOptions	= ExecutionConfiguration.ExecutionOptions.setMaybeNInitialScouts maybeNInitialScouts $ getExecutionOptions options
}

-- | Mutator.
setMaybeOutputConfigFilePath :: Maybe System.FilePath.FilePath -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setMaybeOutputConfigFilePath maybeOutputConfigFilePath options	= options {
	getOutputOptions	= (getOutputOptions options) {
		OutputConfiguration.Options.getMaybeOutputConfigFilePath	= maybeOutputConfigFilePath
	}
}

-- | Mutator.
setMaybeRandomSeed :: Maybe ExecutionConfiguration.ExecutionOptions.RandomSeed -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setMaybeRandomSeed maybeSeed options	= options {
	getExecutionOptions	= (getExecutionOptions options) {
		ExecutionConfiguration.ExecutionOptions.getMaybeRandomSeed	= maybeSeed
	}
}

-- | Mutator.
setMinimumPopulationDiversityRatio :: populationDiversityRatio -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setMinimumPopulationDiversityRatio populationDiversityRatio options	= options {
	getExecutionOptions	= ExecutionConfiguration.ExecutionOptions.setMinimumPopulationDiversityRatio populationDiversityRatio $ getExecutionOptions options
}

-- | Mutator.
setNDecimalDigits :: OutputConfiguration.Options.NDecimalDigits -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setNDecimalDigits nDecimalDigits options	= options {
	getOutputOptions	= (getOutputOptions options) {
		OutputConfiguration.Options.getNDecimalDigits	= nDecimalDigits
	}
}

-- | Mutator.
setOptimiseLessonCriteriaWeights :: ExecutionConfiguration.OptimiseLessonCriteriaWeights.OptimiseLessonCriteriaWeights criterionWeight -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setOptimiseLessonCriteriaWeights optimiseLessonCriteriaWeights options	= options {
	getExecutionOptions	= (getExecutionOptions options) {
		ExecutionConfiguration.ExecutionOptions.getOptimiseLessonCriteriaWeights	= optimiseLessonCriteriaWeights
	}
}

{- |
	* Mutator.

	* Other options are dependent on this one, & so are also toggled.
	If this is inappropriate, then they can always be toggled back, by a separate request.
-}
setPermitTemporaryStudentBodyMerger :: Num criterionWeight => ExecutionConfiguration.ExecutionOptions.PermitTemporaryStudentBodyMerger -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setPermitTemporaryStudentBodyMerger permitTemporaryStudentBodyMerger options	= options {
	getProblemParameters	= problemParameters {
		ProblemConfiguration.ProblemParameters.getProblemValidationSwitches	= (ProblemConfiguration.ProblemParameters.getProblemValidationSwitches problemParameters) {
			ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodiesExceedTeachers		= not permitTemporaryStudentBodyMerger,
			ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodySizeExceedsLocationCapacity	= not permitTemporaryStudentBodyMerger
		}
	},
	getExecutionOptions	= ExecutionConfiguration.ExecutionOptions.setPermitTemporaryStudentBodyMerger permitTemporaryStudentBodyMerger $ getExecutionOptions options
} where
	problemParameters	= getProblemParameters options

-- | Mutator.
setReduceStudentBodyRegister :: ExecutionConfiguration.ExecutionOptions.ReduceStudentBodyRegister -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setReduceStudentBodyRegister b options	= options {
	getExecutionOptions	= (getExecutionOptions options) {
		ExecutionConfiguration.ExecutionOptions.getReduceStudentBodyRegister	= b
	}
}

-- | Mutator.
setRemoveRedundantCourses :: ExecutionConfiguration.ExecutionOptions.RemoveRedundantCourses -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setRemoveRedundantCourses b options	= options {
	getExecutionOptions	= (getExecutionOptions options) {
		ExecutionConfiguration.ExecutionOptions.getRemoveRedundantCourses	= b
	}
}

-- | Mutator.
setTimetableCriteriaWeights :: ExecutionConfiguration.TimetableCriteriaWeights.TimetableCriteriaWeights criterionWeight -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setTimetableCriteriaWeights timetableCriteriaWeights options	= options {
	getExecutionOptions	= (getExecutionOptions options) {
		ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights	= timetableCriteriaWeights
	}
}

{- |
	* Mutator.

	* As a side-effect various criteria-weights may be turned-off, because the concepts to which they relate, don't exist until there're sufficient /time-slot/s per day.
-}
setTimeslotIdBounds :: (Enum timeslotId, Num criterionWeight) => Factory.Data.Interval.Interval timeslotId -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setTimeslotIdBounds timeslotIdBounds options
	| timeslotsPerDay < 2	= setTimetableCriteriaWeights (
		timetableCriteriaWeights {
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest	= minBound,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseRatioOfConsecutiveEqualLessons			= minBound,
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay		= minBound
		} -- Turn off those criteria for which there's no concept until there're two timeslots per day.
	) options'
	| timeslotsPerDay < 3	= setTimetableCriteriaWeights (
		timetableCriteriaWeights {
			ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay	= minBound
		} -- Turn off those criteria for which there's no concept until there're three timeslots per day.
	) options'
	| otherwise	= options'
	where
		timeslotsPerDay	= ProblemConfiguration.ProblemParameters.calculateNTimeslotsPerDay timeslotIdBounds

		options'	= options {
			getProblemParameters	= (getProblemParameters options) {
				ProblemConfiguration.ProblemParameters.getTimeslotIdBounds	= timeslotIdBounds
			}
		} -- Amend timeslotIdBounds as directly requested.

		timetableCriteriaWeights	= ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights $ getExecutionOptions options'

-- | Mutator.
setVerbosity :: Distribution.Verbosity.Verbosity -> Mutator campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId
setVerbosity verbosity options	= options {
	getOutputOptions	= (getOutputOptions options) {
		OutputConfiguration.Options.getVerbosity	= verbosity
	}
}

{- |
	* Depending on each of the specified flags, uses the specified function to mutate each corresponding /criterion-weight/.
	* Logs the tags of all those criterion-weights changed.
-}
criteriaWeightsMutator :: (
	Eq	criterionWeight,
	Num	criterionWeight
 )
	=> criteriaWeights
	-> [
		(
			(
				criteriaWeights -> ExecutionConfiguration.CriterionWeight.CriterionWeight criterionWeight,
				Bool
			), (
				criteriaWeights -> criteriaWeights,
				String
			) -- Pair.
		) -- Pair.
	] -- ^ [((Criterion-weights accessor, Whether to mutate this criterion-weight), (Criteria-weights mutator, Criterion-weight tag))].
	-> Control.Monad.Writer.Writer [String] criteriaWeights	-- ^ Zeroed tags & transformed value.
criteriaWeightsMutator criteriaWeights = foldr (
	(
		\(mutator, message)	-> (
			Control.Monad.Writer.tell [message] >>	-- Log the tag.
		) . fmap mutator	-- Apply the mutator.
	) . snd
 ) (
	return {-to Writer-monad-} criteriaWeights	-- Initial value.
 ) . filter (
	uncurry (&&) . Control.Arrow.first (
		(/= minBound) . ($ criteriaWeights)
	) . fst {-conditions-}
 )

{- |
	* Zeroes the weight of each /lesson-criterion/ & /timetable-criterion/, & the /fecundity/ of each /evolution-strategy/,
	which is either inappropriate for, or irrelevant to, the specified /problem-parameters/.

	* Logs the affected tags.
-}
zeroInappropriateExecutionOptions :: (
	Enum		timeslotId,
	Eq		criterionWeight,
	Num		criterionWeight,
	Ord		campus,
	Ord		level,
	Ord		synchronisationId,
	Ord		timeslotId,
	RealFrac	teachingRatio
 )
	=> Bool	-- ^ Whether to zero non-critical weights, or merely those that must be zeroed to avoid generating an error.
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio	-- ^ Initial value.
	-> (
		ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio,
		(
			(
				[String],
				[String]
			),
			[String]
		)
	) -- ^ Transformed /evolution-strategies/, & the tags of zeroed ((/lesson-criteria weights/, /timetable-criteria weights/), /evolution-strategy fecundities/).
zeroInappropriateExecutionOptions zeroNonCritical problemParameters executionOptions	= (
	executionOptions {
		ExecutionConfiguration.ExecutionOptions.getLessonCriteriaWeights	= lessonCriteriaWeights,
		ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights	= timetableCriteriaWeights,
		ExecutionConfiguration.ExecutionOptions.getEvolutionStrategies		= evolutionStrategies
	}, (
		(
			zeroedLessonCriterionWeightTags,
			zeroedTimetableCriterionWeightTags
		), -- Pair.
		zeroedEvolutionStrategyFecundityTags
	) -- Pair.
 ) where
	nTimeslotsPerDay :: Size.NTimeslots
	nTimeslotsPerDay	= ProblemConfiguration.ProblemParameters.calculateNTimeslotsPerDay $ ProblemConfiguration.ProblemParameters.getTimeslotIdBounds problemParameters

	permitTemporaryStudentBodyMerger :: Bool
	permitTemporaryStudentBodyMerger	= ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger executionOptions

	locationCatalogue	= ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters		-- Access.
	studentBodyRegister	= ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters	-- Access.
	teacherRegister		= ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters		-- Access.

	nLocations :: Size.NLocations
	nLocations	= Data.Map.size locationCatalogue

	nStudents :: Size.NStudents
	nStudents	= Aggregate.StudentClass.getSize $ Aggregate.StudentBodyRegister.getStudentBodies studentBodyRegister

	nTeachers :: Size.NTeachers
	nTeachers	= Data.Map.size teacherRegister

	(distinctCourses, noTeacherHasMoreThanOneLesson)	= Aggregate.TeacherRegister.extractDistinctCourses &&& Data.Foldable.all (
		(<= 1) . Data.Foldable.foldr (
			(+) . Data.Course.getRequiredLessonsPerWeek
		) 0 . Data.Teacher.getService
	 ) $ teacherRegister

	hasAnyFacilities, isSingleCampus, hasAnyOptionalKnowledgeRequirements, hasAnyCourseMaximumClassSizes, hasAnySynchronisedCourses, requireAnyFacilities :: Bool
	(hasAnyFacilities, isSingleCampus)				= Aggregate.LocationCatalogue.hasAnyFacilities &&& Aggregate.LocationCatalogue.isSingleCampus $ locationCatalogue
	hasAnyOptionalKnowledgeRequirements				= Aggregate.StudentBodyRegister.hasAnyOptionalKnowledgeRequirements studentBodyRegister
	(hasAnyCourseMaximumClassSizes, hasAnySynchronisedCourses)	= Aggregate.TeacherRegister.hasAnyCourseMaximumClassSizes &&& Aggregate.TeacherRegister.hasAnySynchronisedCourses $ teacherRegister
	requireAnyFacilities						= Data.Foldable.any (not . Data.Set.null . Data.Course.getRequiredFacilityNames) distinctCourses

	smallestMinimumConsecutivePeriods :: Size.NTimeslots
	smallestMinimumConsecutivePeriods	= Data.Foldable.minimum $ Data.Set.map Data.Course.getMinimumConsecutiveLessons distinctCourses

	(lessonCriteriaWeights, zeroedLessonCriterionWeightTags)	= Control.Monad.Writer.runWriter $ criteriaWeightsMutator (
		ExecutionConfiguration.ExecutionOptions.getLessonCriteriaWeights executionOptions
	 ) [
		(
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfGreatestMinimumConsecutiveLessons,
				zeroNonCritical && not (
					ProblemConfiguration.ProblemParameters.hasVariousMinimumConsecutiveLessons problemParameters
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfGreatestMinimumConsecutiveLessons,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfGreatestMinimumConsecutiveLessonsTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfGreatestSynchronisedCourseSetSize,
				zeroNonCritical && not hasAnySynchronisedCourses	-- Singleton synchronised courses are trapped by ProblemParameters.
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfGreatestSynchronisedCourseSetSize,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfGreatestSynchronisedCourseSetSizeTag
			)
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfIsCoreKnowledgeRequirement,
				zeroNonCritical && not (
					Aggregate.StudentBodyRegister.hasAnyCoreKnowledgeRequirements studentBodyRegister {-enforced by DTD, but not by database-} && hasAnyOptionalKnowledgeRequirements
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfIsCoreKnowledgeRequirement,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfIsCoreKnowledgeRequirementTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfIsSpecialistInTopic,
				zeroNonCritical && (
					nTeachers == 1 || not (Aggregate.TeacherRegister.hasAnySpecialists teacherRegister)
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfIsSpecialistInTopic,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfIsSpecialistInTopicTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMatchCourseClassSizeToLocationCapacity,
				zeroNonCritical && (
					nLocations == 1 || not hasAnyCourseMaximumClassSizes
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMatchCourseClassSizeToLocationCapacity,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMatchCourseClassSizeToLocationCapacityTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseRelativeFacilityUtilisation,
				zeroNonCritical && not (
					hasAnyFacilities && requireAnyFacilities
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMaximiseRelativeFacilityUtilisation,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMaximiseRelativeFacilityUtilisationTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverCourseClassSize,
				zeroNonCritical && or [
					Data.Map.size studentBodyRegister == 1,
					not hasAnyCourseMaximumClassSizes,
					Data.Foldable.all (
						Data.Maybe.maybe True (
							>= nStudents -- Compare each maximum class-size with the total number of students.
						) . Data.Course.getMaybeMaximumClassSize
					) distinctCourses
				]
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMaximiseStudentClassSizeOverCourseClassSize,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMaximiseStudentClassSizeOverCourseClassSizeTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseStudentClassSizeOverLocationCapacity,
				zeroNonCritical && nLocations == 1	-- CAVEAT: if there's only one student-body, it's still better to book them into a location of appropriate capacity, since it reduces the heating-bill.
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMaximiseStudentClassSizeOverLocationCapacity,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMaximiseStudentClassSizeOverLocationCapacityTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
				zeroNonCritical && not (Aggregate.TeacherRegister.hasAnySpecificTimeRequests teacherRegister)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseBookingAtAnotherCoursesSpecifiedTimeTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseBookingOfLocationByOtherTeachers,
				zeroNonCritical && (
					nLocations == 1 || nTeachers == 1
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMinimiseBookingOfLocationByOtherTeachers,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseBookingOfLocationByOtherTeachersTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseDeviationFromTimeslotRequest,
				zeroNonCritical && not (Aggregate.TeacherRegister.hasAnyTimeslotRequests teacherRegister)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMinimiseDeviationFromTimeslotRequest,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseDeviationFromTimeslotRequestTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseInterCampusMigrationsOfStudents,
				zeroNonCritical && (
					nTimeslotsPerDay <= smallestMinimumConsecutivePeriods || isSingleCampus
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMinimiseInterCampusMigrationsOfStudents,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseInterCampusMigrationsOfStudentsTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseInterCampusMigrationsOfTeachers,
				zeroNonCritical && (
					nTimeslotsPerDay <= smallestMinimumConsecutivePeriods || isSingleCampus
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMinimiseInterCampusMigrationsOfTeachers,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseInterCampusMigrationsOfTeachersTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseTeachersLocusOperandi,
				zeroNonCritical && (
					nLocations == 1 || noTeacherHasMoreThanOneLesson
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMinimiseTeachersLocusOperandi,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseTeachersLocusOperandiTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseWasteOfScarceFacilities,
				zeroNonCritical && not (
					hasAnyFacilities && requireAnyFacilities
				)
			), (
				ExecutionConfiguration.LessonCriteriaWeights.zeroWeightOfMinimiseWasteOfScarceFacilities,
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseWasteOfScarceFacilitiesTag
			) -- Pair.
		) -- Pair.
	 ]

	(timetableCriteriaWeights, zeroedTimetableCriterionWeightTags)	= Control.Monad.Writer.runWriter $ criteriaWeightsMutator (
		ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights executionOptions
	 ) [
		(
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseComplianceWithFreePeriodPreferences,
				zeroNonCritical && (
					nTimeslotsPerDay <= smallestMinimumConsecutivePeriods || not (
						ProblemConfiguration.ProblemParameters.hasAnyFreePeriodPreference problemParameters
					)
				)
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMaximiseComplianceWithFreePeriodPreferences,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMaximiseComplianceWithFreePeriodPreferencesTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseSynchronisationOfSynchronisedCourses,
				zeroNonCritical && not hasAnySynchronisedCourses	-- Singleton synchronised courses are trapped by ProblemParameters.
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMaximiseSynchronisationOfSynchronisedCourses,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMaximiseSynchronisationOfSynchronisedCoursesTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
				zeroNonCritical && (
					nTimeslotsPerDay <= smallestMinimumConsecutivePeriods || not (
						Aggregate.TeacherRegister.hasAnyIdealTimeslotRequests teacherRegister
					)
				)
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequestTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay,
				zeroNonCritical && Data.Foldable.all (
					(== 1) . Data.Student.getTeachingRatio
				) studentBodyRegister
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMinimiseDispersionOfStudentFreePeriodsPerDay,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseDispersionOfStudentFreePeriodsPerDayTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanInterCampusMigrationsOfStudents,
				zeroNonCritical && (
					nTimeslotsPerDay <= smallestMinimumConsecutivePeriods || isSingleCampus
				)
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMinimiseMeanInterCampusMigrationsOfStudents,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanInterCampusMigrationsOfStudentsTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers,
				zeroNonCritical && (
					nTimeslotsPerDay <= smallestMinimumConsecutivePeriods || isSingleCampus
				)
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMinimiseMeanInterCampusMigrationsOfTeachers,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanInterCampusMigrationsOfTeachersTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanLocationChangesOfTeachers,
				zeroNonCritical && (
					nLocations == 1 || noTeacherHasMoreThanOneLesson
				)
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMinimiseMeanLocationChangesOfTeachers,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanLocationChangesOfTeachersTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanLocusOperandiOfTeachers,
				zeroNonCritical && (
					nLocations == 1 || noTeacherHasMoreThanOneLesson
				)
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMinimiseMeanLocusOperandiOfTeachers,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanLocusOperandiOfTeachersTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
				zeroNonCritical && not hasAnyOptionalKnowledgeRequirements
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledgeTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseRatioOfConsecutiveEqualLessons,
				nTimeslotsPerDay <= smallestMinimumConsecutivePeriods	-- CAVEAT: required to prevent an attempt to divide by zero.
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMinimiseRatioOfConsecutiveEqualLessons,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseRatioOfConsecutiveEqualLessonsTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay,
				nTimeslotsPerDay <= 2 * smallestMinimumConsecutivePeriods	-- CAVEAT: required to prevent an attempt to divide by zero.
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.zeroWeightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDay,
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseRatioOfSeparatedEqualLessonsWithinAnyDayTag
			) -- Pair.
		) -- Pair.
	 ]

	(evolutionStrategies, zeroedEvolutionStrategyFecundityTags)	= Control.Monad.Writer.runWriter . foldr (
		(
			\(mutator, message)	-> (
				Control.Monad.Writer.tell [message] >>	-- Log the tag.
			) . fmap mutator	-- Apply the mutator.
		) . snd
	 ) (
		return {-to Writer-monad-} $ ExecutionConfiguration.ExecutionOptions.getEvolutionStrategies executionOptions	-- Initial value.
	 ) $ filter (
		uncurry (&&) . Control.Arrow.first (
			(/= ExecutionConfiguration.TimetableBreederFecundity.zero) . ($ ExecutionConfiguration.ExecutionOptions.getEvolutionStrategies executionOptions)
		) . fst {-conditions-}
	 ) [
		(
			(
				ExecutionConfiguration.EvolutionStrategies.getSynchronisedCourseMutationFecundity,
				zeroNonCritical && not hasAnySynchronisedCourses	-- Singleton synchronised courses are trapped by ProblemParameters.
			), (
				ExecutionConfiguration.EvolutionStrategies.zeroSynchronisedCourseMutationFecundity,
				ExecutionConfiguration.EvolutionStrategies.synchronisedCourseMutationTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.EvolutionStrategies.getSynchronisedCourseByDayMutationFecundity,
				zeroNonCritical && not hasAnySynchronisedCourses	-- Singleton synchronised courses are trapped by ProblemParameters.
			), (
				ExecutionConfiguration.EvolutionStrategies.zeroSynchronisedCourseByDayMutationFecundity,
				ExecutionConfiguration.EvolutionStrategies.synchronisedCourseByDayMutationTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.EvolutionStrategies.getExcessRunlengthMutationFecundity,
				zeroNonCritical && nTimeslotsPerDay <= smallestMinimumConsecutivePeriods
			), (
				ExecutionConfiguration.EvolutionStrategies.zeroExcessRunlengthMutationFecundity,
				ExecutionConfiguration.EvolutionStrategies.excessRunlengthMutationTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.EvolutionStrategies.getSingletonStudentClassMutationFecundity,
				zeroNonCritical && not permitTemporaryStudentBodyMerger
			), (
				ExecutionConfiguration.EvolutionStrategies.zeroSingletonStudentClassMutationFecundity,
				ExecutionConfiguration.EvolutionStrategies.singletonStudentClassMutationTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.EvolutionStrategies.getSplitSessionMutationFecundity,
				zeroNonCritical && nTimeslotsPerDay <= 2 * smallestMinimumConsecutivePeriods
			), (
				ExecutionConfiguration.EvolutionStrategies.zeroSplitSessionMutationFecundity,
				ExecutionConfiguration.EvolutionStrategies.splitSessionMutationTag
			) -- Pair.
		), (
			(
				ExecutionConfiguration.EvolutionStrategies.getStudentBodyCombinationMutationFecundity,
				zeroNonCritical && not permitTemporaryStudentBodyMerger
			), (
				ExecutionConfiguration.EvolutionStrategies.zeroStudentBodyCombinationMutationFecundity,
				ExecutionConfiguration.EvolutionStrategies.studentBodyCombinationMutationTag
			) -- Pair.
		) -- Pair.
	 ]

instance (
	Enum		timeslotId,
	Eq		criterionWeight,
	Num		criterionWeight,
	Ord		campus,
	Ord		level,
	Ord		synchronisationId,
	Ord		teacherId,
	Ord		timeslotId,
	RealFrac	teachingRatio,
	Show		level,
	Show		synchronisationId,
	Show		teacherId,
	Show		timeslotId
 ) => Configuration.Configuration (Options campus criterionWeight fecundityDecayRatio level locationId minimumContrastRatio populationDiversityRatio stream synchronisationId teacherId teachingRatio timeslotId) where
	issueWarnings inputOptions	= Configuration.issueWarnings executionOptions ++ Configuration.issueWarnings problemParameters ++ [
		message ++ ", but the weight assigned to the lesson-criterion " ++ show criterionTag ++ " is zero" | (criterionWeightAccessor, condition, message, criterionTag)	<- [
			(
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfGreatestSynchronisedCourseSetSize,
				hasAnySynchronisedCourses,
				"synchronised courses have been defined",
				ExecutionConfiguration.LessonCriteriaWeights.weightOfGreatestSynchronisedCourseSetSizeTag
			), (
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfIsSpecialistInTopic,
				Aggregate.TeacherRegister.hasAnySpecialists teacherRegister,
				"topic-specialists have been defined",
				ExecutionConfiguration.LessonCriteriaWeights.weightOfIsSpecialistInTopicTag
			), (
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMaximiseRelativeFacilityUtilisation,
				hasAnyFacilities,
				"facilities have been defined in various locations",
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMaximiseRelativeFacilityUtilisationTag
			), (
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseWasteOfScarceFacilities,
				hasAnyFacilities,
				"facilities have been defined in various locations",
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseWasteOfScarceFacilitiesTag
			), (
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseBookingAtAnotherCoursesSpecifiedTime,
				hasAnyTimeslotRequests,
				"time-slots have been requested by teachers",
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseBookingAtAnotherCoursesSpecifiedTimeTag
			), (
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseDeviationFromTimeslotRequest,
				hasAnyTimeslotRequests,
				"time-slots have been requested by teachers",
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseDeviationFromTimeslotRequestTag
			), (
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseInterCampusMigrationsOfStudents,
				not isSingleCampus,
				"locations have been partitioned into different campuses",
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseInterCampusMigrationsOfStudentsTag
			), (
				ExecutionConfiguration.LessonCriteriaWeights.getWeightOfMinimiseInterCampusMigrationsOfTeachers,
				not isSingleCampus,
				"locations have been partitioned into different campuses",
				ExecutionConfiguration.LessonCriteriaWeights.weightOfMinimiseInterCampusMigrationsOfTeachersTag
			) -- Quadruple.
		],
		criterionWeightAccessor lessonCriteriaWeights == minBound,
		condition
	 ] ++ [
		message ++ ", but the weight assigned to the timetable-criterion " ++ show criterionTag ++ " is zero" | (criterionWeightAccessor, condition, message, criterionTag)	<- [
			(
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseSynchronisationOfSynchronisedCourses,
				hasAnySynchronisedCourses,
				"synchronised courses have been defined",
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMaximiseSynchronisationOfSynchronisedCoursesTag
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequest,
				Aggregate.TeacherRegister.hasAnyIdealTimeslotRequests teacherRegister,
				"ideal time-slots have been specified for courses",
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseAverageAbsoluteDeviationFromIdealTimeslotRequestTag
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMaximiseComplianceWithFreePeriodPreferences,
				ProblemConfiguration.ProblemParameters.hasAnyFreePeriodPreference problemParameters,
				"free-period preferences have been expressed",
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMaximiseComplianceWithFreePeriodPreferencesTag
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanInterCampusMigrationsOfStudents,
				not isSingleCampus,
				"locations have been partitioned into different campuses",
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanInterCampusMigrationsOfStudentsTag
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanInterCampusMigrationsOfTeachers,
				not isSingleCampus,
				"locations have been partitioned into different campuses",
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanInterCampusMigrationsOfTeachersTag
			), (
				ExecutionConfiguration.TimetableCriteriaWeights.getWeightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledge,
				Aggregate.StudentBodyRegister.hasAnyOptionalKnowledgeRequirements studentBodyRegister,
				"student-bodies have specified optional knowledge-requirements",
				ExecutionConfiguration.TimetableCriteriaWeights.weightOfMinimiseMeanRatioOfIncompletelyBookedOptionalKnowledgeTag
			) -- Quadruple.
		],
		criterionWeightAccessor timetableCriteriaWeights == minBound,
		condition
	 ] where
		(executionOptions, problemParameters)			= getExecutionOptions &&& getProblemParameters $ inputOptions
		(lessonCriteriaWeights, timetableCriteriaWeights)	= ExecutionConfiguration.ExecutionOptions.getLessonCriteriaWeights &&& ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights $ executionOptions
		(studentBodyRegister, teacherRegister)			= ProblemConfiguration.ProblemParameters.getStudentBodyRegister &&& ProblemConfiguration.ProblemParameters.getTeacherRegister $ problemParameters
		locationCatalogue					= ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters
		(hasAnyFacilities, isSingleCampus)			= Aggregate.LocationCatalogue.hasAnyFacilities &&& Aggregate.LocationCatalogue.isSingleCampus $ locationCatalogue
		(hasAnySynchronisedCourses, hasAnyTimeslotRequests)	= Aggregate.TeacherRegister.hasAnySynchronisedCourses &&& Aggregate.TeacherRegister.hasAnyTimeslotRequests $ teacherRegister

