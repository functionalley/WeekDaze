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

 [@DESCRIPTION@]	Defines the extent to which various strategies are applied in an attempt to evolve the /timetable/.
-}

module WeekDaze.ExecutionConfiguration.EvolutionStrategies(
-- * Types
-- ** Type-synonyms
--	Mutator,
-- ** Data-types
	EvolutionStrategies(..),
-- * Constants
--	tag,
	synchronisedCourseMutationTag,
	synchronisedCourseByDayMutationTag,
	excessRunlengthMutationTag,
	homogeneousStudentViewLessonMutationTag,
	incompleteCourseMutationTag,
	randomLessonMutationTag,
	singletonStudentClassMutationTag,
	splitSessionMutationTag,
	studentBodyCombinationMutationTag,
	studentViewTimetableForDayMutationTag,
	studentViewTimetableForWeekMutationTag,
	synchronousLessonMutationTag,
--	randomLessonMutationNTrialsTag,
--	randomLessonMutationNTimeslotsTag,
--	studentViewTimetableForDayMutationNDaysTag,
--	studentViewTimetableForWeekMutationNTrialsTag,
--	studentViewTimetableForWeekMutationNTimeslotsTag,
	fecundityDecayRatioTag,
	minimumPopulationDiversityRatioTag,
	nInitialScoutsTag,
-- * Functions
-- ** Mutators
	zeroSynchronisedCourseMutationFecundity,
	zeroSynchronisedCourseByDayMutationFecundity,
	zeroExcessRunlengthMutationFecundity,
--	zeroHomogeneousStudentViewLessonMutationFecundity,
--	zeroIncompleteCourseMutationFecundity,
--	zeroRandomLessonMutationFecundity,
	zeroSingletonStudentClassMutationFecundity,
	zeroSplitSessionMutationFecundity,
	zeroStudentBodyCombinationMutationFecundity,
--	zeroStudentViewTimetableForDayMutationFecundity,
--	zeroStudentViewTimetableForWeekMutationFecundity,
--	zeroSynchronousLessonMutationFecundity,
-- ** Predicates
	areAllZero
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Text.XML.HXT.Arrow.Pickle					as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.ExecutionConfiguration.TimetableBreederFecundity	as ExecutionConfiguration.TimetableBreederFecundity
import qualified	WeekDaze.Size							as Size
import qualified	WeekDaze.Temporal.Day						as Temporal.Day

#ifdef USE_HDBC
import			Control.Arrow((&&&))
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector					as Database.Selector

instance (
	Data.Convertible.Convertible	Database.HDBC.SqlValue fecundityDecayRatio,		-- Flexible context.
	Data.Convertible.Convertible	Database.HDBC.SqlValue populationDiversityRatio,	-- Flexible context.
	Data.Typeable.Typeable		fecundityDecayRatio,
	Data.Typeable.Typeable		populationDiversityRatio,
	RealFrac			fecundityDecayRatio,
	RealFrac			populationDiversityRatio
 ) => Database.Selector.Selector (EvolutionStrategies fecundityDecayRatio populationDiversityRatio) where
	fromDatabase connection	projectIdSql	= let
		tableName :: Database.Selector.TableName
		tableName	= Database.Selector.tablePrefix ++ tag
	 in do
		strategiesRows	<- Database.Selector.select connection [
			"synchronisedCourseMutationDeterministicCTor",
			"synchronisedCourseMutationRandomCTor",
			"synchronisedCourseByDayMutationDeterministicCTor",
			"synchronisedCourseByDayMutationRandomCTor",
			"excessRunlengthMutationDeterministicCTor",
			"excessRunlengthMutationRandomCTor",
			"homogeneousStudentViewLessonMutationDeterministicCTor",
			"homogeneousStudentViewLessonMutationRandomCTor",
			"incompleteCourseMutationDeterministicCTor",
			"incompleteCourseMutationRandomCTor",
			"randomLessonMutationDeterministicCTor",
			"randomLessonMutationRandomCTor",
			"singletonStudentClassMutationDeterministicCTor",
			"singletonStudentClassMutationRandomCTor",
			"splitSessionMutationDeterministicCTor",
			"splitSessionMutationRandomCTor",
			"studentBodyCombinationMutationDeterministicCTor",
			"studentBodyCombinationMutationRandomCTor",
			"studentViewTimetableForDayMutationDeterministicCTor",
			"studentViewTimetableForDayMutationRandomCTor",
			"studentViewTimetableForWeekMutationDeterministicCTor",
			"studentViewTimetableForWeekMutationRandomCTor",
			"synchronousLessonMutationDeterministicCTor",
			"synchronousLessonMutationRandomCTor",
			randomLessonMutationNTrialsTag,
			randomLessonMutationNTimeslotsTag,
			studentViewTimetableForDayMutationNDaysTag,
			studentViewTimetableForWeekMutationNTrialsTag,
			studentViewTimetableForWeekMutationNTimeslotsTag,
			fecundityDecayRatioTag,
			minimumPopulationDiversityRatioTag,
			nInitialScoutsTag
		 ] [tableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		return {-to IO-Monad-} $ case strategiesRows of
			[]		-> Data.Default.def
			[strategiesRow]	-> case strategiesRow of
				[
					synchronisedCourseMutationDeterministicConstructorFecunditySql,
					synchronisedCourseMutationRandomConstructorFecunditySql,
					synchronisedCourseByDayMutationDeterministicConstructorFecunditySql,
					synchronisedCourseByDayMutationRandomConstructorFecunditySql,
					excessRunlengthMutationDeterministicConstructorFecunditySql,
					excessRunlengthMutationRandomConstructorFecunditySql,
					homogeneousStudentViewLessonMutationDeterministicConstructorFecunditySql,
					homogeneousStudentViewLessonMutationRandomConstructorFecunditySql,
					incompleteCourseMutationDeterministicConstructorFecunditySql,
					incompleteCourseMutationRandomConstructorFecunditySql,
					randomLessonMutationDeterministicConstructorFecunditySql,
					randomLessonMutationRandomConstructorFecunditySql,
					singletonStudentClassMutationDeterministicConstructorFecunditySql,
					singletonStudentClassMutationRandomConstructorFecunditySql,
					splitSessionMutationDeterministicConstructorFecunditySql,
					splitSessionMutationRandomConstructorFecunditySql,
					studentBodyCombinationMutationDeterministicConstructorFecunditySql,
					studentBodyCombinationMutationRandomConstructorFecunditySql,
					studentViewTimetableForDayMutationDeterministicConstructorFecunditySql,
					studentViewTimetableForDayMutationRandomConstructorFecunditySql,
					studentViewTimetableForWeekMutationDeterministicConstructorFecunditySql,
					studentViewTimetableForWeekMutationRandomConstructorFecunditySql,
					synchronousLessonMutationDeterministicConstructorFecunditySql,
					synchronousLessonMutationRandomConstructorFecunditySql,
					randomLessonMutationNTrialsSql,
					randomLessonMutationNTimeslotsSql,
					studentViewTimetableForDayMutationNDaysSql,
					studentViewTimetableForWeekMutationNTrialsSql,
					studentViewTimetableForWeekMutationNTimeslotsSql,
					fecundityDecayRatioSql,
					minimumPopulationDiversityRatioSql,
					nInitialScoutsSql
				 ] -> let
					mkTimetableBreederFecundity x y	= ExecutionConfiguration.TimetableBreederFecundity.mkTimetableBreederFecundity (
						Data.Maybe.fromMaybe defaultDeterministicFecundity . either (
							error . showString "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase.mkTimetableBreederFecundity:\tfailed to parse the value for the 'deterministic fecundity' read from the database; " . show
						) id $ Database.HDBC.safeFromSql x
					 ) (
						Data.Maybe.fromMaybe defaultRandomFecundity . either (
							error . showString "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase.mkTimetableBreederFecundity:\tfailed to parse the value for the 'random fecundity' read from the database; " . show
						) id $ Database.HDBC.safeFromSql y
					 ) where
						(defaultDeterministicFecundity, defaultRandomFecundity)	= ExecutionConfiguration.TimetableBreederFecundity.getDeterministicConstructorFecundity &&& ExecutionConfiguration.TimetableBreederFecundity.getRandomConstructorFecundity $ Data.Default.def

					def	= Data.Default.def
				 in MkEvolutionStrategies {
					getSynchronisedCourseMutationFecundity			= mkTimetableBreederFecundity synchronisedCourseMutationDeterministicConstructorFecunditySql synchronisedCourseMutationRandomConstructorFecunditySql,
					getSynchronisedCourseByDayMutationFecundity		= mkTimetableBreederFecundity synchronisedCourseByDayMutationDeterministicConstructorFecunditySql synchronisedCourseByDayMutationRandomConstructorFecunditySql,
					getExcessRunlengthMutationFecundity			= mkTimetableBreederFecundity excessRunlengthMutationDeterministicConstructorFecunditySql excessRunlengthMutationRandomConstructorFecunditySql,
					getHomogeneousStudentViewLessonMutationFecundity	= mkTimetableBreederFecundity homogeneousStudentViewLessonMutationDeterministicConstructorFecunditySql homogeneousStudentViewLessonMutationRandomConstructorFecunditySql,
					getIncompleteCourseMutationFecundity			= mkTimetableBreederFecundity incompleteCourseMutationDeterministicConstructorFecunditySql incompleteCourseMutationRandomConstructorFecunditySql,
					getRandomLessonMutationFecundity			= mkTimetableBreederFecundity randomLessonMutationDeterministicConstructorFecunditySql randomLessonMutationRandomConstructorFecunditySql,
					getSingletonStudentClassMutationFecundity		= mkTimetableBreederFecundity singletonStudentClassMutationDeterministicConstructorFecunditySql singletonStudentClassMutationRandomConstructorFecunditySql,
					getSplitSessionMutationFecundity			= mkTimetableBreederFecundity splitSessionMutationDeterministicConstructorFecunditySql splitSessionMutationRandomConstructorFecunditySql,
					getStudentBodyCombinationMutationFecundity		= mkTimetableBreederFecundity studentBodyCombinationMutationDeterministicConstructorFecunditySql studentBodyCombinationMutationRandomConstructorFecunditySql,
					getStudentViewTimetableForDayMutationFecundity		= mkTimetableBreederFecundity studentViewTimetableForDayMutationDeterministicConstructorFecunditySql studentViewTimetableForDayMutationRandomConstructorFecunditySql,
					getStudentViewTimetableForWeekMutationFecundity		= mkTimetableBreederFecundity studentViewTimetableForWeekMutationDeterministicConstructorFecunditySql studentViewTimetableForWeekMutationRandomConstructorFecunditySql,
					getSynchronousLessonMutationFecundity			= mkTimetableBreederFecundity synchronousLessonMutationDeterministicConstructorFecunditySql synchronousLessonMutationRandomConstructorFecunditySql,
					getRandomLessonMutationNTrials				= Data.Maybe.fromMaybe (getRandomLessonMutationNTrials def) . either (
						error . showString "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase:\tfailed to parse the value for " . shows randomLessonMutationNTrialsTag . showString " read from the database; " . show
					) id $ Database.HDBC.safeFromSql randomLessonMutationNTrialsSql,
					getRandomLessonMutationNTimeslots			= Data.Maybe.fromMaybe (getRandomLessonMutationNTimeslots def) . either (
						error . showString "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase:\tfailed to parse the value for " . shows randomLessonMutationNTimeslotsTag . showString " read from the database; " . show
					) id $ Database.HDBC.safeFromSql randomLessonMutationNTimeslotsSql,
					getStudentViewTimetableForDayMutationMaybeNDays		= either (
						error . showString "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase:\tfailed to parse the value for " . shows studentViewTimetableForDayMutationNDaysTag . showString " read from the database; " . show
					) id $ Database.HDBC.safeFromSql studentViewTimetableForDayMutationNDaysSql,	-- Returns 'Nothing' for SqlNull.
					getStudentViewTimetableForWeekMutationNTrials		= Data.Maybe.fromMaybe (getStudentViewTimetableForWeekMutationNTrials def) . either (
						error . showString "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase:\tfailed to parse the value for " . shows studentViewTimetableForWeekMutationNTrialsTag . showString " read from the database; " . show
					) id $ Database.HDBC.safeFromSql studentViewTimetableForWeekMutationNTrialsSql,
					getStudentViewTimetableForWeekMutationNTimeslots	= Data.Maybe.fromMaybe (getStudentViewTimetableForWeekMutationNTimeslots def) . either (
						error . showString "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase:\tfailed to parse the value for " . shows studentViewTimetableForWeekMutationNTimeslotsTag . showString " read from the database; " . show
					) id $ Database.HDBC.safeFromSql studentViewTimetableForWeekMutationNTimeslotsSql,
					getFecundityDecayRatio					= Database.Selector.fromSqlFractional (getFecundityDecayRatio def) fecundityDecayRatioSql,
					getMinimumPopulationDiversityRatio			= Database.Selector.fromSqlFractional (getMinimumPopulationDiversityRatio def) minimumPopulationDiversityRatioSql,
					getMaybeNInitialScouts					= either (
						error . showString "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase:\tfailed to parse the value for " . shows nInitialScoutsTag . showString " read from the database; " . show
					) id $ Database.HDBC.safeFromSql nInitialScoutsSql	-- Returns 'Nothing' for SqlNull.
				}
				_ -> error $ "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase:\tunexpected number of columns=" ++ show (length strategiesRow) ++ " in row of table " ++ show tableName ++ "."
			_		-> error $ "WeekDaze.ExecutionConfiguration.EvolutionStrategies.fromDatabase:\tunexpected number of rows=" ++ show (length strategiesRows) ++ " selected from table " ++ show tableName ++ "."
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag							= "evolutionStrategies"

-- | Used to qualify XML.
synchronisedCourseMutationTag :: String
synchronisedCourseMutationTag				= "synchronisedCourseMutation"

-- | Used to qualify XML.
synchronisedCourseByDayMutationTag :: String
synchronisedCourseByDayMutationTag			= "synchronisedCourseByDayMutation"

-- | Used to qualify XML.
excessRunlengthMutationTag :: String
excessRunlengthMutationTag				= "excessRunlengthMutation"

-- | Used to qualify XML.
homogeneousStudentViewLessonMutationTag :: String
homogeneousStudentViewLessonMutationTag			= "homogeneousStudentViewLessonMutation"

-- | Used to qualify XML.
incompleteCourseMutationTag :: String
incompleteCourseMutationTag				= "incompleteCourseMutation"

-- | Used to qualify XML.
randomLessonMutationTag :: String
randomLessonMutationTag					= "randomLessonMutation"

-- | Used to qualify XML.
singletonStudentClassMutationTag :: String
singletonStudentClassMutationTag			= "singletonStudentClassMutation"

-- | Used to qualify XML.
splitSessionMutationTag :: String
splitSessionMutationTag					= "splitSessionMutation"

-- | Used to qualify XML.
studentBodyCombinationMutationTag :: String
studentBodyCombinationMutationTag			= "studentBodyCombinationMutation"

-- | Used to qualify XML.
studentViewTimetableForDayMutationTag :: String
studentViewTimetableForDayMutationTag			= "studentViewTimetableForDayMutation"

-- | Used to qualify XML.
studentViewTimetableForWeekMutationTag :: String
studentViewTimetableForWeekMutationTag			= "studentViewTimetableForWeekMutation"

-- | Used to qualify XML.
synchronousLessonMutationTag :: String
synchronousLessonMutationTag				= "synchronousLessonMutation"

-- | Used to qualify SQL & XML.
randomLessonMutationNTrialsTag :: String
randomLessonMutationNTrialsTag				= "randomLessonMutationNTrials"

-- | Used to qualify SQL & XML.
randomLessonMutationNTimeslotsTag :: String
randomLessonMutationNTimeslotsTag			= "randomLessonMutationNTimeslots"

-- | Used to qualify SQL & XML.
studentViewTimetableForDayMutationNDaysTag :: String
studentViewTimetableForDayMutationNDaysTag		= "studentViewTimetableForDayMutationNDays"

-- | Used to qualify SQL & XML.
studentViewTimetableForWeekMutationNTrialsTag :: String
studentViewTimetableForWeekMutationNTrialsTag		= "studentViewTimetableForWeekMutationNTrials"

-- | Used to qualify SQL & XML.
studentViewTimetableForWeekMutationNTimeslotsTag :: String
studentViewTimetableForWeekMutationNTimeslotsTag	= "studentViewTimetableForWeekMutationNTimeslots"

-- | Used to qualify XML.
fecundityDecayRatioTag :: String
fecundityDecayRatioTag					= "fecundityDecayRatio"

-- | Used to qualify XML.
minimumPopulationDiversityRatioTag :: String
minimumPopulationDiversityRatioTag			= "minimumPopulationDiversityRatio"

-- | Used to qualify XML.
nInitialScoutsTag :: String
nInitialScoutsTag					= "nInitialScouts"

-- | Defines the various strategies used to evolve the /timetable/.
data EvolutionStrategies fecundityDecayRatio populationDiversityRatio = MkEvolutionStrategies {
	getSynchronisedCourseMutationFecundity			:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking the /lesson/s of /synchronised course/s, before (deterministically, randomly) reconstructing.
	getSynchronisedCourseByDayMutationFecundity		:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking synchronous groups of /lesson/s for /synchronised course/s, before (deterministically, randomly) reconstructing.
	getExcessRunlengthMutationFecundity			:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking the terminal /lesson/s of excessive runlengths, before (deterministically, randomly) reconstructing.
	getHomogeneousStudentViewLessonMutationFecundity	:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking groups of homogeneous /student-view lesson/s, before (deterministically, randomly) reconstructing.
	getIncompleteCourseMutationFecundity			:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking all the /lesson/s for incomplete /course/s, in the /timetable/ for each /student/, before (deterministically, randomly) reconstructing.
	getRandomLessonMutationFecundity			:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking random /lesson/s from the /timetable/, before (deterministically, randomly) reconstructing.
	getSingletonStudentClassMutationFecundity		:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking /lesson/s whose /student-classes/ have been composed from a single /student-body/, before (deterministically, randomly) reconstructing.
	getSplitSessionMutationFecundity			:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking runlengths of /lesson/s which occur in split sessions within a /day/, before (deterministically, randomly) reconstructing.
	getStudentBodyCombinationMutationFecundity		:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking /lesson/s for a /course/, whose /student-classes/ have been composed from more than one combination of /student-bodies/, before (deterministically, randomly) reconstructing.
	getStudentViewTimetableForDayMutationFecundity		:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking a whole day, from each /studentViewTimetableForWeek/ in isolation, before (deterministically, randomly) reconstructing.
	getStudentViewTimetableForWeekMutationFecundity		:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking a small number of randomly selected /lesson/s, from each /studentViewTimetableForWeek/ in isolation, before (deterministically, randomly) reconstructing.
	getSynchronousLessonMutationFecundity			:: ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,	-- ^ Whether to attempt to evolve the /timetable/, by unbooking groups of synchronous of heterogeneous /lesson/s.
	getRandomLessonMutationNTrials				:: Int,										-- ^ The number of random trials to perform in 'getRandomLessonMutationFecundity'.
	getRandomLessonMutationNTimeslots			:: Size.NTimeslots,								-- ^ The number of /time-slot/s to randomly unbook in 'getRandomLessonMutationFecundity'.
	getStudentViewTimetableForDayMutationMaybeNDays		:: Maybe Size.NDays,								-- ^ The optional number of /day/s to unbook in 'getStudentViewTimetableForDayMutationFecundity'; if 'Nothing' then all possible values are tried.
	getStudentViewTimetableForWeekMutationNTrials		:: Int,										-- ^ The number of random trials to perform in 'getStudentViewTimetableForWeekMutationFecundity'.
	getStudentViewTimetableForWeekMutationNTimeslots	:: Size.NTimeslots,								-- ^ The number of /time-slot/s to randomly unbook in 'getStudentViewTimetableForWeekMutationFecundity'.
	getFecundityDecayRatio					:: fecundityDecayRatio,								-- ^ The factor (in the closed unit-interval) by which the /fecundity/ of the breeding-program for future generations, is multiplied (& therefore reduced), when the /population-diversity ratio/ falls beneath 'getMinimumPopulationDiversityRatio'.
	getMinimumPopulationDiversityRatio			:: populationDiversityRatio,							-- ^ The /population-diversity ratio/ (in the closed unit-interval), beneath which a reduction in the /fecundity/ of the breeding-program for future generations, by a factor of 'getFecundityDecayRatio', is triggered.
	getMaybeNInitialScouts					:: Maybe Size.NTimetables							-- ^ The initial number of candidates to select from each generation in the evolution of the /timetable/; if 'Nothing' then all candidates measuring equal best & better than their parent, will be selected.
} deriving (Eq, Show)

instance (Fractional fecundityDecayRatio, Fractional populationDiversityRatio) => Data.Default.Default (EvolutionStrategies fecundityDecayRatio populationDiversityRatio) where
	def = MkEvolutionStrategies {
		getSynchronisedCourseMutationFecundity			= Data.Default.def,
		getSynchronisedCourseByDayMutationFecundity		= Data.Default.def,
		getExcessRunlengthMutationFecundity			= Data.Default.def,
		getHomogeneousStudentViewLessonMutationFecundity	= Data.Default.def,
		getIncompleteCourseMutationFecundity			= Data.Default.def,
		getRandomLessonMutationFecundity			= Data.Default.def,
		getSingletonStudentClassMutationFecundity		= Data.Default.def,
		getSplitSessionMutationFecundity			= Data.Default.def,
		getStudentBodyCombinationMutationFecundity		= Data.Default.def,
		getStudentViewTimetableForDayMutationFecundity		= Data.Default.def,
		getStudentViewTimetableForWeekMutationFecundity		= Data.Default.def,
		getSynchronousLessonMutationFecundity			= Data.Default.def,
		getRandomLessonMutationNTrials				= 256,
		getRandomLessonMutationNTimeslots			= 3,		-- Empirically found to be approximately optimal; though 4 may be better when the strategy is used in isolation.
		getStudentViewTimetableForDayMutationMaybeNDays		= Nothing,	-- Try all valid values for the number of days.
		getStudentViewTimetableForWeekMutationNTrials		= 16,		-- When multiplied by the number of student-bodies, should be approximately the required fecundity.
		getStudentViewTimetableForWeekMutationNTimeslots	= 4,		-- Empirically found to be approximately optimal.
		getFecundityDecayRatio					= recip 2,	-- Rather arbitrarily.
		getMinimumPopulationDiversityRatio			= recip 2,	-- Rather arbitrarily.
		getMaybeNInitialScouts					= Nothing	-- Select all candidates until the quality drops.
	}

instance (
	Fractional	fecundityDecayRatio,
	Fractional	populationDiversityRatio,
	HXT.XmlPickler	fecundityDecayRatio,
	HXT.XmlPickler	populationDiversityRatio,
	Ord		fecundityDecayRatio,
	Ord		populationDiversityRatio,
	Show		fecundityDecayRatio,
	Show		populationDiversityRatio
 ) => HXT.XmlPickler (EvolutionStrategies fecundityDecayRatio populationDiversityRatio) where
	xpickle	= HXT.xpDefault defaultEvolutionStrategies . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)	-> let
			evolutionStrategies	= MkEvolutionStrategies a b c d e f g h i j k l m n o p q r s t	-- Construct from a tuple.
		in if ToolShed.SelfValidate.isValid evolutionStrategies
			then evolutionStrategies
			else error $ "WeekDaze.ExecutionConfiguration.EvolutionStrategies.xpickle:\t" ++ ToolShed.SelfValidate.getFirstError evolutionStrategies ++ ".",
		\MkEvolutionStrategies {
			getSynchronisedCourseMutationFecundity			= synchronisedCourseMutationFecundity,
			getSynchronisedCourseByDayMutationFecundity		= synchronisedCourseByDayMutationFecundity,
			getExcessRunlengthMutationFecundity			= excessRunlengthMutationFecundity,
			getHomogeneousStudentViewLessonMutationFecundity	= homogeneousStudentViewLessonMutationFecundity,
			getIncompleteCourseMutationFecundity			= incompleteCourseMutationFecundity,
			getRandomLessonMutationFecundity			= randomLessonMutationFecundity,
			getSingletonStudentClassMutationFecundity		= singletonStudentClassMutationFecundity,
			getSplitSessionMutationFecundity			= splitSessionMutationFecundity,
			getStudentBodyCombinationMutationFecundity		= studentBodyCombinationMutationFecundity,
			getStudentViewTimetableForDayMutationFecundity		= studentViewTimetableForDayMutationFecundity,
			getStudentViewTimetableForWeekMutationFecundity		= studentViewTimetableForWeekMutationFecundity,
			getSynchronousLessonMutationFecundity			= synchronousLessonMutationFecundity,
			getRandomLessonMutationNTrials				= randomLessonMutationNTrials,
			getRandomLessonMutationNTimeslots			= randomLessonMutationNTimeslots,
			getStudentViewTimetableForDayMutationMaybeNDays		= studentViewTimetableForDayMutationMaybeNDays,
			getStudentViewTimetableForWeekMutationNTrials		= studentViewTimetableForWeekMutationNTrials,
			getStudentViewTimetableForWeekMutationNTimeslots	= studentViewTimetableForWeekMutationNTimeslots,
			getFecundityDecayRatio					= fecundityDecayRatio,
			getMinimumPopulationDiversityRatio			= minimumPopulationDiversityRatio,
			getMaybeNInitialScouts					= maybeNInitialScouts
		} -> (
			synchronisedCourseMutationFecundity,
			synchronisedCourseByDayMutationFecundity,
			excessRunlengthMutationFecundity,
			homogeneousStudentViewLessonMutationFecundity,
			incompleteCourseMutationFecundity,
			randomLessonMutationFecundity,
			singletonStudentClassMutationFecundity,
			splitSessionMutationFecundity,
			studentBodyCombinationMutationFecundity,
			studentViewTimetableForDayMutationFecundity,
			studentViewTimetableForWeekMutationFecundity,
			synchronousLessonMutationFecundity,
			randomLessonMutationNTrials,
			randomLessonMutationNTimeslots,
			studentViewTimetableForDayMutationMaybeNDays,
			studentViewTimetableForWeekMutationNTrials,
			studentViewTimetableForWeekMutationNTimeslots,
			fecundityDecayRatio,
			minimumPopulationDiversityRatio,
			maybeNInitialScouts
		) -- Deconstruct to a tuple.
	 ) $ HXT.xp20Tuple (
		HXT.xpElem synchronisedCourseMutationTag HXT.xpickle
	 ) (
		HXT.xpElem synchronisedCourseByDayMutationTag HXT.xpickle
	 ) (
		HXT.xpElem excessRunlengthMutationTag HXT.xpickle
	 ) (
		HXT.xpElem homogeneousStudentViewLessonMutationTag HXT.xpickle
	 ) (
		HXT.xpElem incompleteCourseMutationTag HXT.xpickle
	 ) (
		HXT.xpElem randomLessonMutationTag HXT.xpickle
	 ) (
		HXT.xpElem singletonStudentClassMutationTag HXT.xpickle
	 ) (
		HXT.xpElem splitSessionMutationTag HXT.xpickle
	 ) (
		HXT.xpElem studentBodyCombinationMutationTag HXT.xpickle
	 ) (
		HXT.xpElem studentViewTimetableForDayMutationTag HXT.xpickle
	 ) (
		HXT.xpElem studentViewTimetableForWeekMutationTag HXT.xpickle
	 ) (
		HXT.xpElem synchronousLessonMutationTag HXT.xpickle
	 ) (
		HXT.xpDefault (getRandomLessonMutationNTrials defaultEvolutionStrategies) $ HXT.xpAttr randomLessonMutationNTrialsTag HXT.xpInt
	 ) (
		HXT.xpDefault (getRandomLessonMutationNTimeslots defaultEvolutionStrategies) $ HXT.xpAttr randomLessonMutationNTimeslotsTag HXT.xpInt
	 ) (
		HXT.xpOption $ HXT.xpAttr studentViewTimetableForDayMutationNDaysTag HXT.xpInt
	 ) (
		HXT.xpDefault (getStudentViewTimetableForWeekMutationNTrials defaultEvolutionStrategies) $ HXT.xpAttr studentViewTimetableForWeekMutationNTrialsTag HXT.xpInt
	 ) (
		HXT.xpDefault (getStudentViewTimetableForWeekMutationNTimeslots defaultEvolutionStrategies) $ HXT.xpAttr studentViewTimetableForWeekMutationNTimeslotsTag HXT.xpInt
	 ) (
		getFecundityDecayRatio defaultEvolutionStrategies `HXT.xpDefault` HXT.xpAttr fecundityDecayRatioTag HXT.xpickle
	 ) (
		getMinimumPopulationDiversityRatio defaultEvolutionStrategies `HXT.xpDefault` HXT.xpAttr minimumPopulationDiversityRatioTag HXT.xpickle
	 ) (
		HXT.xpOption $ HXT.xpAttr nInitialScoutsTag HXT.xpInt
	 ) where
		defaultEvolutionStrategies	= Data.Default.def

instance (
	Num	fecundityDecayRatio,
	Num	populationDiversityRatio,
	Ord	fecundityDecayRatio,
	Ord	populationDiversityRatio,
	Show	fecundityDecayRatio,
	Show	populationDiversityRatio
 ) => ToolShed.SelfValidate.SelfValidator (EvolutionStrategies fecundityDecayRatio populationDiversityRatio) where
	getErrors evolutionStrategies	= ToolShed.SelfValidate.extractErrors [
		(
			getRandomLessonMutationNTrials evolutionStrategies < 0,
			show randomLessonMutationNTrialsTag ++ " can't be negative; " ++ show evolutionStrategies
		), (
			getRandomLessonMutationNTimeslots evolutionStrategies < 0,
			show randomLessonMutationNTimeslotsTag ++ " can't be negative; " ++ show evolutionStrategies
		), (
			Data.Maybe.maybe False {-NDays unspecified-} (
				\nDays -> any ($ nDays) [(< 0), (> Temporal.Day.nDaysPerWeek)]
			) $ getStudentViewTimetableForDayMutationMaybeNDays evolutionStrategies,
			show studentViewTimetableForDayMutationNDaysTag ++ " (where specified) must be in the closed interval [0, " ++ show Temporal.Day.nDaysPerWeek ++ "]; " ++ show evolutionStrategies
		), (
			getStudentViewTimetableForWeekMutationNTrials evolutionStrategies < 0,
			show studentViewTimetableForWeekMutationNTrialsTag ++ " can't be negative; " ++ show evolutionStrategies
		), (
			getStudentViewTimetableForWeekMutationNTimeslots evolutionStrategies < 0,
			show studentViewTimetableForWeekMutationNTimeslotsTag ++ " can't be negative; " ++ show evolutionStrategies
		), (
			any ($ getFecundityDecayRatio evolutionStrategies) [(< 0), (> 1)],
			show fecundityDecayRatioTag ++ " must be within the closed unit-interval; " ++ show evolutionStrategies
		), (
			any ($ getMinimumPopulationDiversityRatio evolutionStrategies) [(< 0), (> 1)],
			show minimumPopulationDiversityRatioTag ++ " must be within the closed unit-interval; " ++ show evolutionStrategies
		), (
			Data.Maybe.maybe False {-NInitialScouts unspecified-} (< 1) $ getMaybeNInitialScouts evolutionStrategies,
			show nInitialScoutsTag ++ " can't be fewer than one"
		)
	 ]

instance (
	Control.DeepSeq.NFData	fecundityDecayRatio,
	Control.DeepSeq.NFData	populationDiversityRatio
 ) => Control.DeepSeq.NFData (EvolutionStrategies fecundityDecayRatio populationDiversityRatio) where
	rnf MkEvolutionStrategies {
		getSynchronisedCourseMutationFecundity			= synchronisedCourseMutationFecundity,
		getSynchronisedCourseByDayMutationFecundity		= synchronisedCourseByDayMutationFecundity,
		getExcessRunlengthMutationFecundity			= excessRunlengthMutationFecundity,
		getHomogeneousStudentViewLessonMutationFecundity	= homogeneousStudentViewLessonMutationFecundity,
		getIncompleteCourseMutationFecundity			= incompleteCourseMutationFecundity,
		getRandomLessonMutationFecundity			= randomLessonMutationFecundity,
		getSingletonStudentClassMutationFecundity		= singletonStudentClassMutationFecundity,
		getSplitSessionMutationFecundity			= splitSessionMutationFecundity,
		getStudentBodyCombinationMutationFecundity		= studentBodyCombinationMutationFecundity,
		getStudentViewTimetableForDayMutationFecundity		= studentViewTimetableForDayMutationFecundity,
		getStudentViewTimetableForWeekMutationFecundity		= studentViewTimetableForWeekMutationFecundity,
		getSynchronousLessonMutationFecundity			= synchronousLessonMutationFecundity,
		getRandomLessonMutationNTrials				= randomLessonMutationNTrials,
		getRandomLessonMutationNTimeslots			= randomLessonMutationNTimeslots,
		getStudentViewTimetableForDayMutationMaybeNDays		= studentViewTimetableForDayMutationMaybeNDays,
		getStudentViewTimetableForWeekMutationNTrials		= studentViewTimetableForWeekMutationNTrials,
		getStudentViewTimetableForWeekMutationNTimeslots	= studentViewTimetableForWeekMutationNTimeslots,
		getFecundityDecayRatio					= fecundityDecayRatio,
		getMinimumPopulationDiversityRatio			= minimumPopulationDiversityRatio,
		getMaybeNInitialScouts					= maybeNInitialScouts
	} = Control.DeepSeq.rnf (
		[
			synchronisedCourseMutationFecundity,
			synchronisedCourseByDayMutationFecundity,
			excessRunlengthMutationFecundity,
			homogeneousStudentViewLessonMutationFecundity,
			incompleteCourseMutationFecundity,
			randomLessonMutationFecundity,
			singletonStudentClassMutationFecundity,
			splitSessionMutationFecundity,
			studentBodyCombinationMutationFecundity,
			studentViewTimetableForDayMutationFecundity,
			studentViewTimetableForWeekMutationFecundity,
			synchronousLessonMutationFecundity
		],
		randomLessonMutationNTrials,
		randomLessonMutationNTimeslots,
		studentViewTimetableForDayMutationMaybeNDays,
		studentViewTimetableForWeekMutationNTrials,
		studentViewTimetableForWeekMutationNTimeslots,
		fecundityDecayRatio,
		minimumPopulationDiversityRatio,
		maybeNInitialScouts
	 )

-- | The type of any function which mutates an evolution-strategies.
type Mutator fecundityDecayRatio populationDiversityRatio	= EvolutionStrategies fecundityDecayRatio populationDiversityRatio -> EvolutionStrategies fecundityDecayRatio populationDiversityRatio

-- Mutator.
zeroSynchronisedCourseMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroSynchronisedCourseMutationFecundity evolutionStrategies	= evolutionStrategies {
	getSynchronisedCourseMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroSynchronisedCourseByDayMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroSynchronisedCourseByDayMutationFecundity evolutionStrategies	= evolutionStrategies {
	getSynchronisedCourseByDayMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroExcessRunlengthMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroExcessRunlengthMutationFecundity evolutionStrategies	= evolutionStrategies {
	getExcessRunlengthMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroHomogeneousStudentViewLessonMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroHomogeneousStudentViewLessonMutationFecundity evolutionStrategies	= evolutionStrategies {
	getHomogeneousStudentViewLessonMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroIncompleteCourseMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroIncompleteCourseMutationFecundity evolutionStrategies	= evolutionStrategies {
	getIncompleteCourseMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroRandomLessonMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroRandomLessonMutationFecundity evolutionStrategies	= evolutionStrategies {
	getRandomLessonMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroSingletonStudentClassMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroSingletonStudentClassMutationFecundity evolutionStrategies	= evolutionStrategies {
	getSingletonStudentClassMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroSplitSessionMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroSplitSessionMutationFecundity evolutionStrategies	= evolutionStrategies {
	getSplitSessionMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroStudentBodyCombinationMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroStudentBodyCombinationMutationFecundity evolutionStrategies	= evolutionStrategies {
	getStudentBodyCombinationMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroStudentViewTimetableForDayMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroStudentViewTimetableForDayMutationFecundity evolutionStrategies	= evolutionStrategies {
	getStudentViewTimetableForDayMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroStudentViewTimetableForWeekMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroStudentViewTimetableForWeekMutationFecundity evolutionStrategies	= evolutionStrategies {
	getStudentViewTimetableForWeekMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- Mutator.
zeroSynchronousLessonMutationFecundity :: Mutator fecundityDecayRatio populationDiversityRatio
zeroSynchronousLessonMutationFecundity evolutionStrategies	= evolutionStrategies {
	getSynchronousLessonMutationFecundity	= ExecutionConfiguration.TimetableBreederFecundity.zero
}

-- True when all /fecundities/ are zero.
areAllZero :: EvolutionStrategies fecundityDecayRatio populationDiversityRatio -> Bool
areAllZero evolutionStrategies	= all (
	(== ExecutionConfiguration.TimetableBreederFecundity.zero) . ($ evolutionStrategies)
 ) [
	getSynchronisedCourseMutationFecundity,
	getSynchronisedCourseByDayMutationFecundity,
	getExcessRunlengthMutationFecundity,
	getHomogeneousStudentViewLessonMutationFecundity,
	getIncompleteCourseMutationFecundity,
	getRandomLessonMutationFecundity,
	getSingletonStudentClassMutationFecundity,
	getSplitSessionMutationFecundity,
	getStudentBodyCombinationMutationFecundity,
	getStudentViewTimetableForDayMutationFecundity,
	getStudentViewTimetableForWeekMutationFecundity,
	getSynchronousLessonMutationFecundity
 ]

