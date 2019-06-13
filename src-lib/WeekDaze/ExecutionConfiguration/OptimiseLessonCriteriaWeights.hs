{-# LANGUAGE CPP, FlexibleContexts, UndecidableInstances #-}
{-
	Copyright (C) 2014-2015 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Contains the parameters which govern the attempt to automatically optimise the specified /lesson-criteria/ weights.
-}

module WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights (
-- * Types
-- ** Type-synonyms
--	NTrials,
-- ** Data-types
	OptimiseLessonCriteriaWeights(
--		MkOptimiseLessonCriteriaWeights,
		getNTrials,
		getChangeMagnitude,
		getReductionFactor,
		getUseMeanOverRasterScans
	),
-- * Constants
	tag,
--	nTrialsTag,
	changeMagnitudeTag,
--	reductionFactorTag,
--	useMeanOverRasterScansTag,
-- * Functions
-- ** Constructor
	mkOptimiseLessonCriteriaWeights,
-- ** Predicates
	isRequired
) where

import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	ToolShed.Data.Quadruple
import qualified	ToolShed.SelfValidate
import			WeekDaze.Enhanced.EnhancedBool()

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Maybe
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector	as Database.Selector

instance (
	Data.Convertible.Convertible	Database.HDBC.SqlValue criterionWeight,	-- Flexible context.
	Data.Typeable.Typeable		criterionWeight,
	RealFrac			criterionWeight
 ) => Database.Selector.Selector (OptimiseLessonCriteriaWeights criterionWeight) where
	fromDatabase connection	projectIdSql	= let
		optimiseLessonCriteriaWeightsTableName :: Database.Selector.TableName
		optimiseLessonCriteriaWeightsTableName	= Database.Selector.tablePrefix ++ tag
	 in do
		optimiseLessonCriteriaWeightsRows <- Database.Selector.select connection [
			nTrialsTag,
			changeMagnitudeTag,
			reductionFactorTag,
			useMeanOverRasterScansTag
		 ] [optimiseLessonCriteriaWeightsTableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		return {-to IO-monad-} $ (
			case optimiseLessonCriteriaWeightsRows of
				[]					-> id
				[optimiseLessonCriteriaWeightsRow]	-> case optimiseLessonCriteriaWeightsRow of
					[nTrialsSql, changeMagnitudeSql, reductionFactorSql, useMeanOverRasterScansSql] -> (
						\optimiseLessonCriteriaWeights -> Data.Maybe.maybe optimiseLessonCriteriaWeights (
							\value -> optimiseLessonCriteriaWeights { getNTrials = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights.fromDatabase:\tfailed to parse the value for " . shows nTrialsTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql nTrialsSql
					 ) . (
						\optimiseLessonCriteriaWeights -> Data.Maybe.maybe optimiseLessonCriteriaWeights (
							\value -> optimiseLessonCriteriaWeights { getChangeMagnitude = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights.fromDatabase:\tfailed to parse the value for " . shows changeMagnitudeTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql changeMagnitudeSql
					 ) . (
						\optimiseLessonCriteriaWeights -> Data.Maybe.maybe optimiseLessonCriteriaWeights (
							\value -> optimiseLessonCriteriaWeights { getReductionFactor = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights.fromDatabase:\tfailed to parse the value for " . shows reductionFactorTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql reductionFactorSql
					 ) . (
						\optimiseLessonCriteriaWeights -> Data.Maybe.maybe optimiseLessonCriteriaWeights (
							\value -> optimiseLessonCriteriaWeights { getUseMeanOverRasterScans = value }
						) . either (
							error . showString "WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights.fromDatabase:\tfailed to parse the value for " . shows useMeanOverRasterScansTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql useMeanOverRasterScansSql
					 ) -- Compose a mutator to operate on the default value.
					_ -> error $ "WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights.fromDatabase:\tunexpected number of columns=" ++ show (length optimiseLessonCriteriaWeightsRow) ++ " in row of table " ++ show optimiseLessonCriteriaWeightsTableName
				_					-> error $ "WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights.fromDatabase:\tunexpected number of rows=" ++ show (length optimiseLessonCriteriaWeightsRows) ++ " selected from table " ++ show optimiseLessonCriteriaWeightsTableName
		 ) Data.Default.def
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag				= "optimiseLessonCriteriaWeights"

-- | Used to qualify SQL & XML.
nTrialsTag :: String
nTrialsTag			= "nTrials"

-- | Used to qualify SQL & XML.
changeMagnitudeTag :: String
changeMagnitudeTag		= "changeMagnitude"

-- | Used to qualify SQL & XML.
reductionFactorTag :: String
reductionFactorTag		= "reductionFactor"

-- | Used to qualify SQL & XML.
useMeanOverRasterScansTag :: String
useMeanOverRasterScansTag	= "useMeanOverRasterScans"

-- | A number of trials.
type NTrials	= Int

-- | Encapsulates the data which drives the optimisation of lesson-criteria weights.
data OptimiseLessonCriteriaWeights criterionWeight	= MkOptimiseLessonCriteriaWeights {
	getNTrials			:: NTrials,		-- ^ The number of random trials used when attempting to optimise the lesson-criteria weights.
	getChangeMagnitude		:: criterionWeight,	-- ^ The magnitude of the random change applied to individual lesson-criteria, while attempting to find a better set.
	getReductionFactor		:: criterionWeight,	-- ^ The factor used to reduce the change-magnitude after each success.
	getUseMeanOverRasterScans	:: Bool			-- ^ Whether to accept a proposed set of /lesson-criteria weights/ on the basis of the mean (as opposed to the maximum) over the specified raster-scans, of the weighted mean over all heterogeneous /timetable-criteria/.
} deriving Eq

instance Show criterionWeight => Show (OptimiseLessonCriteriaWeights criterionWeight) where
	showsPrec _ optimiseLessonCriteriaWeights	= shows (
		getNTrials optimiseLessonCriteriaWeights,
		getChangeMagnitude optimiseLessonCriteriaWeights,
		getReductionFactor optimiseLessonCriteriaWeights,
		getUseMeanOverRasterScans optimiseLessonCriteriaWeights
	 ) -- Quadruple.

instance (
	Read	criterionWeight,
	Real	criterionWeight,
	Show	criterionWeight
 ) => Read (OptimiseLessonCriteriaWeights criterionWeight) where
	readsPrec _	= map (Control.Arrow.first $ ToolShed.Data.Quadruple.uncurry4 mkOptimiseLessonCriteriaWeights) . reads

instance Control.DeepSeq.NFData criterionWeight => Control.DeepSeq.NFData (OptimiseLessonCriteriaWeights criterionWeight) where
	rnf (MkOptimiseLessonCriteriaWeights x0 x1 x2 x3)	= Control.DeepSeq.rnf (x0, x1, x2, x3)

-- | Smart constructor.
mkOptimiseLessonCriteriaWeights :: (Real criterionWeight, Show criterionWeight)
	=> NTrials		-- ^ The number of random trials.
	-> criterionWeight	-- ^ Change-magnitude
	-> criterionWeight	-- ^ Reduction-factor.
	-> Bool			-- ^ Use Mean over Raster-scans.
	-> OptimiseLessonCriteriaWeights criterionWeight
mkOptimiseLessonCriteriaWeights nTrials changeMagnitude reductionFactor useMeanOverRasterScans
	| ToolShed.SelfValidate.isValid optimiseLessonCriteriaWeights	= optimiseLessonCriteriaWeights
	| otherwise							= error $ "WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights.mkOptimiseLessonCriteriaWeights:\t" ++ ToolShed.SelfValidate.getFirstError optimiseLessonCriteriaWeights ++ "."
	where
		optimiseLessonCriteriaWeights	= MkOptimiseLessonCriteriaWeights nTrials changeMagnitude reductionFactor useMeanOverRasterScans

instance Fractional criterionWeight => Data.Default.Default (OptimiseLessonCriteriaWeights criterionWeight) where
	def	= MkOptimiseLessonCriteriaWeights {
		getNTrials			= 0,
		getChangeMagnitude		= 1,
		getReductionFactor		= 9 / 10,
		getUseMeanOverRasterScans	= True
	}

instance (Real criterionWeight, Show criterionWeight) => ToolShed.SelfValidate.SelfValidator (OptimiseLessonCriteriaWeights criterionWeight) where
	getErrors optimiseLessonCriteriaWeights@MkOptimiseLessonCriteriaWeights {
		getNTrials		= nTrials,
		getChangeMagnitude	= changeMagnitude,
		getReductionFactor	= reductionFactor
	} = ToolShed.SelfValidate.extractErrors [
		(
			nTrials < 0,
			show nTrialsTag ++ " must be positive; " ++ show optimiseLessonCriteriaWeights
		), (
			changeMagnitude <= 0,
			show changeMagnitudeTag ++ " must be greater than zero; " ++ show optimiseLessonCriteriaWeights
		), (
			any ($ reductionFactor) [(< 0), (> 1)],
			show reductionFactorTag ++ " must be within the closed unit-interval; " ++ show optimiseLessonCriteriaWeights
		)
	 ]

instance (
	Fractional	criterionWeight,
	HXT.XmlPickler	criterionWeight,
	Real		criterionWeight,
	Show		criterionWeight
 ) => HXT.XmlPickler (OptimiseLessonCriteriaWeights criterionWeight) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d)	-> mkOptimiseLessonCriteriaWeights a b c d,	-- Construct from a quadruple.
		\MkOptimiseLessonCriteriaWeights {
			getNTrials			= nTrials,
			getChangeMagnitude		= changeMagnitude,
			getReductionFactor		= reductionFactor,
			getUseMeanOverRasterScans	= useMeanOverRasterScans
		} -> (
			nTrials,
			changeMagnitude,
			reductionFactor,
			useMeanOverRasterScans
		) -- Deconstruct to a quadruple.
	 ) $ HXT.xp4Tuple (
		getNTrials def `HXT.xpDefault` HXT.xpAttr nTrialsTag HXT.xpickle
	 ) (
		getChangeMagnitude def `HXT.xpDefault` HXT.xpAttr changeMagnitudeTag HXT.xpickle
	 ) (
		getReductionFactor def `HXT.xpDefault` HXT.xpAttr reductionFactorTag HXT.xpickle
	 ) (
		getUseMeanOverRasterScans def `HXT.xpDefault` HXT.xpAttr useMeanOverRasterScansTag HXT.xpickle
	 ) where
		def	= Data.Default.def

-- | Whether optimisation is required.
isRequired :: OptimiseLessonCriteriaWeights criterionWeight -> Bool
isRequired MkOptimiseLessonCriteriaWeights { getNTrials = nTrials }	= nTrials > 0

