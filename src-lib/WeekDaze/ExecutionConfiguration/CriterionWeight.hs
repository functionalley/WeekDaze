{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses #-}
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

	* Defines the weight associated with some /criterion/.

	* Each weighting is quantified by some 'Fractional' value in the /closed unit-interval/; negative values aren't permitted.

	* If a concept is of no significance, then its weight can be set to /minBound/, whilst concepts of great significance can be set to /maxBound/.

 [@CAVEAT@]

	* While this data-type could implement the classes 'Functor', 'Num', 'Fractional' & 'Real', these interfaces allow one to construct invalid instances.
-}

module WeekDaze.ExecutionConfiguration.CriterionWeight(
-- * Type-classes
	CriterionWeights(..),
-- * Types
-- ** Data-types
	CriterionWeight(
--		MkCriterionWeight,
		deconstruct
	),
-- * Functions
-- ** Constructor
	mkCriterionWeight
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	ToolShed.SelfValidate

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Ratio
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector	as Database.Selector

instance (
	Data.Convertible.Convertible	Database.HDBC.SqlValue w,	-- Flexible context.
	Data.Typeable.Typeable		w,
	RealFrac			w
 ) => Data.Convertible.Convertible Database.HDBC.SqlValue (CriterionWeight w) {-multi-parameter type-class-} where
	safeConvert	= fmap (
		mkCriterionWeight . (
			\w -> if Data.Typeable.typeOf w == Data.Typeable.typeOf (undefined :: Data.Ratio.Rational)
				then realToFrac $ Database.Selector.round' w
				else w
		)
	 ) . Data.Convertible.safeConvert
#endif /* USE_HDBC */

{- |
	* Quantifies the relative significance, of criteria used to assess the desirability of a /resource/.

	* The larger the value, the more significant the criterion; relative to other criteria applicable to the same resource.
-}
newtype CriterionWeight w	= MkCriterionWeight {
	deconstruct	:: w
} deriving (Eq, Ord, Show)

instance Num w => Bounded (CriterionWeight w) where
	minBound	= MkCriterionWeight 0
	maxBound	= MkCriterionWeight 1

-- | True if the specified 'criterion-weight' falls within the /closed unit-interval/; <https://en.wikipedia.org/wiki/Unit_interval>.
instance Real w => ToolShed.SelfValidate.SelfValidator (CriterionWeight w) where
	getErrors w	= ToolShed.SelfValidate.extractErrors [
		(
			any ($ w) [(< minBound), (> maxBound)],
			"'" ++ show (realToFrac $ deconstruct w :: Double {-hide the data-constructor & the actual type-}) ++ "' should be within the closed unit-interval '[0,1]'"
		) -- Pair.
	 ]

instance Num w => Data.Default.Default (CriterionWeight w) where
	def	= minBound

instance (HXT.XmlPickler w, Real w) => HXT.XmlPickler (CriterionWeight w) where
	xpickle	= HXT.xpWrap (mkCriterionWeight, deconstruct) HXT.xpickle	-- CAVEAT: only apply 'HXT.xpDefault' after 'HXT.xpAttr' or 'HXT.xpElem'.

instance Control.DeepSeq.NFData w => Control.DeepSeq.NFData (CriterionWeight w) where
	rnf	= Control.DeepSeq.rnf . deconstruct

-- | Smart constructor.
mkCriterionWeight :: Real w => w -> CriterionWeight w
mkCriterionWeight w
	| ToolShed.SelfValidate.isValid criterionWeight	= criterionWeight
	| otherwise					= error $ "WeekDaze.ExecutionConfiguration.CriterionWeight.mkCriterionWeight:\t" ++ ToolShed.SelfValidate.getFirstError criterionWeight ++ "."
	where
		criterionWeight	= MkCriterionWeight w

-- | An interface to which a collection of criteria-weights may conform.
class CriterionWeights a where
	areAllZero	:: a -> Bool	-- ^ True if all the weights are zero.

instance (Bounded a, Eq a) => CriterionWeights [a] where
	areAllZero	= all (== minBound)

