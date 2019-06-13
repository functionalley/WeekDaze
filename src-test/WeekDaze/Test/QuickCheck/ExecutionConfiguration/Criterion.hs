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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'ExecutionConfiguration.Criterion.Criterion'.
-}

module WeekDaze.Test.QuickCheck.ExecutionConfiguration.Criterion(
-- * Types
-- ** Type-synonyms
--	Criterion,
-- * Constants
	results
) where

import qualified	Control.Monad.Writer
import qualified	Factory.Data.Interval
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))
import qualified	WeekDaze.ExecutionConfiguration.Criterion	as ExecutionConfiguration.Criterion
import qualified	WeekDaze.ExecutionConfiguration.CriterionWeight	as ExecutionConfiguration.CriterionWeight
import			WeekDaze.Test.QuickCheck.ExecutionConfiguration.CriterionWeight()

instance (
	Fractional	criterionValue,
	Real		criterionValue
 ) => Test.QuickCheck.Arbitrary (ExecutionConfiguration.Criterion.Criterion criterionValue) where
	arbitrary	= (ExecutionConfiguration.Criterion.mkCriterion "" . recip . fromIntegral) `fmap` Test.QuickCheck.elements [1 :: Int {-arbitrarily-} .. 10 {-arbitrarily-}]

-- | Defines a concrete type for testing.
-- type Criterion	= ExecutionConfiguration.Criterion.Criterion Rational {-arbitrarily-}
type Criterion	= ExecutionConfiguration.Criterion.Criterion Double {-arbitrarily-}

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [prop_calculateWeightedMean] where
	prop_calculateWeightedMean :: [(Criterion, ExecutionConfiguration.CriterionWeight.CriterionWeight Double)] -> Test.QuickCheck.Property
	prop_calculateWeightedMean assocs	= not (null assocs)	==> Test.QuickCheck.label "prop_calculateWeightedMean" $ (`Factory.Data.Interval.elem'` Factory.Data.Interval.closedUnitInterval) weightedMean where
		weightedMean :: Double
		weightedMean	= fst {-weighted mean-} . Control.Monad.Writer.runWriter $ ExecutionConfiguration.Criterion.calculateWeightedMean assocs

