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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'ExecutionConfiguration.CriterionWeight.CriterionWeight'.
-}

module WeekDaze.Test.QuickCheck.ExecutionConfiguration.CriterionWeight(
-- * Types
-- ** Type-synonyms
	CriterionWeight
) where

import qualified	Test.QuickCheck
import qualified	WeekDaze.ExecutionConfiguration.CriterionWeight	as ExecutionConfiguration.CriterionWeight

instance (
	Fractional	criterionWeight,
	Real		criterionWeight
 ) => Test.QuickCheck.Arbitrary (ExecutionConfiguration.CriterionWeight.CriterionWeight criterionWeight) where
	arbitrary	= (ExecutionConfiguration.CriterionWeight.mkCriterionWeight . recip) `fmap` Test.QuickCheck.elements (map realToFrac [1 :: Int {-arbitrarily-} .. 10 {-arbitrarily-}])

-- | Defines a concrete type for testing.
-- type CriterionWeight	= ExecutionConfiguration.CriterionWeight.CriterionWeight Rational {-arbitrarily-}
type CriterionWeight	= ExecutionConfiguration.CriterionWeight.CriterionWeight Double {-arbitrarily-}
