{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'ExecutionConfiguration.OptimiseLessonCriteriaWeights.OptimiseLessonCriteriaWeights'.
-}

module WeekDaze.Test.QuickCheck.ExecutionConfiguration.OptimiseLessonCriteriaWeights(
-- * Types
-- ** Type-synonyms
--	OptimiseLessonCriteriaWeights,
-- * Constants
	results
 ) where

import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO
import qualified	WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights	as ExecutionConfiguration.OptimiseLessonCriteriaWeights

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance (
	Fractional	criterionWeight,
	Real		criterionWeight,
	Show		criterionWeight
 ) => Test.QuickCheck.Arbitrary (ExecutionConfiguration.OptimiseLessonCriteriaWeights.OptimiseLessonCriteriaWeights criterionWeight) where
	arbitrary	= ExecutionConfiguration.OptimiseLessonCriteriaWeights.mkOptimiseLessonCriteriaWeights <$> Test.QuickCheck.elements [0 .. 9] <*> Test.QuickCheck.elements (
		take 10 $ iterate (/ 2) 2
	 ) <*> Test.QuickCheck.elements (
		take 10 $ iterate (/ 2) 1
	 ) <*> Test.QuickCheck.arbitrary {-useMeanOverRasterScans-}

-- | Defines a concrete type for testing.
type OptimiseLessonCriteriaWeights	= ExecutionConfiguration.OptimiseLessonCriteriaWeights.OptimiseLessonCriteriaWeights Rational {-arbitrarily-}

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_readPrependedWhiteSpace,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_read,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_readTrailingGarbage
 ] where
	prop_readPrependedWhiteSpace :: OptimiseLessonCriteriaWeights -> Test.QuickCheck.Property
	prop_readPrependedWhiteSpace	= Test.QuickCheck.label "prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace

	prop_read :: String -> Test.QuickCheck.Property
	prop_read garbage	= Test.QuickCheck.label "prop_read" $ case (reads garbage :: [(OptimiseLessonCriteriaWeights, String)]) of
		[_]	-> True
		_	-> True	-- Unless the read-implementation throws an exception.

	prop_readTrailingGarbage :: OptimiseLessonCriteriaWeights -> String -> Test.QuickCheck.Property
	prop_readTrailingGarbage optimiseLessonCriteriaWeights	= Test.QuickCheck.label "prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) optimiseLessonCriteriaWeights
