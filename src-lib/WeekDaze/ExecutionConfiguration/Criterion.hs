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

	* Defines a single /criterion/, which quantifies the significant of some concept.
	These criteria represent /soft/-constraints of the problem;
	/hard/-constraints aren't relevent, because they are never allowed to be violated at all.

	* Many such criteria may exist, & their weighted-mean drives the selection
	amongst either competing /lesson/-definitions at a specific time-slot in the /timetable/,
	or between competing /timetable/s when attempting to optimise the solution.

	* Each /criterion/ is quantified by some 'Fractional' value,
	but since the weighted-mean should ideally be affected by the suitability of the solution rather than the dimensions of the problem,
	each is required to be normalised into the /closed unit-interval/.

 [@CAVEAT@]

	* While this data-type could implement the classes Num, Fractional & Real, these interfaces allow one to construct invalid instances.
-}

module WeekDaze.ExecutionConfiguration.Criterion(
-- * Types
-- ** Data-types
	Criterion(),
-- * Constants
	median,
-- * Functions
	invertNaturalNumbersIntoUnitInterval,
	invertWholeNumbersIntoUnitInterval,
	reflectUnitInterval,
	calculateWeightedMean,
-- ** Constructors
	mkCriterion,
	mkCriterionFrom
) where

import			Control.Arrow((&&&), (***))
import qualified	Control.Monad.Writer
import qualified	Factory.Math.Statistics
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.ExecutionConfiguration.CriterionWeight	as ExecutionConfiguration.CriterionWeight

{- |
	* Quantifies criteria used to assess the desirability of a /resource/.

	* The larger the value the better, relative to the same criterion applied other resources.
-}
newtype Criterion c	= MkCriterion {
	deconstruct	:: c
} deriving (Eq, Ord, Show)

instance Num c => Bounded (Criterion c) where
	minBound	= MkCriterion 0
	maxBound	= MkCriterion 1

-- | True if the specified 'criterion-weight' falls within the /closed unit-interval/; <https://en.wikipedia.org/wiki/Unit_interval>.
instance (Ord c, Real c) => ToolShed.SelfValidate.SelfValidator (Criterion c) where
	getErrors c	= ToolShed.SelfValidate.extractErrors [
		(
			any ($ c) [(< minBound), (> maxBound)],
			"'" ++ show (realToFrac $ deconstruct c :: Double {-hide the data-constructor & the actual type-}) ++ "' should be within the closed unit-interval '[0,1]'"
		) -- Pair.
	 ]

-- | Smart constructor.
mkCriterion :: Real c => String -> c -> Criterion c
mkCriterion name r
	| ToolShed.SelfValidate.isValid criterion	= criterion
	| otherwise					= error $ "WeekDaze.ExecutionConfiguration.Criterion.mkCriterion:\t" ++ show name ++ " " ++ ToolShed.SelfValidate.getFirstError criterion ++ "."
	where
		criterion	= MkCriterion r

-- | Build a /criterion/ from a 'Bool', arbitrarily assuming 'True' is better than 'False'.
mkCriterionFrom :: Num c => Bool -> Criterion c
mkCriterionFrom True	= maxBound
mkCriterionFrom _	= minBound

-- | Define the middle of the range of possible values.
median :: Fractional c => Criterion c
median	= MkCriterion $ recip 2

-- | Map a natural number into the unit-interval, by reflecting about 'one' & compressing the range; so that 'one' remains 'one', but 'infinity' becomes 'zero'.
invertNaturalNumbersIntoUnitInterval :: (Fractional f, Real f) => String -> f -> Criterion f
invertNaturalNumbersIntoUnitInterval name	= mkCriterion name . recip

-- | Map a whole number into the unit-interval, so that zero becomes one, & infinity becomes zero.
invertWholeNumbersIntoUnitInterval :: (
	Enum		c,
	Fractional	c,
	Real		c
 ) => String -> c -> Criterion c
invertWholeNumbersIntoUnitInterval name	= invertNaturalNumbersIntoUnitInterval name . succ {-avoid divide-by-zero-}

{- |
	* Reflect a number in the unit-interval, so that zero becomes one, & one becomes zero.

	* CAVEAT: if the number provided exceeds one, then an error will be generated.
-}
reflectUnitInterval :: Real c => String -> c -> Criterion c
reflectUnitInterval name	= mkCriterion name . (1 -)

{- |
	* Calculates the /weighted mean/ of the specified criterion-values using the corresponding criterion-weights.

	* Also writes individual unweighted criterion-values, to facilitate post-analysis;
	if the corresponding weight is zero, evaluation of the criterion is avoided,
	both for efficiency & to avoid the possibility of generating an error while evaluating a criterion which may have no validity in the context of the current problem.

	* CAVEAT: if all weights are zero, then the result can't be evaluated.
-}
calculateWeightedMean :: (
	Fractional	weightedMean,
	Real		criterionValue,
	Real		criterionWeight
 )
	=> [(Criterion criterionValue, ExecutionConfiguration.CriterionWeight.CriterionWeight criterionWeight)]
	-> Control.Monad.Writer.Writer [Maybe criterionValue] weightedMean
calculateWeightedMean assocList
	| all (
		(== minBound) . snd
	) assocList	= error "WeekDaze.ExecutionConfiguration.Criterion.calculateWeightedMean:\tzero weight => indeterminate result."
	| otherwise	= Control.Monad.Writer.writer . (
		Factory.Math.Statistics.getWeightedMean &&& map (
			\(criterionValue, criterionWeight)	-> if criterionWeight == 0
				then Nothing	-- Avoid unnecessary evaluation.
				else Just criterionValue
		)
	) $ map (deconstruct *** ExecutionConfiguration.CriterionWeight.deconstruct) assocList

