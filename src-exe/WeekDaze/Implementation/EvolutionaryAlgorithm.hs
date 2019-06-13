{-# LANGUAGE CPP, ScopedTypeVariables #-}
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

	* Deterministically constructs the initial /timetable/.

	* Uses an evolutionary algorithm (<https://en.wikipedia.org/wiki/Evolutionary_algorithm>),
	based on an ordered sequence of different evolution-strategies, to progressively improve it.
-}

module WeekDaze.Implementation.EvolutionaryAlgorithm(
-- * Types
-- ** Type-synonyms
--	NGenerations,
-- * Functions
--	evolveQuantifiedStudentViewTimetable,
	evolveStudentViewTimetable,
--	selectDeterministicStudentViewTimetable,
	optimiseLessonCriteriaWeights
) where

import			Control.Arrow((&&&))
import			Data.Map((!))
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Monad.Writer	-- The lazy instance.
import qualified	Control.Parallel.Strategies
import qualified	Data.Array.IArray
import qualified	Data.Default
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	Data.Set
import qualified	Factory.Math.Factorial
import qualified	Factory.Math.Implementations.Factorial
import qualified	Factory.Math.Statistics
import qualified	System.Random
import qualified	ToolShed.Data.List
import qualified	ToolShed.Data.Pair
import qualified	ToolShed.Data.Quadruple
import qualified	ToolShed.Data.Triple
import qualified	ToolShed.System.Random
import qualified	WeekDaze.Aggregate.StudentBody						as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentBodyRegister					as Aggregate.StudentBodyRegister
import qualified	WeekDaze.Dynamic.StudentViewTimetableUtilities				as Dynamic.StudentViewTimetableUtilities
import qualified	WeekDaze.ExecutionConfiguration.CriterionWeight				as ExecutionConfiguration.CriterionWeight
import qualified	WeekDaze.ExecutionConfiguration.EvolutionStrategies			as ExecutionConfiguration.EvolutionStrategies
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions			as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.ExecutionConfiguration.LessonCriteriaWeights			as ExecutionConfiguration.LessonCriteriaWeights
import qualified	WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights		as ExecutionConfiguration.OptimiseLessonCriteriaWeights
import qualified	WeekDaze.ExecutionConfiguration.TimetableBreederFecundity		as ExecutionConfiguration.TimetableBreederFecundity
import qualified	WeekDaze.Implementation.DeterministicConstructor			as Implementation.DeterministicConstructor
import qualified	WeekDaze.Implementation.QuantifiedStudentViewTimetable			as Implementation.QuantifiedStudentViewTimetable
import qualified	WeekDaze.Implementation.RandomConstructor				as Implementation.RandomConstructor
import qualified	WeekDaze.Implementation.StudentViewTimetableRandomBreeders		as Implementation.StudentViewTimetableRandomBreeders
import qualified	WeekDaze.Model.TimetableAxisTriple					as Model.TimetableAxisTriple
import qualified	WeekDaze.Model.TimetableCoordinates					as Model.TimetableCoordinates
import qualified	WeekDaze.Model.Traverse							as Model.Traverse
import qualified	WeekDaze.OutputFormat.DeterministicStudentViewTimetableSelection	as OutputFormat.DeterministicStudentViewTimetableSelection
import qualified	WeekDaze.OutputFormat.EvolutionStrategyStatistics			as OutputFormat.EvolutionStrategyStatistics
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis				as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters				as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.StudentView.Timetable						as StudentView.Timetable
import qualified	WeekDaze.StudentView.TimetableCoordinates				as StudentView.TimetableCoordinates
import			WeekDaze.Implementation.StudentViewTimetableRandomBreeders(StudentViewTimetableRandomBreederStrategy((:*>)))

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<*>))
#endif

-- | A number of generations in the evolution of the /timetable/.
type NGenerations	= Int

{- |
	* Uses the specified 'WeekDaze.Implementation.StudentViewTimetableRandomBreeders.StudentViewTimetableQuantifiedRandomBreeder' to breed a population of candidates from a parent /StudentViewTimetable/.
	The size of the population is initially defined by the specified /fecundity/,
	but is then dynamically controlled by feedback, in an attempt to maintain the configured minimum /diversity-ratio/ in the candidate populations.

	* The best /timetable/s (though potentially less fit than their parent), are used to seed subsequent generations to produce a family-tree.

	* By selecting fewer candidates from each successive generation, the resulting family-tree thins, & dies (when zero are selected).
	The best child from this entire tree is then compared with the original parent to determine whether any beneficial evolution has occurred;
	if fitter, then the whole process is restarted from that child, otherwise the evolution is terminated.
	The number of branches sprouting from any node in this tree, may be alternatively be variable if /maybeNInitialScouts/ is set to 'Nothing'.

	* The number of generations of evolution, the final fecundity of the breeding-program, & the evolved 'Implementation.QuantifiedStudentViewTimetable.QuantifiedStudentViewTimetable'; are returned.
	The resulting 'nGenerations' is a typically a multiple of 'nInitialScouts' (where defined),
	since at each stage scouts are dispatched for this number of generations before returning & comparing for fitness with the original parent,
	& if a leaf-node is fittest then 'nInitialScouts' is added to 'nGenerations'.
-}
evolveQuantifiedStudentViewTimetable :: forall campus criterionValue criterionWeight fecundityDecayRatio level locationId populationDiversityRatio randomGen stream synchronisationId teacherId teachingRatio timeslotId weightedMean. (
	Control.DeepSeq.NFData	level,
	Control.DeepSeq.NFData	locationId,
	Control.DeepSeq.NFData	teacherId,
	Control.DeepSeq.NFData	timeslotId,
	Control.DeepSeq.NFData	weightedMean,
	Data.Array.IArray.Ix	timeslotId,
	Enum			criterionValue,
	Enum			timeslotId,
	Eq			campus,
	Fractional		criterionValue,
	Fractional		weightedMean,
	Num			populationDiversityRatio,
	Ord			level,
	Ord			locationId,
	Ord			populationDiversityRatio,
	Ord			synchronisationId,
	Ord			teacherId,
	Ord			weightedMean,
	Real			criterionValue,
	Real			criterionWeight,
	RealFrac		fecundityDecayRatio,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId,
	System.Random.RandomGen	randomGen
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Implementation.StudentViewTimetableRandomBreeders.StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level
	-> ExecutionConfiguration.TimetableBreederFecundity.Fecundity												-- ^ Define the size of the required population.
	-> randomGen																		-- ^ Permit mutation of each candidate wrt the parent from which it was bred.
	-> Implementation.QuantifiedStudentViewTimetable.QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue	-- ^ The parent from which to breed a candidate population.
	-> (
		NGenerations,
		ExecutionConfiguration.TimetableBreederFecundity.Fecundity,
		Implementation.QuantifiedStudentViewTimetable.QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue
	) -- Triple.
evolveQuantifiedStudentViewTimetable problemParameters executionOptions problemAnalysis studentViewTimetableQuantifiedRandomBreeder	= evolve 0 {-nGenerations-} where
	evolutionStrategies	= ExecutionConfiguration.ExecutionOptions.getEvolutionStrategies executionOptions

	dispatchScouts
		:: Enum nGenerations
		=> Maybe Int
		-> (
			nGenerations,
			ExecutionConfiguration.TimetableBreederFecundity.Fecundity,
			randomGen,
			Implementation.QuantifiedStudentViewTimetable.QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue
		)
		-> (
			nGenerations,
			ExecutionConfiguration.TimetableBreederFecundity.Fecundity,
			randomGen,
			Implementation.QuantifiedStudentViewTimetable.QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue
		)
	dispatchScouts (Just 0 {-nScouts-}) parentQuadruple									= parentQuadruple	-- Not strictly necessary since the general case can handle zero.
	dispatchScouts maybeNScouts parentQuadruple@(nGenerations, fecundity, randomGen, quantifiedStudentViewTimetable)	= Data.List.maximumBy {-known as "Elitist Selection"-} (
		Data.Ord.comparing $ Implementation.QuantifiedStudentViewTimetable.getWeightedMeanOfTimetableCriteria . ToolShed.Data.Quadruple.getFourth
	 ) . (
		parentQuadruple :	-- Compare the fitness of the scouts with their parent; which as a side-effect ensures 'Data.List.maximumBy' doesn't receive a null list.
{-
	 ) . Control.Parallel.Strategies.withStrategy (
		Control.Parallel.Strategies.parList $ Control.Parallel.Strategies.parTuple4 Control.Parallel.Strategies.r0 Control.Parallel.Strategies.r0 Control.Parallel.Strategies.r0 Control.Parallel.Strategies.rdeepseq	-- Generates too many sparks.
-}
	 ) . map (
		dispatchScouts (
			fmap pred maybeNScouts	-- Progressively thin the family-tree, with each successive generation, until ultimately zero branches sprout.
		) {-recurse-} . (,,,) (
			succ nGenerations	-- Count the generations.
		) (
			(
				if fromIntegral (length distinctChildren) < ExecutionConfiguration.EvolutionStrategies.getMinimumPopulationDiversityRatio evolutionStrategies * fromIntegral fecundity	-- Check whether a diverse population of the requested size wasn't achieved, either because insufficient candidates were bred or because they were insufficiently diverse.
					then ceiling {-guarantee a minimum of one-} . (
						* ExecutionConfiguration.EvolutionStrategies.getFecundityDecayRatio evolutionStrategies	-- Reduce the fecundity when breeding the next generation.
					) . fromIntegral
					else id
			) fecundity
		) unusedRandomGen	-- Recursively dispatch scouts, some of whom may be less fit than their parent.
	 ) . (
		\candidates -> Data.Maybe.maybe (
			concat {-ungroup-} . take 1 {-perhaps zero-} . takeWhile (
				(
					> Implementation.QuantifiedStudentViewTimetable.getWeightedMeanOfTimetableCriteria quantifiedStudentViewTimetable	-- Since the number of scouts isn't decremented (ultimately to zero), require that the weighted mean over timetable-criteria increases, in order to guarantee termination.
				) . Implementation.QuantifiedStudentViewTimetable.getWeightedMeanOfTimetableCriteria . head {-of group of equals-}
			) $ Data.List.groupBy (
				ToolShed.Data.List.equalityBy Implementation.QuantifiedStudentViewTimetable.getWeightedMeanOfTimetableCriteria
			) candidates
		) (
			`take` candidates
		) maybeNScouts	-- Select either the specified number of scouts, or those which are fitter than their parent.
	 ) . Data.List.sortBy (
		flip $ Data.Ord.comparing Implementation.QuantifiedStudentViewTimetable.getWeightedMeanOfTimetableCriteria	-- Fittest candidate first.
	 ) . Control.Parallel.Strategies.withStrategy (
		Control.Parallel.Strategies.parList Control.Parallel.Strategies.rdeepseq	-- Evaluate each quantifiedStudentViewTimetable in parallel.
	 ) $ map (
		Implementation.QuantifiedStudentViewTimetable.mkQuantifiedStudentViewTimetable problemParameters executionOptions problemAnalysis	-- Expensive, so postponed until after de-duplication.
	 ) distinctChildren where
		(distinctChildren, unusedRandomGen)	= Control.Arrow.first (
			Data.Set.toList . Data.Set.delete parentStudentViewTimetable {-remove any clone-} . Data.Set.fromList {-de-duplicate-} . Control.Parallel.Strategies.withStrategy (
				Control.Parallel.Strategies.parList Control.Parallel.Strategies.rdeepseq	-- Evaluate each studentViewTimetable in parallel.
			) . (
				$ parentStudentViewTimetable	-- Breed from the parent.
			) . studentViewTimetableQuantifiedRandomBreeder fecundity	-- Implement the breeding-program; which may produce fewer candidates than requested.
		 ) $ System.Random.split randomGen {-if identical timetables are selected from different generations, we still want to breed a distinct population from each-} where
			parentStudentViewTimetable	= Implementation.QuantifiedStudentViewTimetable.getStudentViewTimetable quantifiedStudentViewTimetable

	evolve
		:: Enum nGenerations
		=> nGenerations
		-> ExecutionConfiguration.TimetableBreederFecundity.Fecundity
		-> randomGen
		-> Implementation.QuantifiedStudentViewTimetable.QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue
		-> (
			nGenerations,
			ExecutionConfiguration.TimetableBreederFecundity.Fecundity,
			Implementation.QuantifiedStudentViewTimetable.QuantifiedStudentViewTimetable weightedMean timeslotId locationId teacherId level criterionValue
		) -- Triple.
	evolve nGenerations fecundity randomGen parentQuantifiedStudentViewTimetable
		| uncurry (>) $ ToolShed.Data.Pair.mirror Implementation.QuantifiedStudentViewTimetable.getWeightedMeanOfTimetableCriteria (
			ToolShed.Data.Quadruple.getFourth quadruple,
			parentQuantifiedStudentViewTimetable
		)		= ToolShed.Data.Quadruple.uncurry4 evolve quadruple			-- Tail-recurse.
		| otherwise	= (nGenerations, fecundity, parentQuantifiedStudentViewTimetable)	-- Triple.
		where
			quadruple	= dispatchScouts (
				ExecutionConfiguration.EvolutionStrategies.getMaybeNInitialScouts evolutionStrategies
			 ) (
				nGenerations,
				fecundity,
				randomGen,
				parentQuantifiedStudentViewTimetable
			 ) -- Quadruple.

{- |
	* Sequentially mutates the /timetable/, by firstly undefining all /lesson/s in a randomly selected /subject/,
	then by undefining progressively smaller sets of randomly selected /lesson/s.

	* The best /timetable/ is returned, according to the weighted mean of the /timetable-criteria/, by which it was improved at each stage.

	* An 'OutputFormat.EvolutionStrategyStatistics.EvolutionStrategyStatistics', for each strategy, is written to facilitate subsequent analysis.
-}
evolveStudentViewTimetable :: forall campus criterionValue criterionWeight fecundityDecayRatio level locationId populationDiversityRatio randomGen stream synchronisationId teacherId teachingRatio timeslotId weightedMean. (
	Control.DeepSeq.NFData	level,
	Control.DeepSeq.NFData	locationId,
	Control.DeepSeq.NFData	teacherId,
	Control.DeepSeq.NFData	timeslotId,
	Control.DeepSeq.NFData	weightedMean,
	Data.Array.IArray.Ix	timeslotId,
	Enum			criterionValue,
	Enum			timeslotId,
	Enum			weightedMean,
	Eq			campus,
	Fractional		criterionValue,
	Fractional		weightedMean,
	Num			populationDiversityRatio,
	Ord			level,
	Ord			locationId,
	Ord			populationDiversityRatio,
	Ord			stream,
	Ord			synchronisationId,
	Ord			teacherId,
	Ord			weightedMean,
	Real			criterionValue,
	Real			criterionWeight,
	RealFrac		fecundityDecayRatio,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId,
	System.Random.RandomGen	randomGen
 )
	=> randomGen
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> StudentView.Timetable.Timetable timeslotId locationId teacherId level	-- ^ The initial value (returned by 'selectDeterministicStudentViewTimetable'), from which to evolve.
	-> Control.Monad.Writer.Writer [OutputFormat.EvolutionStrategyStatistics.EvolutionStrategyStatistics weightedMean criterionValue] (
		StudentView.Timetable.Timetable timeslotId locationId teacherId level
	) -- ^ The evolved /timetable/.
evolveStudentViewTimetable randomGen problemParameters executionOptions problemAnalysis studentViewTimetable
	| ExecutionConfiguration.CriterionWeight.areAllZero (
		ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights executionOptions
	)		= return {-to Writer-monad-} studentViewTimetable	-- No improvement can ever be measured.
	| otherwise	= fmap Implementation.QuantifiedStudentViewTimetable.getStudentViewTimetable . foldr (
		\(depletionStrategyName, reconstructionStrategyName, fecundity, studentViewTimetableQuantifiedRandomBreeder) quantifiedStudentViewTimetableWriter	-> do
			quantifiedStudentViewTimetable	<- quantifiedStudentViewTimetableWriter	-- Receive the fittest from the previous evolution-strategy.

			let
				(nGenerations, finalFecundity, quantifiedStudentViewTimetable')	= evolveQuantifiedStudentViewTimetable problemParameters executionOptions problemAnalysis studentViewTimetableQuantifiedRandomBreeder fecundity randomGen quantifiedStudentViewTimetable	-- Implement the current evolution-strategy, & gather timetable-criteria from the selected candidate, in each generation. Since each strategy is different, the random-generator can be re-used.

			Control.Monad.Writer.tell [
				OutputFormat.EvolutionStrategyStatistics.mkEvolutionStrategyStatistics depletionStrategyName reconstructionStrategyName fecundity finalFecundity (
					Implementation.QuantifiedStudentViewTimetable.calculateImprovementFactor quantifiedStudentViewTimetable quantifiedStudentViewTimetable'
				 ) (
					Implementation.QuantifiedStudentViewTimetable.getWeightedMeanOfTimetableCriteria quantifiedStudentViewTimetable'
				 ) nGenerations $ if nGenerations == 0
					then Nothing
					else Just $ Implementation.QuantifiedStudentViewTimetable.getTimetableCriteriaValues quantifiedStudentViewTimetable'
			 ] -- Singleton-list.

			return {-to Writer-monad-} quantifiedStudentViewTimetable'	-- Forward the fittest candidate to the next evolution-strategy.
	) (
		return {-to Writer-monad-} $ Implementation.QuantifiedStudentViewTimetable.mkQuantifiedStudentViewTimetable problemParameters executionOptions problemAnalysis studentViewTimetable	-- Initial value.
	) . filter (
		(> 0) . ToolShed.Data.Quadruple.getThird {-fecundity-}	-- Reduce unnecessary logging.
	) $ [
		specifyRandomBreedingStrategy,	-- Relatively ineffective, but can explore a part of the solution-space unreachable by 'Implementation.DeterministicConstructor.mutateStudentViewTimetable'.
		specifyDeterministicBreedingStrategy
	] {-functions which require a triple-} <*> map (
		\studentViewTimetableRandomBreederStrategy -> (
			show studentViewTimetableRandomBreederStrategy,	-- Generate a name for this strategy.
			Implementation.StudentViewTimetableRandomBreeders.getTimetableBreederFecundity studentViewTimetableRandomBreederStrategy evolutionStrategies,
			Implementation.StudentViewTimetableRandomBreeders.breed studentViewTimetableRandomBreederStrategy problemAnalysis problemParameters	-- Encapsulates both the depleter & the reconstructor.
		) -- Triple to which the above functions can be applied.
	) [
		Implementation.StudentViewTimetableRandomBreeders.SplitSessionMutation :*> randomLessonMutation,		-- Targets a specific problem, but needs additional space to work. CAVEAT: split sessions can only be generated during random reconstruction.
		Implementation.StudentViewTimetableRandomBreeders.ExcessRunlengthMutation :*> randomLessonMutation,		-- Targets a specific problem, but needs additional space to work.
		Implementation.StudentViewTimetableRandomBreeders.IncompleteCourseMutation :*> randomLessonMutation,		-- Targets a specific problem, but needs additional space to work.
		Implementation.StudentViewTimetableRandomBreeders.StudentBodyCombinationMutation :*> randomLessonMutation,	-- Targets a specific problem, but needs additional space to work.
		Implementation.StudentViewTimetableRandomBreeders.SingletonStudentClassMutation :*> randomLessonMutation,	-- Targets a specific problem, but needs additional space to work.
		uncurry Implementation.StudentViewTimetableRandomBreeders.StudentViewTimetableForWeekMutation $ (ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForWeekMutationNTrials &&& ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForWeekMutationNTimeslots) evolutionStrategies,	-- Highly effective, but can't easily change a student's routine; so perform after that's been refined.
		Implementation.StudentViewTimetableRandomBreeders.HomogeneousStudentViewLessonMutation,				-- Breaks a student's routine (relationship for a subject, with a location & teacher), allowing exploration of alternatives.
		Implementation.StudentViewTimetableRandomBreeders.StudentViewTimetableForDayMutation $ ExecutionConfiguration.EvolutionStrategies.getStudentViewTimetableForDayMutationMaybeNDays evolutionStrategies,	-- Probably the most effective strategy.
		Implementation.StudentViewTimetableRandomBreeders.SynchronisedCourseByDayMutation :*> Implementation.StudentViewTimetableRandomBreeders.SynchronousLessonMutation,
		Implementation.StudentViewTimetableRandomBreeders.SynchronisedCourseMutation					-- Relocating synchronised courses is highly disruptive, so locate these before proceeding up the list.
	] -- Generate combinations of depleter & re-constructor. CAVEAT: 'foldr' applies these in reverse order.
	where
		evolutionStrategies	= ExecutionConfiguration.ExecutionOptions.getEvolutionStrategies executionOptions
		randomLessonMutation	= uncurry Implementation.StudentViewTimetableRandomBreeders.RandomLessonMutation $ (ExecutionConfiguration.EvolutionStrategies.getRandomLessonMutationNTrials &&& ExecutionConfiguration.EvolutionStrategies.getRandomLessonMutationNTimeslots) evolutionStrategies	-- For application with another depleter to liberate space for change. The intention is to apply it after a specific-issue depleter, & as such it won't be invoked unless that issue exists; if applied before the other specific-issue depleter, then it might create that issue.

		selectUnallocatedAvailableUnreservedCoordinatePermutations
			:: ExecutionConfiguration.TimetableBreederFecundity.Fecundity
			-> randomGen
			-> StudentView.Timetable.Timetable timeslotId locationId teacherId level
			-> [StudentView.TimetableCoordinates.Vector timeslotId]
		selectUnallocatedAvailableUnreservedCoordinatePermutations fecundity randomGen'	= (
			\unallocatedAvailableUnreservedCoordinates -> if Factory.Math.Factorial.factorial (
				Data.Default.def	:: Factory.Math.Implementations.Factorial.Algorithm
			) (fromIntegral $ length unallocatedAvailableUnreservedCoordinates) <= toInteger {-avoid overflow in factorial-} fecundity
				then Data.List.permutations unallocatedAvailableUnreservedCoordinates	-- Generate all coordinate-permutations (the order is consequently irrelevant), which will either be equal or fewer than required.
				else take fecundity . Data.List.nub {-lazy-} . map (
					`ToolShed.System.Random.shuffle` unallocatedAvailableUnreservedCoordinates
				) $ ToolShed.System.Random.randomGens randomGen'	-- The number of coordinate-permutations is excessive, so randomly select the required number of distinct coordinate-vectors.
		 ) . Dynamic.StudentViewTimetableUtilities.locateUnallocatedAvailableUnreservedCoordinates problemParameters problemAnalysis

		specifyDeterministicBreedingStrategy
			:: (
				String,
				ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,
				Implementation.StudentViewTimetableRandomBreeders.StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level -> studentViewTimetableRandomBreeder
			) -- Triple.
			-> (
				String,
				String,
				ExecutionConfiguration.TimetableBreederFecundity.Fecundity,
				studentViewTimetableRandomBreeder
			) -- Quadruple.
		specifyDeterministicBreedingStrategy (tag, timetableBreederFecundity, studentViewTimetableBreederMutator)	= (
			tag,
			"Deterministic",
			ExecutionConfiguration.TimetableBreederFecundity.getDeterministicConstructorFecundity timetableBreederFecundity,
			studentViewTimetableBreederMutator studentViewTimetableBreeder	-- Partially apply, by defining the call-back.
		 ) where
			studentViewTimetableBreeder :: Implementation.StudentViewTimetableRandomBreeders.StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level
			studentViewTimetableBreeder fecundity randomGen'	= map (
				fst {-studentViewTimetable-} . Control.Arrow.second (
					\lessonCriteriaStatistics -> lessonCriteriaStatistics :: [Maybe ((Rational {-mean-}, Double {-standard deviation-}), (Rational {-min-}, Rational {-max-}))]	-- The type of the discarded statistics should still be defined.
				) . Control.Monad.Writer.runWriter
			 ) . uncurry map . (
				Implementation.DeterministicConstructor.mutateStudentViewTimetable problemParameters executionOptions problemAnalysis &&& selectUnallocatedAvailableUnreservedCoordinatePermutations fecundity randomGen'
			 )

		specifyRandomBreedingStrategy
			:: (
				String,
				ExecutionConfiguration.TimetableBreederFecundity.TimetableBreederFecundity,
				Implementation.StudentViewTimetableRandomBreeders.StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level -> studentViewTimetableRandomBreeder
			) -- Triple.
			-> (
				String,
				String,
				ExecutionConfiguration.TimetableBreederFecundity.Fecundity,
				studentViewTimetableRandomBreeder
			) -- Quadruple.
		specifyRandomBreedingStrategy (tag, timetableBreederFecundity, studentViewTimetableBreederMutator)	= (
			tag,
			"Random",
			ExecutionConfiguration.TimetableBreederFecundity.getRandomConstructorFecundity timetableBreederFecundity,
			studentViewTimetableBreederMutator studentViewTimetableBreeder	-- Partially apply, by defining the call-back.
		 ) where
			studentViewTimetableBreeder :: Implementation.StudentViewTimetableRandomBreeders.StudentViewTimetableQuantifiedRandomBreeder randomGen timeslotId locationId teacherId level
			studentViewTimetableBreeder fecundity randomGen'	= uncurry map . (
				Implementation.RandomConstructor.mutateStudentViewTimetable randomGen' problemParameters executionOptions problemAnalysis &&& selectUnallocatedAvailableUnreservedCoordinatePermutations fecundity randomGen'
			 )

{- |
	* Generates the initial /student/-view of the /timetable/,
	by a deterministic process of raster-scanning the /time-slot/s in the three-dimensional structure,
	& at each /time-slot/ selecting the best /lesson/ according to the weighted mean over all heterogeneous /lesson-criteria/.

	* The raster-scan can be performed via many different routes through the three-dimensional structure of the /timetable/.
	This can be precisely specified, but since it's difficult to choose the optimal route in advance,
	typically one opts to traverse all permutations in parallel, selecting the best according to the weighted mean over all heterogeneous /timetable-criteria/.

	* Whilst one could replace the regular raster with a random walk, it doesn't seem to be competitive.

	* Ancillary data is returned to facilitate subsequent analysis.
-}
selectDeterministicStudentViewTimetable :: forall campus criterionValue criterionWeight fecundityDecayRatio level locationId mean populationDiversityRatio standardDeviation stream synchronisationId teacherId teachingRatio timeslotId weightedMean. (
	Control.DeepSeq.NFData	weightedMean,
	Data.Array.IArray.Ix	timeslotId,
	Enum			criterionValue,
	Enum			timeslotId,
	Eq			campus,
	Floating		standardDeviation,
	Fractional		criterionValue,
	Fractional		mean,
	Fractional		weightedMean,
	Ord			level,
	Ord			locationId,
	Ord			stream,
	Ord			synchronisationId,
	Ord			teacherId,
	Ord			weightedMean,
	Real			criterionValue,
	Real			criterionWeight,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Maybe (StudentView.Timetable.Timetable timeslotId locationId teacherId level)	-- ^ Read from 'ExecutionConfiguration.ExecutionOptions.getMaybeHint' for the case of /Right inputFilePath/.
	-> OutputFormat.DeterministicStudentViewTimetableSelection.DeterministicStudentViewTimetableSelection criterionValue criterionWeight level locationId mean standardDeviation teacherId timeslotId weightedMean
selectDeterministicStudentViewTimetable problemParameters executionOptions problemAnalysis maybeInitialStudentViewTimetable	= (
	\(
		(
			(studentViewTimetable, initialLessonCriteriaStatistics),	-- Pair.
			initialTimetableCriterionValues
		), -- Pair.
		weightedMeanOfTimetableCriteriaByTraversalOrder
	) {-pair-} -> OutputFormat.DeterministicStudentViewTimetableSelection.mkDeterministicStudentViewTimetableSelection studentViewTimetable initialLessonCriteriaStatistics initialTimetableCriterionValues weightedMeanOfTimetableCriteriaByTraversalOrder
 ) . Data.Maybe.maybe (
	selectBestRasterScan Model.TimetableAxisTriple.permutations
 ) (
	(
		\traversalOrder -> if Model.TimetableAxisTriple.hasWildSense traversalOrder
			then selectBestRasterScan $ Model.TimetableAxisTriple.generatePermutationsOf traversalOrder
			else Control.Arrow.second Left . getTimetableFor $ generateStudentViewTimetableCoordinateVector traversalOrder
	) `either` const {-drop file-path-} (
		Data.Maybe.maybe (
			error "WeekDaze.Implementation.EvolutionaryAlgorithm.selectDeterministicStudentViewTimetable:\tinitial studentViewTimetable is unspecified."
		) (
			\studentViewTimetable -> (
				(,) (
					studentViewTimetable,
					[{-initialLessonCriteriaStatistics-}]
				) . Implementation.QuantifiedStudentViewTimetable.getTimetableCriteriaValues &&& Left . Implementation.QuantifiedStudentViewTimetable.getWeightedMeanOfTimetableCriteria
			) $ Implementation.QuantifiedStudentViewTimetable.mkQuantifiedStudentViewTimetable problemParameters executionOptions problemAnalysis studentViewTimetable
		) maybeInitialStudentViewTimetable
	) -- A file-path to the initial studentViewTimetable was specified, so the corresponding parsed studentViewTimetable should have been passed as a parameter.
 ) $ ExecutionConfiguration.ExecutionOptions.getMaybeHint executionOptions where
	selectBestRasterScan
		:: [Model.TimetableAxisTriple.Axes]
		-> (
			(
				(
					StudentView.Timetable.Timetable timeslotId locationId teacherId level,
					[
						Maybe (
							Implementation.DeterministicConstructor.LessonCriteriaStatistics criterionValue mean standardDeviation
						)
					]
				),
				[
					Maybe criterionValue
				]
			),
			Either a [
				(Model.TimetableAxisTriple.Axes, weightedMean)
			]
		)
	selectBestRasterScan	= (
		snd {-drop traversalOrder-} . fst {-drop weightedMeanOfTimetableCriteria-} . Data.List.maximumBy (
			Data.Ord.comparing snd {-weightedMeanOfTimetableCriteria-}
		) &&& Right . map (
			Control.Arrow.first fst {-traversalOrder-}
		)
	 ) . Control.Parallel.Strategies.withStrategy (
		Control.Parallel.Strategies.parList $ Control.Parallel.Strategies.parTuple2 Control.Parallel.Strategies.r0 Control.Parallel.Strategies.rdeepseq {-weightedMeanOfTimetableCriteria-}
	 ) . map (
		\traversalOrder -> Control.Arrow.first (
			(,) traversalOrder	-- Document the means by which this solution was created.
		) . getTimetableFor $ generateStudentViewTimetableCoordinateVector traversalOrder
	 )

	generateStudentViewTimetableCoordinateVector :: Model.TimetableAxisTriple.Axes -> StudentView.TimetableCoordinates.Vector timeslotId
	generateStudentViewTimetableCoordinateVector traversalOrder	= filter (
		uncurry (
			ProblemConfiguration.ProblemAnalysis.isFree problemParameters
		) . (
			Model.TimetableCoordinates.getTime &&& (studentBodyRegister !) . Model.TimetableCoordinates.getObserverId
		) -- Select only those times, when the student is both available & not booked for a meeting.
	 ) . Model.Traverse.generateRasterCoordinates traversalOrder (
		Data.List.sortBy (
			Data.Ord.comparing Aggregate.StudentBody.getSize	-- More meaningful than the native alphabetical order.
		) $ Aggregate.StudentBodyRegister.getStudentBodies studentBodyRegister
	 ) $ ProblemConfiguration.ProblemAnalysis.getTimeslotIdRange problemAnalysis where
		studentBodyRegister	= ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters

	getTimetableFor
		:: StudentView.TimetableCoordinates.Vector timeslotId
		-> (
			(
				(
					StudentView.Timetable.Timetable timeslotId locationId teacherId level,
					[Maybe (Implementation.DeterministicConstructor.LessonCriteriaStatistics criterionValue mean standardDeviation)]
				),
				[Maybe criterionValue]
			),
			weightedMean
		) -- Pair.
	getTimetableFor = (
		\pair -> (
			(,) pair . Implementation.QuantifiedStudentViewTimetable.getTimetableCriteriaValues &&& Implementation.QuantifiedStudentViewTimetable.getWeightedMeanOfTimetableCriteria
		) . Implementation.QuantifiedStudentViewTimetable.mkQuantifiedStudentViewTimetable problemParameters executionOptions problemAnalysis $ fst {-studentViewTimetable-} pair -- Evaluate the initial deterministic timetable.
	 ) . Control.Monad.Writer.runWriter . Implementation.DeterministicConstructor.mutateStudentViewTimetable problemParameters executionOptions problemAnalysis (
		ProblemConfiguration.ProblemAnalysis.getFreeStudentViewTimetable problemAnalysis
	 )

{- |
	* Repeatedly randomly mutates the /lesson-criteria weights/ over the specified number of trials,
	in an attempt to improve them according to the weighted mean over heterogeneous /timetable-criteria/ for the resulting /deterministic student-view timetable/.

	* The proposed set of /lesson-criteria weights/ is considered to be improved, if the mean over (or maximum of) each raster-scan, of the weighted-mean over /timetable-criteria/, improves.

	* After finding a better set of /lesson-criteria weights/, the /change-magnitude/ is reduced by multiplication by the specified /reduction-factor/;
	see 'ExecutionConfiguration.ExecutionOptions.getOptimiseLessonCriteriaWeights'.
-}
optimiseLessonCriteriaWeights :: (
	Control.Parallel.Strategies.NFData	weightedMean,
	Data.Array.IArray.Ix			timeslotId,
	Enum					criterionValue,
	Enum					criterionWeight,
	Enum					timeslotId,
	Eq					campus,
	Floating				standardDeviation,
	Fractional				criterionValue,
	Fractional				criterionWeight,
	Fractional				mean,
	Fractional				weightedMean,
	Ord					level,
	Ord					locationId,
	Ord					stream,
	Ord					synchronisationId,
	Ord					teacherId,
	Real					criterionValue,
	Real					criterionWeight,
	RealFrac				teachingRatio,
	Real					weightedMean,
	Show					criterionWeight,
	Show					level,
	Show					locationId,
	Show					teacherId,
	Show					timeslotId,
	System.Random.Random			criterionWeight,
	System.Random.RandomGen			randomGen
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Maybe (StudentView.Timetable.Timetable timeslotId locationId teacherId level)	-- ^ Where an initial /student-view timetable/ has been read from file.
	-> randomGen										-- ^ Only used if @ ExecutionConfiguration.ExecutionOptions.getOptimiseLessonCriteriaWeights.isRequired @.
	-> Control.Monad.Writer.Writer (OutputFormat.EvolutionStrategyStatistics.OptimiseLessonCriteriaWeightsLog criterionWeight weightedMean) (
		ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio,
		OutputFormat.DeterministicStudentViewTimetableSelection.DeterministicStudentViewTimetableSelection criterionValue criterionWeight level locationId mean standardDeviation teacherId timeslotId weightedMean
	) -- ^ A Writer which logs the magnitude of the perturbation & the increase in the mean or maximum over all specified raster-scans of the weighted mean over heterogeneous timetable-criteria, & carries a payload of (The modified execution-options, The initial deterministic timetable & various statistics).
optimiseLessonCriteriaWeights problemParameters executionOptions problemAnalysis maybeStudentViewTimetable	= fmap (
	ToolShed.Data.Triple.getSecond &&& ToolShed.Data.Triple.getThird
 ) . foldr (
	\(trialNumber, randomGen) writer	-> do
		originalTriple@(changeMagnitude, _, _)	<- writer	-- Extract the payload from the monad.

		let
			proposedExecutionOptions	= executionOptions {
				ExecutionConfiguration.ExecutionOptions.getLessonCriteriaWeights	= ExecutionConfiguration.LessonCriteriaWeights.perturbWeights randomGen changeMagnitude . ExecutionConfiguration.ExecutionOptions.getLessonCriteriaWeights $ ToolShed.Data.Triple.getSecond {-execution-options-} originalTriple
			} -- Randomly mutate the lesson-criteria weights.

			proposedTriple	= (
				changeMagnitude * ExecutionConfiguration.OptimiseLessonCriteriaWeights.getReductionFactor optimiseLessonCriteriaWeightsData,	-- Reduce the perturbation-magnitude.
				proposedExecutionOptions,
				selectDeterministicStudentViewTimetable problemParameters proposedExecutionOptions problemAnalysis Nothing {-proposing an initial studentViewTimetable makes no sense-}
			 )

			changeBetweenRasterScansOfWeightedMeanOverTimetableCriteria	= uncurry (-) $ ToolShed.Data.Pair.mirror (
				either id (
					(
						if ExecutionConfiguration.OptimiseLessonCriteriaWeights.getUseMeanOverRasterScans optimiseLessonCriteriaWeightsData
							then Factory.Math.Statistics.getMean
							else maximum
					) . map snd {-weighted mean over timetable-criteria-}
				) . OutputFormat.DeterministicStudentViewTimetableSelection.getWeightedMeanOfTimetableCriteria . ToolShed.Data.Triple.getThird
			 ) (proposedTriple, originalTriple)
		if changeMagnitude == 0 {-avoid unnecessary evaluation-} || changeBetweenRasterScansOfWeightedMeanOverTimetableCriteria <= 0
			then return {-to Writer-monad-} originalTriple
			else do
				Control.Monad.Writer.tell [(trialNumber, changeMagnitude, changeBetweenRasterScansOfWeightedMeanOverTimetableCriteria)]

				return {-to Writer-monad-} proposedTriple
 ) (
	return {-to Writer-monad-} (
		ExecutionConfiguration.OptimiseLessonCriteriaWeights.getChangeMagnitude optimiseLessonCriteriaWeightsData,
		executionOptions,
		selectDeterministicStudentViewTimetable problemParameters executionOptions problemAnalysis maybeStudentViewTimetable
	) {-initial triple-}
 ) . reverse . zip [1 ..] . take (
	ExecutionConfiguration.OptimiseLessonCriteriaWeights.getNTrials optimiseLessonCriteriaWeightsData
 ) . ToolShed.System.Random.randomGens where
	optimiseLessonCriteriaWeightsData	= ExecutionConfiguration.ExecutionOptions.getOptimiseLessonCriteriaWeights executionOptions

