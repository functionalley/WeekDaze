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

 [@DESCRIPTION@]

	* Defines the structure produced by module 'WeekDaze.Implementation.EvolutionaryAlgorithm.evolveStudentViewTimetable'.

	* Renders this structure in either text or XHTML.
-}

module WeekDaze.OutputFormat.EvolutionStrategyStatistics(
-- * Types
-- ** Type-synonyms
--	NGenerations,
	OptimiseLessonCriteriaWeightsLog,
-- ** Data-types
	EvolutionStrategyStatistics(
--		MkEvolutionStrategyStatistics,
--		getDepletionStrategyName,
--		getReconstructionStrategyName,
--		getInitialFecundity,
--		getFinalFecundity,
--		getMaybeImprovementFactor,
		getWeightedMeanOfTimetableCriteria
--		getNGenerations,
--		getMaybeTimetableCriteriaValues
	),
-- * Constants
--	notApplicable,
--	runtimeInformationCSSIdentifier,
--	timetableAxisCSSIdentifier,
-- * Functions
	composeRuntimeLog,
	toHtml,
--	showReal
-- ** Constructor
	mkEvolutionStrategyStatistics,
-- ** Predicates
--	isFecundityUnchanged
) where

import			Control.Arrow((***), (&&&))
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	Factory.Math.Statistics
import qualified	Text.Printf
import qualified	Text.XHtml.Strict
import qualified	WeekDaze.Colour.HTMLColour						as Colour.HTMLColour
import qualified	WeekDaze.ExecutionConfiguration.CriterionWeight				as ExecutionConfiguration.CriterionWeight
import qualified	WeekDaze.ExecutionConfiguration.EvolutionStrategies			as ExecutionConfiguration.EvolutionStrategies
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions			as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.ExecutionConfiguration.LessonCriteriaWeights			as ExecutionConfiguration.LessonCriteriaWeights
import qualified	WeekDaze.ExecutionConfiguration.OptimiseLessonCriteriaWeights		as ExecutionConfiguration.OptimiseLessonCriteriaWeights
import qualified	WeekDaze.ExecutionConfiguration.TimetableBreederFecundity		as ExecutionConfiguration.TimetableBreederFecundity
import qualified	WeekDaze.ExecutionConfiguration.TimetableCriteriaWeights		as ExecutionConfiguration.TimetableCriteriaWeights
import qualified	WeekDaze.Model.TimetableAxisTriple					as Model.TimetableAxisTriple
import qualified	WeekDaze.Model.TimetableForWeek						as Model.TimetableForWeek
import qualified	WeekDaze.OutputConfiguration.Options					as OutputConfiguration.Options
import qualified	WeekDaze.OutputFormat.DeterministicStudentViewTimetableSelection	as OutputFormat.DeterministicStudentViewTimetableSelection
import qualified	WeekDaze.Text.CSS							as Text.CSS
import			Text.XHtml.Strict((+++), (<<), (!))

-- | Used in text & html output.
notApplicable :: String
notApplicable	= "N/A"

-- | A CSS class-label, for runtime-information tables.
runtimeInformationCSSIdentifier :: Text.CSS.CSSIdentifier
runtimeInformationCSSIdentifier	= "runtimeInformation"

-- | A number of generations in the evolution of the /timetable/.
type NGenerations	= Int

-- | Identifier used for the CSS-class for timetable-axes
timetableAxisCSSIdentifier :: Text.CSS.CSSIdentifier
timetableAxisCSSIdentifier	= "timetableAxis"

-- | Show the specified real number to a specific number of decimal places.
showReal :: Real r => OutputConfiguration.Options.NDecimalDigits -> r -> String
showReal nDecimalDigits		= (Text.Printf.printf "%.*f" nDecimalDigits :: Double -> String) . realToFrac

-- | Defines the ancillary data written by 'Implementation.EvolutionaryAlgorithm.evolveStudentViewTimetable', for one /evolution-strategy/.
data EvolutionStrategyStatistics weightedMean criterionValue	= MkEvolutionStrategyStatistics {
	getDepletionStrategyName		:: String,							-- ^ The name of the depletion-strategy used to liberate space in the candidate timetable.
	getReconstructionStrategyName		:: String,							-- ^ The name of the reconstruction-strategy used to re-populate the candidate timetable.
	getInitialFecundity			:: ExecutionConfiguration.TimetableBreederFecundity.Fecundity,	-- ^ The initial value of the fecundity.
	getFinalFecundity			:: ExecutionConfiguration.TimetableBreederFecundity.Fecundity,	-- ^ The value to which the initial fecundity was reduced, when the evolutionary process terminated.
	getMaybeImprovementFactor		:: Maybe weightedMean,						-- ^ The relative improvement in the /timetable/ resulting from this evolution-strategy relative to the previous, according to the weighted mean over the values of heterogeneous /timetable-criteria/; infinity is represented by 'Nothing'.
	getWeightedMeanOfTimetableCriteria	:: weightedMean,						-- ^ The /fitness/ of the /timetable/, according to the weighted mean over the values of heterogeneous /timetable-criteria/.
	getNGenerations				:: NGenerations,						-- ^ The number of generations through which the evolution progressed.
	getMaybeTimetableCriteriaValues		:: Maybe [Maybe criterionValue]					-- ^ Given at least one productive generation, the values of heterogeneous /timetable-criteria/ (where the corresponding weight is non-zero), for the selected candidate.
}

-- Constructor.
mkEvolutionStrategyStatistics
	:: String							-- ^ The name of the depletion-strategy used to liberate space in the candidate timetable.
	-> String							-- ^ The name of the reconstruction-strategy used to re-populate the candidate timetable.
	-> ExecutionConfiguration.TimetableBreederFecundity.Fecundity	-- ^ The value of the initial fecundity.
	-> ExecutionConfiguration.TimetableBreederFecundity.Fecundity	-- ^ The value to which the initial fecundity was reduced, when the evolutionary process terminated.
	-> Maybe weightedMean						-- ^ The relative improvement in the /timetable/ resulting from this evolution-strategy relative to the previous, according to the weighted mean over the values of heterogeneous /timetable-criteria/.
	-> weightedMean							-- ^ The /fitness/ of the /timetable/, according to the weighted mean over the values of heterogeneous /timetable-criteria/.
	-> NGenerations							-- ^ The number of generations through which the evolution progressed.
	-> Maybe [Maybe criterionValue]					-- ^ Given at least one productive generation, the values of heterogeneous /timetable-criteria/ (where the corresponding weight is non-zero), for the selected candidate.
	-> EvolutionStrategyStatistics weightedMean criterionValue
mkEvolutionStrategyStatistics depletionStrategyName reconstructionStrategyName initialFecundity finalFecundity maybeImprovement weightedMeanOfTimetableCriteria nGenerations maybeTimetableCriteriaValues	= MkEvolutionStrategyStatistics {
	getDepletionStrategyName		= depletionStrategyName,
	getReconstructionStrategyName		= reconstructionStrategyName,
	getInitialFecundity			= initialFecundity,
	getFinalFecundity			= finalFecundity,
	getMaybeImprovementFactor		= maybeImprovement,
	getWeightedMeanOfTimetableCriteria	= weightedMeanOfTimetableCriteria,
	getNGenerations				= nGenerations,
	getMaybeTimetableCriteriaValues		= maybeTimetableCriteriaValues
}

-- | True if the initial & final fecundities are identical.
isFecundityUnchanged :: EvolutionStrategyStatistics weightedMean criterionValue -> Bool
isFecundityUnchanged	= uncurry (==) . (getInitialFecundity &&& getFinalFecundity)

-- ^ The log of the optimisation of /lesson-criteria weight/s, based on maximisation of the weighted-mean over heterogeneous /timetable-criteria/, for the initial deterministic timetable.
type OptimiseLessonCriteriaWeightsLog criterionWeight weightedMean	= [(Int {-trial-number-}, criterionWeight, weightedMean)]

-- | Compose the text returned lazily at runtime, to aid diagnosis.
composeRuntimeLog :: (
	Real	criterionValue,
	Real	criterionWeight,
	Real	mean,
	Real	standardDeviation,
	Real	weightedMean
 )
	=> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> OutputConfiguration.Options.NDecimalDigits						-- ^ The required precision in decimal digits.
	-> (Bool, Bool)										-- ^ Whether the /traversal-order/ was specified, & whether the input file-path was specified.
	-> Bool											-- ^ Whether the fecundity of all /evolution-strategies/ have been set to zero.
	-> ExecutionConfiguration.LessonCriteriaWeights.LessonCriteriaWeights criterionWeight	-- ^ The unoptimised /lesson-criteria/ weights.
	-> Bool											-- ^ Whether to accept a proposed set of /lesson-criteria weights/ on the basis of the mean (as opposed to the maximum) over the specified raster-scans, of the weighted mean over all heterogeneous /timetable-criteria/.
	-> OptimiseLessonCriteriaWeightsLog criterionWeight weightedMean
	-> OutputFormat.DeterministicStudentViewTimetableSelection.DeterministicStudentViewTimetableSelection criterionValue criterionWeight level locationId mean standardDeviation teacherId timeslotId weightedMean
	-> [EvolutionStrategyStatistics weightedMean criterionValue]
	-> [Maybe criterionValue]								-- ^ The final values of timetable-criteria.
	-> [(String, String)]
composeRuntimeLog executionOptions nDecimalDigits (traversalOrderWasSpecified, inputFilePathWasSpecified) areAllEvolutionStrategiesZero originalLessonCriteriaWeights useMeanOverRasterScans optimiseLessonCriteriaWeightsLog deterministicStudentViewTimetableSelection evolutionStrategyStatisticsList finalTimetableCriteriaValues	= Data.Maybe.catMaybes [
	if null optimiseLessonCriteriaWeightsLog
		then Nothing
		else Just (
			"By randomly mutating lesson-criteria weights by the specified magnitude, the " ++ (
				if useMeanOverRasterScans then "mean over" else "maximum of"
			) ++ " all specified raster-scans, of the weighted mean over heterogeneous timetable-criteria, for the " ++ hintText ++ ", was sequentially improved by",
			showListOfStrings $ map (
				\(trialNumber, changeMagnitude, deltaOverRastersOfWeightedMeanOverTimetableCriteria) -> showChar '(' . shows trialNumber . showChar ',' $ showReal nDecimalDigits changeMagnitude ++ ")=>" ++ showReal nDecimalDigits deltaOverRastersOfWeightedMeanOverTimetableCriteria
			) optimiseLessonCriteriaWeightsLog
		), -- Pair.
	let
		preamble	= "The weighted mean over the values of heterogeneous timetable-criteria, of the "
	in Just . either (
		(,) (preamble ++ hintText) . showUnitInterval
	) (
		\weightedMeanOfTimetableCriteriaByTraversalOrder -> (
			preamble ++ "deterministic timetable resulting from each raster-scan; & the maximum",
			showListOfReals (map snd weightedMeanOfTimetableCriteriaByTraversalOrder) ++ "; " ++ uncurry (++) (
				show *** showString "=>" . showUnitInterval $ Data.List.maximumBy (Data.Ord.comparing snd) weightedMeanOfTimetableCriteriaByTraversalOrder
			)
		) -- Pair.
	) $ OutputFormat.DeterministicStudentViewTimetableSelection.getWeightedMeanOfTimetableCriteria deterministicStudentViewTimetableSelection,
	Just (
		"The value of each timetable-criterion, for the " ++ hintText,
		showListOfReals . map (Data.Maybe.fromMaybe 0) $ OutputFormat.DeterministicStudentViewTimetableSelection.getInitialTimetableCriteriaValues deterministicStudentViewTimetableSelection
	), -- Pair.
	let
		initialLessonCriteriaStatistics	= OutputFormat.DeterministicStudentViewTimetableSelection.getInitialLessonCriteriaStatistics deterministicStudentViewTimetableSelection
	in if null initialLessonCriteriaStatistics
		then Nothing
		else Just (
			"The (weight, (mean, standard deviation), (minimum, maximum)) gathered over the lessons of the " ++ hintText ++ ", for the value of each individual lesson-criterion",
			showListOfStrings . map (
				\((_, lessonCriterionWeightAccessor), maybeLessonCriteriaStatistics)	-> Data.Maybe.maybe notApplicable (
					\lessonCriterionStatistics -> let
						[lessonCriterionWeight, lessonCriterionWeight']	= map (
							showUnitInterval . ExecutionConfiguration.CriterionWeight.deconstruct . lessonCriterionWeightAccessor
						 ) [
							originalLessonCriteriaWeights,
							ExecutionConfiguration.ExecutionOptions.getLessonCriteriaWeights executionOptions
						 ]

						((mean, standardDeviation), (minimumValue, maximumValue))	= (showUnitInterval *** showUnitInterval) *** (showUnitInterval *** showUnitInterval) $ lessonCriterionStatistics
					in Text.Printf.printf "(%s,(%s,%s),(%s,%s))" (
						if null optimiseLessonCriteriaWeightsLog || lessonCriterionWeight == lessonCriterionWeight'
							then lessonCriterionWeight
							else lessonCriterionWeight ++ "->" ++ lessonCriterionWeight'
					) mean standardDeviation minimumValue maximumValue
				) maybeLessonCriteriaStatistics
			) $ zip ExecutionConfiguration.LessonCriteriaWeights.associationList initialLessonCriteriaStatistics
		), -- Pair
	if areAllEvolutionStrategiesZero
		then Nothing
		else Just (
			"The (weighted mean over heterogeneous timetable-criteria, relative improvement in the weighted mean over heterogeneous timetable-criteria, the number of generations through which the timetable evolved, the fecundity, & the value of each timetable-criterion for the best candidate), for each evolution-strategy",
			showListOfStrings $ map (
				\evolutionStrategyStatistics -> Text.Printf.printf "%s/%s=(%s, %s, %d, %s, %s)" (
					getDepletionStrategyName evolutionStrategyStatistics
				) (
					getReconstructionStrategyName evolutionStrategyStatistics
				) (
					showUnitInterval $ getWeightedMeanOfTimetableCriteria evolutionStrategyStatistics
				) (
					Data.Maybe.maybe "Infinity" (
						\improvementFactor -> if improvementFactor == 0
							then "0"
							else showString (showReal nDecimalDigits (100 * improvementFactor)) "%"
					) $ getMaybeImprovementFactor evolutionStrategyStatistics
				) (
					getNGenerations evolutionStrategyStatistics
				) (
					(
						\(x, y)	-> show x ++ if x == y
							then ""
							else "->" ++ show y
					) $ (getInitialFecundity &&& getFinalFecundity) evolutionStrategyStatistics
				) (
					Data.Maybe.maybe notApplicable showListOfStrings $ map (
						Data.Maybe.maybe notApplicable showUnitInterval
					) `fmap` getMaybeTimetableCriteriaValues evolutionStrategyStatistics
				)
			) evolutionStrategyStatisticsList
		), -- Pair.
	if areAllEvolutionStrategiesZero
		then Nothing
		else Just (
			"The final value of each timetable-criterion, & the improvement, from the " ++ hintText,
			showListOfStrings . zipWith (
				\maybeFinalWeightedTimetableCriterion maybeInitialWeightedTimetableCriterion	-> case (maybeFinalWeightedTimetableCriterion, maybeInitialWeightedTimetableCriterion) of
					(Just finalWeightedTimetableCriterion, Just initialWeightedTimetableCriterion)	-> Text.Printf.printf "(%s,%s)" (
						showUnitInterval finalWeightedTimetableCriterion
					 ) . showUnitInterval $ finalWeightedTimetableCriterion - initialWeightedTimetableCriterion
					_										-> notApplicable
			) finalTimetableCriteriaValues $ OutputFormat.DeterministicStudentViewTimetableSelection.getInitialTimetableCriteriaValues deterministicStudentViewTimetableSelection
		) -- Pair.
 ] where
	hintText :: String
	hintText
		| traversalOrderWasSpecified	= "deterministic timetable resulting from all specified raster-scans"	-- There may be more than one.
		| inputFilePathWasSpecified	= "specified timetable"
		| otherwise			= "best deterministic timetable"

	showListOfStrings :: [String] -> String
	showListOfStrings	= showChar '[' . (`showString` "]") . Data.List.intercalate ", "

	showUnitInterval :: Real r => r -> String
	showUnitInterval 0	= "0"
	showUnitInterval 1	= "1"
	showUnitInterval x	= showReal nDecimalDigits x

	showListOfReals :: Real r => [r] -> String
	showListOfReals	= showListOfStrings . map showUnitInterval

-- | Render the results in XHTML.
toHtml :: (
	RealFrac	criterionValue,
	RealFrac	criterionWeight,
	RealFrac	standardDeviation,
	RealFrac	mean,
	RealFrac	weightedMean,
	Show		criterionValue,
	Show		criterionWeight,
	Show		mean,
	Show		standardDeviation,
	Show		weightedMean
 )
	=> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> OutputConfiguration.Options.NDecimalDigits						-- ^ The required number of decimal digits.
	-> ExecutionConfiguration.LessonCriteriaWeights.LessonCriteriaWeights criterionWeight	-- ^ The unoptimised /lesson-criteria/ weights.
	-> OptimiseLessonCriteriaWeightsLog criterionWeight weightedMean
	-> OutputFormat.DeterministicStudentViewTimetableSelection.DeterministicStudentViewTimetableSelection criterionValue criterionWeight level locationId mean standardDeviation teacherId timeslotId weightedMean
	-> [EvolutionStrategyStatistics weightedMean criterionValue]
	-> Text.XHtml.Strict.Html
toHtml executionOptions nDecimalDigits originalLessonCriteriaWeights optimiseLessonCriteriaWeightsLog deterministicStudentViewTimetableSelection evolutionStrategyStatisticsList	= (
	if null optimiseLessonCriteriaWeightsLog
		then Text.XHtml.Strict.noHtml
		else Text.XHtml.Strict.paragraph ! [
			Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier
		] << (
			"By randomly mutating lesson-criteria weights by the specified magnitude, the " ++ (
				if useMeanOverRasterScans then "mean over" else "maximum of"
			) ++ " all specified raster-scans, of the weighted mean over heterogeneous timetable-criteria, for the initial deterministic timetable, was improved by; " +++ Data.List.intersperse (
				Text.XHtml.Strict.toHtml ", "
			) (
				map (
					\(trialNumber, changeMagnitude, deltaOverRastersOfWeightedMeanOverTimetableCriteria)	-> '(' +++ (
						Text.XHtml.Strict.thespan ! [
							Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier,
							Text.XHtml.Strict.title "Trial-number."
						] << trialNumber
					) +++ ',' +++ (
						Text.XHtml.Strict.thespan ! [
							Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier,
							Text.XHtml.Strict.title "Change-magnitude."
						] << showReal nDecimalDigits changeMagnitude
					) +++ ')' +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.primHtmlChar "rArr" +++ Text.XHtml.Strict.spaceHtml +++ (
						colourUnitIntervalSpan deltaOverRastersOfWeightedMeanOverTimetableCriteria ! [
							Text.XHtml.Strict.title	$ "Incremental improvement in the " ++ (
								if useMeanOverRasterScans then "mean over" else "maximum of"
							) ++ " all specified raster-scans, of the weighted mean over heterogeneous timetable-criteria."
						]
					)
				) optimiseLessonCriteriaWeightsLog
			) +++ "."
		)
 ) +++ either (
	(
		Text.XHtml.Strict.paragraph ! [
			Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier
		] <<
	) . (
		(
			"The weighted mean value over heterogeneous timetable-criteria of the single deterministic timetable" +++ Data.Maybe.maybe Text.XHtml.Strict.noHtml (
				(
					Text.XHtml.Strict.thespan ! [
						Text.XHtml.Strict.theclass Text.CSS.dataCSSIdentifier
					] <<
				) . either (
					showString " resulting from the raster-scan " . show
				) (
					showString " specified in the file " . show
				)
			) (
				ExecutionConfiguration.ExecutionOptions.getMaybeHint executionOptions
			) +++ ", is "
		) +++
	) . (
		+++ "."	-- Terminate
	) . colourUnitIntervalSpan	-- ExecutionConfiguration.ExecutionOptions.getMaybeHint was specified.
 ) (
	\weightedMeanOfTimetableCriteriaByTraversalOrder -> Text.XHtml.Strict.table ! [
		Text.XHtml.Strict.theclass runtimeInformationCSSIdentifier,
		Text.XHtml.Strict.identifier "weightedMeanOfTimetableCriteriaByTraversalOrderTable"
	] << (
		[
			Text.XHtml.Strict.tr << (
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.colspan 4	-- Spans axis-triple & the weighted-mean over the values of heterogeneous timetable-criteria.
				] << (
					"The Weighted Mean over the values of Heterogeneous Timetable-criteria," +++ Text.XHtml.Strict.br +++ "of the Deterministic Timetable resulting from each Raster-scan."
				)
			),
			Text.XHtml.Strict.tr << [
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.colspan 3	-- Spans axis-triple.
				] << "Slowest-changing to Fastest-changing Coordinate.",
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.title "Weighted Mean over the values of Heterogeneous Timetable-criteria."
				] << mu
			]
		] ++ map (
			\(timetableAxisTriple, weightedMeanOfTimetableCriteria)	-> (
				Text.XHtml.Strict.tr ! if timetableAxisTriple == fst (Data.List.maximumBy (Data.Ord.comparing snd) weightedMeanOfTimetableCriteriaByTraversalOrder)
					then [
						Text.XHtml.Strict.identifier "selectedRaster",
						Text.XHtml.Strict.title "Selected Raster-scan."
					]
					else []
			) << let
				(x, y, z)	= Model.TimetableAxisTriple.deconstruct timetableAxisTriple
				renderAxis	= (
					Text.XHtml.Strict.td ! [
						Text.XHtml.Strict.theclass timetableAxisCSSIdentifier
					] <<
				 ) . show
			in [
				renderAxis x,
				renderAxis y,
				renderAxis z,
				colourUnitIntervalTD weightedMeanOfTimetableCriteria
			]
		) weightedMeanOfTimetableCriteriaByTraversalOrder ++ [
			Text.XHtml.Strict.tr << (
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.colspan 3,	-- Spans axis-triple.
					Text.XHtml.Strict.title "The Mean, over the Weighted Mean, over the values of Heterogeneous Timetable-criteria."
				] << mu +++ colourUnitIntervalTD (
					Factory.Math.Statistics.getMean $ map snd {-weightedMean-} weightedMeanOfTimetableCriteriaByTraversalOrder :: Rational
				)
			)
		]
	)
 ) (
	OutputFormat.DeterministicStudentViewTimetableSelection.getWeightedMeanOfTimetableCriteria deterministicStudentViewTimetableSelection
 ) +++ let
	initialLessonCriteriaStatistics	= OutputFormat.DeterministicStudentViewTimetableSelection.getInitialLessonCriteriaStatistics deterministicStudentViewTimetableSelection
 in (
	if null initialLessonCriteriaStatistics
		then Text.XHtml.Strict.noHtml	-- Otherwise mean of null list fails.
		else Text.XHtml.Strict.table ! [
			Text.XHtml.Strict.theclass runtimeInformationCSSIdentifier,
			Text.XHtml.Strict.identifier "initialLessonCriteriaStatisticsTable"
		] << (
			let
				statisticsHeaders :: [Text.XHtml.Strict.Html]
				statisticsHeaders	= map (
					\(title, contents) -> Text.XHtml.Strict.th ! [
						Text.XHtml.Strict.title title
					] << contents
				 ) [
					(
						"The mean value",
						mu
					), (
						"The standard deviation.",
						sigma
					), (
						"The minimum value.",
						Text.XHtml.Strict.toHtml "Min."
					), (
						"The maximum value.",
						Text.XHtml.Strict.toHtml "Max."
					)
				 ]

				nStatisticsHeaders :: Int
				nStatisticsHeaders	= length statisticsHeaders
			in [
				Text.XHtml.Strict.tr << (
					Text.XHtml.Strict.th ! [
						Text.XHtml.Strict.colspan $ (
							if wereOptimisedLessonCriteriaWeights
								then succ	-- Spans 'Optimised'.
								else id
						) (
							2 {-spans 'Name' & 'Original'-} + nStatisticsHeaders
						)
					] << (
						"Statistics gathered for the values of each Lesson-criterion in isolation," +++ Text.XHtml.Strict.br +++ "Evaluated over each of the lessons of the best Deterministic Timetable."
					)
				),
				Text.XHtml.Strict.tr << (
					[
						Text.XHtml.Strict.th ! [
							Text.XHtml.Strict.rowspan 2
						] << "Name",
						Text.XHtml.Strict.th ! [
							if wereOptimisedLessonCriteriaWeights
								then Text.XHtml.Strict.colspan 2	-- Spans 'Original' & 'Optimised'.
								else Text.XHtml.Strict.rowspan 2,
							Text.XHtml.Strict.title "The weight of this criterion, in the weighted mean over the values of heterogeneous lesson-criteria."
						] << "Weight"
					] ++ [
						Text.XHtml.Strict.th ! [
							Text.XHtml.Strict.colspan nStatisticsHeaders,
							Text.XHtml.Strict.title "Statistics gathered for the values of each lesson-criterion in isolation, evaluated over each of the lessons of the best deterministic timetable."
						] << "Statistics"
					]
				),
				Text.XHtml.Strict.tr << (
					if wereOptimisedLessonCriteriaWeights
						then (
							map (Text.XHtml.Strict.th <<) ["Original", "Optimised"] ++
						) -- Section.
						else id
				) statisticsHeaders
			] ++ map (
				\((lessonCriterionTag, lessonCriterionWeightAccessor), maybeLessonCriteriaStatistics)	-> Text.XHtml.Strict.tr << (
					(
						(
							Text.XHtml.Strict.td ! [
								Text.XHtml.Strict.theclass "lessonCriterion"
							] << lessonCriterionTag
						) : let
							[lessonCriterionWeight, lessonCriterionWeight']	= map (
								ExecutionConfiguration.CriterionWeight.deconstruct . lessonCriterionWeightAccessor
							 ) [
								originalLessonCriteriaWeights,
								ExecutionConfiguration.ExecutionOptions.getLessonCriteriaWeights executionOptions
							 ]
						in if wereOptimisedLessonCriteriaWeights
							then if lessonCriterionWeight == lessonCriterionWeight'
								then [
									colourUnitIntervalTD lessonCriterionWeight ! [Text.XHtml.Strict.colspan 2]
								]
								else map colourUnitIntervalTD [lessonCriterionWeight, lessonCriterionWeight']
							else [
								colourUnitIntervalTD lessonCriterionWeight
							]
					) ++ Data.Maybe.maybe [
						Text.XHtml.Strict.td ! [
							Text.XHtml.Strict.colspan nStatisticsHeaders,
							Text.XHtml.Strict.theclass Model.TimetableForWeek.unavailableCSSIdentifier,
							Text.XHtml.Strict.title zeroWeight
						] << notApplicable
					] (
						\((mean, standardDeviation), (min', max'))	-> [
							colourUnitIntervalTD mean,
							colourUnitIntervalTD standardDeviation,
							colourUnitIntervalTD min',
							colourUnitIntervalTD max'
						]
					) maybeLessonCriteriaStatistics
				)
			) (
				zip ExecutionConfiguration.LessonCriteriaWeights.associationList initialLessonCriteriaStatistics
			)
		)
 ) +++ Text.XHtml.Strict.table ! [
	Text.XHtml.Strict.theclass runtimeInformationCSSIdentifier,
	Text.XHtml.Strict.identifier "evolutionStrategyStatisticsTable"
 ] << (
	let
		tableHeaders :: [Text.XHtml.Strict.Html]
		tableHeaders	= map (
			\(title, contents)	-> Text.XHtml.Strict.th ! [
				Text.XHtml.Strict.title title
			] << contents
		 ) [
			(
				"The strategy by which space is liberated in a candidate-timetable, to permit potentially beneficial mutation.",
				"Depletion"
			), (
				"The strategy by which a candidate-timetable is reconstructed, after application of the chosen depletion-strategy.",
				"Reconstruction"
			), (
				"The initial size of the breeding-program for this evolution-strategy.",
				"Initial"
			), (
				"The final size of the breeding-program on termination of this evolution-strategy, which may be less than the original size should that have proven excessive.",
				"Final"
			)
		 ]

		nColumns, nTimetableCriteria :: Int
		nColumns		= length tableHeaders + 3 {-double height headers from previous row-}
		nTimetableCriteria	= length ExecutionConfiguration.TimetableCriteriaWeights.associationList
	in [
		Text.XHtml.Strict.tr << (
			Text.XHtml.Strict.th ! [
				Text.XHtml.Strict.colspan $ nColumns + nTimetableCriteria
			] << (
				"The Improvement in the value of each Timetable-criterion, resulting from each Evolution-strategy, where " +++ Data.Maybe.maybe (
					Text.XHtml.Strict.toHtml "a Variable number of"
				) (
					(+++ " Initial") . (
						Text.XHtml.Strict.thespan ! [
							Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
						] <<
					) -- Section.
				) (
					ExecutionConfiguration.EvolutionStrategies.getMaybeNInitialScouts $ ExecutionConfiguration.ExecutionOptions.getEvolutionStrategies executionOptions
				) +++ " Scouts were Dispatched."
			)
		),
		Text.XHtml.Strict.tr ! [
			Text.XHtml.Strict.identifier "timetableCriteriaWeights"
		] << (
			(
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.colspan nColumns
				] << (
					"Timetable-criteria Weights" +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.primHtmlChar "rarr"
				)
			) : map (
				\(timetableCriterionTag, timetableCriterionWeightAccessor)	-> colourUnitIntervalTD (
					ExecutionConfiguration.CriterionWeight.deconstruct . timetableCriterionWeightAccessor $ ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights executionOptions
				) ! [
					Text.XHtml.Strict.title timetableCriterionTag
				]
			) ExecutionConfiguration.TimetableCriteriaWeights.associationList
		),
		Text.XHtml.Strict.tr ! [
			Text.XHtml.Strict.identifier "bestDeterministicWeightedTimetableCriteria"
		] << (
			(
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.colspan nColumns
				] << (
					"The value of each Timetable-criterion, for the Best Deterministic Timetable" +++ Text.XHtml.Strict.spaceHtml +++ Text.XHtml.Strict.primHtmlChar "rarr"
				)
			) : zipWith (
				\(timetableCriterionTag, _)	-> Data.Maybe.maybe (
					Text.XHtml.Strict.td ! [
						Text.XHtml.Strict.theclass Model.TimetableForWeek.unavailableCSSIdentifier,
						Text.XHtml.Strict.title zeroWeight
					] << notApplicable
				) (
					(! [Text.XHtml.Strict.title timetableCriterionTag]) . colourUnitIntervalTD
				)
			) ExecutionConfiguration.TimetableCriteriaWeights.associationList (
				OutputFormat.DeterministicStudentViewTimetableSelection.getInitialTimetableCriteriaValues deterministicStudentViewTimetableSelection
			)
		)
	] ++ if null evolutionStrategyStatisticsList
		then []
		else [
			Text.XHtml.Strict.tr << [
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.colspan 2,	-- Spans 'Depletion' & 'Reconstruction'.
					Text.XHtml.Strict.title "The means by which a candidate-timetable is mutated, in the search for improvement."
				] << "Chronologically ordered Evolution-strategies",
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.colspan 2,	-- Spans 'Initial' & 'Final'.
					Text.XHtml.Strict.title "The size of the breeding-program for this evolution-strategy."
				] << "Fecundity",
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.rowspan 2,	-- Double height.
					Text.XHtml.Strict.title "The number of generations through which the timetable evolved, using this evolution-strategy."
				] << "Generations",
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.rowspan 2,	-- Double height.
					Text.XHtml.Strict.title "The weighted mean over the values of heterogeneous timetable-criteria."
				] << mu,
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.rowspan 2,	-- Double height.
					Text.XHtml.Strict.title "The improvement in the weighted mean over the values of heterogeneous timetable-criteria, relative to the previous phase."
				] << delta,
				Text.XHtml.Strict.th ! [
					Text.XHtml.Strict.rowspan 2,	-- Double height.
					Text.XHtml.Strict.colspan nTimetableCriteria,
					Text.XHtml.Strict.title "The value of each timetable-criterion, for the best candidate resulting from a specific evolution-strategy."
				] << "Timetable-criterion Values"
			],
			Text.XHtml.Strict.tr << tableHeaders
		] ++ map (
			\evolutionStrategyStatistics -> let
				isFecundityUnchanged'	= isFecundityUnchanged evolutionStrategyStatistics
			in Text.XHtml.Strict.tr << (
				[
					Text.XHtml.Strict.td ! [
						Text.XHtml.Strict.theclass "depletionStrategy"
					] << getDepletionStrategyName evolutionStrategyStatistics,
					Text.XHtml.Strict.td ! [
						Text.XHtml.Strict.theclass "reconstructionStrategy"
					] << getReconstructionStrategyName evolutionStrategyStatistics,
					Text.XHtml.Strict.td ! [
						Text.XHtml.Strict.colspan $ if isFecundityUnchanged' then 2 else 1,
						Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
					] << getInitialFecundity evolutionStrategyStatistics,
					if isFecundityUnchanged'
						then Text.XHtml.Strict.noHtml
						else Text.XHtml.Strict.td ! [
							Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
						] << getFinalFecundity evolutionStrategyStatistics,
					Text.XHtml.Strict.td ! [
						Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
					] << getNGenerations evolutionStrategyStatistics
				] ++ Data.Maybe.maybe [
					Text.XHtml.Strict.td ! [
						Text.XHtml.Strict.theclass Model.TimetableForWeek.unavailableCSSIdentifier,
						Text.XHtml.Strict.colspan $ nTimetableCriteria + 2 {-spans mu & delta-},
						Text.XHtml.Strict.title "Zero productive generations of evolution occurred."
					] << notApplicable
				] (
					(
						[
							colourUnitIntervalTD $ getWeightedMeanOfTimetableCriteria evolutionStrategyStatistics,
							Data.Maybe.maybe infiniteValue (
								\improvementFactor -> (
									Text.XHtml.Strict.td ! [
										Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier,
										Text.XHtml.Strict.thestyle $ "color: " ++ Colour.HTMLColour.unitIntervalToHTMLColourCode (1 - recip (1 + improvementFactor))	-- Map the interval [0, infinity) to [0, 1].
									] <<
								) . (`showString` "%") . showReal nDecimalDigits $ 100 * improvementFactor
							) $ getMaybeImprovementFactor evolutionStrategyStatistics
						] ++
					) . zipWith (
						\(timetableCriterionTag, _)	-> Data.Maybe.maybe (
							Text.XHtml.Strict.td ! [
								Text.XHtml.Strict.theclass Model.TimetableForWeek.unavailableCSSIdentifier,
								Text.XHtml.Strict.title zeroWeight
							] << notApplicable
						) (
							(! [Text.XHtml.Strict.title timetableCriterionTag]) . colourUnitIntervalTD
						)
					) ExecutionConfiguration.TimetableCriteriaWeights.associationList
				) (
					getMaybeTimetableCriteriaValues evolutionStrategyStatistics
				)
			)
		) evolutionStrategyStatisticsList ++ take 1 {-perhaps none-} (
			map (
				\finalTimetableCriteriaValues -> Text.XHtml.Strict.tr << (
					(
						Text.XHtml.Strict.th ! [
							Text.XHtml.Strict.title "The weighted change in the value of each timetable-criterion, from the best Deterministic Timetable.",
							Text.XHtml.Strict.colspan 7	-- Depletion, Reconstruction, Initial, Final, Generations, Mu, Delta.
						] << delta
					) : Data.List.zipWith3 (
						\(timetableCriterionTag, timetableCriterionWeightAccessor) maybeFinalTimetableCriterionValue maybeInitialTimetableCriterionValue	-> case (maybeFinalTimetableCriterionValue, maybeInitialTimetableCriterionValue) of
							(Just finalTimetableCriterionValue, Just initialTimetableCriterionValue)	-> let
								deltaTimetableCriterionValue	= finalTimetableCriterionValue - initialTimetableCriterionValue
							 in Text.XHtml.Strict.td ! [
								Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier,
								Text.XHtml.Strict.thestyle $ "color: " ++ Colour.HTMLColour.unitIntervalToHTMLColourCode ((1 + deltaTimetableCriterionValue) / 2),	-- Map the interval [-1, 1] -> [0, 1].
								Text.XHtml.Strict.title timetableCriterionTag
							 ] << Text.XHtml.Strict.lineToHtml {-embed nbsps-} (
								case deltaTimetableCriterionValue of
									0	-> Text.Printf.printf "%-*d" (nDecimalDigits + 2) (0 :: Int)	-- Left-justify in a fixed-width field.
									_	-> showReal nDecimalDigits . (
										* realToFrac deltaTimetableCriterionValue
									 ) . ExecutionConfiguration.CriterionWeight.deconstruct . timetableCriterionWeightAccessor $ ExecutionConfiguration.ExecutionOptions.getTimetableCriteriaWeights executionOptions
							 )
							_									-> Text.XHtml.Strict.td ! [
								Text.XHtml.Strict.theclass Model.TimetableForWeek.unavailableCSSIdentifier,
								Text.XHtml.Strict.title zeroWeight
							 ] << notApplicable
					) ExecutionConfiguration.TimetableCriteriaWeights.associationList finalTimetableCriteriaValues (
						OutputFormat.DeterministicStudentViewTimetableSelection.getInitialTimetableCriteriaValues deterministicStudentViewTimetableSelection
					)
				)
			) . Data.Maybe.mapMaybe getMaybeTimetableCriteriaValues {-weed-out those with zero productive generations-} $ reverse {-the last set of results is required-} evolutionStrategyStatisticsList
		)
 ) where
	wereOptimisedLessonCriteriaWeights, useMeanOverRasterScans :: Bool
	(wereOptimisedLessonCriteriaWeights, useMeanOverRasterScans)	= ExecutionConfiguration.OptimiseLessonCriteriaWeights.isRequired &&& ExecutionConfiguration.OptimiseLessonCriteriaWeights.getUseMeanOverRasterScans $ ExecutionConfiguration.ExecutionOptions.getOptimiseLessonCriteriaWeights executionOptions

	colourUnitInterval :: (RealFrac r, Show r) => (Text.XHtml.Strict.Html -> Text.XHtml.Strict.Html) -> r -> Text.XHtml.Strict.Html
	colourUnitInterval element x	= element ! [
		Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier,
		Text.XHtml.Strict.thestyle $ "color: " ++ Colour.HTMLColour.unitIntervalToHTMLColourCode x
	 ] << (
		Text.XHtml.Strict.lineToHtml {-embed nbsps-} . if x `elem` [0, 1]
			then (
				Text.Printf.printf "%-*d" (nDecimalDigits + 2) :: Int -> String	-- Left-justify in a fixed-width field.
			) . round
			else showReal nDecimalDigits
	 ) x

	colourUnitIntervalSpan, colourUnitIntervalTD :: (RealFrac r, Show r) => r -> Text.XHtml.Strict.Html
	colourUnitIntervalSpan	= colourUnitInterval Text.XHtml.Strict.thespan
	colourUnitIntervalTD	= colourUnitInterval Text.XHtml.Strict.td

	zeroWeight :: String
	zeroWeight	= "Zero weight"

	delta, mu, sigma, infinity :: Text.XHtml.Strict.Html
	[delta, mu, sigma, infinity]	= map (
		uncurry (<<) . (
			(Text.XHtml.Strict.abbr !) . return {-to List-monad-} . Text.XHtml.Strict.title &&& Text.XHtml.Strict.primHtmlChar
		)
	 ) ["Delta", "mu", "sigma", "infin"]

	infiniteValue	= Text.XHtml.Strict.td ! [
		Text.XHtml.Strict.title "Infinity"
	 ] << infinity

