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

	* Defines an ordered triple of orthogonal /timetable/-axes.

	* Since these axes represent different concepts, the order in which they're traversed when defining the /lesson/ at each /time-slot/, is significant.
-}

module WeekDaze.Model.TimetableAxisTriple(
-- * Types
-- ** Type-synonyms
--	Triple,
-- ** Data-types
	Axes(
		deconstruct
	),
-- * Constants
	tag,
	permutations,
-- * Functions
	fromList,
	toList,
	invertSense,
	generatePermutationsOf,
-- ** Constructor
	mkAxes,
-- ** Predicates
--	areOrthogonal,
	hasWildSense
) where

import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.List
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Model.TimetableAxis		as Model.TimetableAxis
import qualified	WeekDaze.Model.TimetableAxisTraversal	as Model.TimetableAxisTraversal

-- | Used to qualify XML.
tag :: String
tag	= "traversalOrder"

{- |
	* Defines the order & sense, in which the axes of a /timetable/ are traversed.

	* Each traversal is a raster-scan over the coordinates of a /timetable/,
	but the order in which one nests the required iterations potentially affects both the efficiency & result.

	* The first axis specified changes least frequently during the raster-scan, & the last most frequently.

	* There are @ (3! * 2^3 = 48) @ permutations, since we distinguish between the /senses/ in which each axis can be traversed.
-}
type Triple	= (Model.TimetableAxisTraversal.AxisTraversal, Model.TimetableAxisTraversal.AxisTraversal, Model.TimetableAxisTraversal.AxisTraversal)

-- | Dummy data-type on which to hang instance-definitions.
newtype Axes	= MkAxes {
	deconstruct	:: Triple
} deriving Eq

instance Read Axes where
	readsPrec _	= map (Control.Arrow.first mkAxes) . reads

instance Show Axes where
	showsPrec _ (MkAxes triple)	= shows triple

instance ToolShed.SelfValidate.SelfValidator Axes where
	getErrors axes	= ToolShed.SelfValidate.extractErrors [(not $ areOrthogonal axes, "axes must be orthogonal; " ++ show axes)]

instance HXT.XmlPickler Axes where
	xpickle	= HXT.xpElem tag $ HXT.xpWrap (
		mkAxes,		-- Construct from a Triple.
		deconstruct	-- Deconstruct to a Triple.
	 ) $ HXT.xpTriple (
		HXT.xpElem "x" HXT.xpickle
	 ) (
		HXT.xpElem "y" HXT.xpickle
	 ) (
		HXT.xpElem "z" HXT.xpickle
	 )

instance Control.DeepSeq.NFData Axes where
	rnf	= Control.DeepSeq.rnf . deconstruct

-- | Smart constructor.
mkAxes :: Triple -> Axes
mkAxes triple
	| ToolShed.SelfValidate.isValid axes	= axes
	| otherwise				= error $ "WeekDaze.Model.TimetableAxisTriple.mkAxes:\t" ++ ToolShed.SelfValidate.getFirstError axes ++ "."
	where
		axes	= MkAxes triple

-- | True if the three specified axes are distinct.
areOrthogonal :: Axes -> Bool
areOrthogonal	= (== 3) . length . Data.List.nub . map Model.TimetableAxisTraversal.getAxis . toList

-- | Invert the sense of each axis, but not their order, thus reversing the raster.
invertSense :: Axes -> Axes
invertSense	= fromList . map Model.TimetableAxisTraversal.invertSense . toList

-- | Convert from a list of /axis-traversal/s.
fromList :: [Model.TimetableAxisTraversal.AxisTraversal] -> Axes
fromList [x, y, z]	= mkAxes (x, y, z)
fromList axisTraversals	= error $ "WeekDaze.Model.TimetableAxisTriple.mkAxes:\tprecisely three axes-traversals are required " ++ show axisTraversals ++ "."

-- | Convert into a list of /axis-traversal/s.
toList :: Axes -> [Model.TimetableAxisTraversal.AxisTraversal]
toList (MkAxes (x, y, z))	= [x, y, z]

-- | The constant list of all permutations of axis-order & sense of travel along each.
permutations :: [Axes]
permutations	= [
	mkAxes (
		Model.TimetableAxisTraversal.MkAxisTraversal (Just xSense) xAxis,
		Model.TimetableAxisTraversal.MkAxisTraversal (Just ySense) yAxis,
		Model.TimetableAxisTraversal.MkAxisTraversal (Just zSense) $ Model.TimetableAxis.getPerpendicular xAxis yAxis
	) |
		xSense	<- [minBound .. maxBound],
		xAxis	<- Model.TimetableAxis.range,
		ySense	<- [minBound .. maxBound],
		yAxis	<- Model.TimetableAxis.getOthers xAxis,
		zSense	<- [minBound .. maxBound]
 ] -- List-comprehension.

-- | The list of permutations of axis-order & sense of travel along each matching the supplied specification.
generatePermutationsOf :: Axes -> [Axes]
generatePermutationsOf axes@(MkAxes (x, y, z))
	| not $ hasWildSense axes	= [axes]	-- Not strictly necessary, since the main algorithm copes with this case.
	| otherwise			= [
		mkAxes (
			Model.TimetableAxisTraversal.MkAxisTraversal (Just xSense) $ Model.TimetableAxisTraversal.getAxis x,
			Model.TimetableAxisTraversal.MkAxisTraversal (Just ySense) $ Model.TimetableAxisTraversal.getAxis y,
			Model.TimetableAxisTraversal.MkAxisTraversal (Just zSense) $ Model.TimetableAxisTraversal.getAxis z
		) |
			xSense	<- Model.TimetableAxisTraversal.maybeSenseToList $ Model.TimetableAxisTraversal.getMaybeSense x,
			ySense	<- Model.TimetableAxisTraversal.maybeSenseToList $ Model.TimetableAxisTraversal.getMaybeSense y,
			zSense	<- Model.TimetableAxisTraversal.maybeSenseToList $ Model.TimetableAxisTraversal.getMaybeSense z
	] -- List-comprehension.

-- | True if the sense is ill-defined.
hasWildSense :: Axes -> Bool
hasWildSense	= any Model.TimetableAxisTraversal.hasWildSense . toList

