{-# LANGUAGE CPP, MultiParamTypeClasses #-}
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

	Describes the availability of some /resource/.

 [@CAVEAT@]

	* Availability, as defined here, can only be defined for each whole /day/ in any unspecified week,
	rather than individual time-slots within a day, or sporadic days in the year.
	This restriction isn't conceptual, but merely for the tractability of the code.

	* Resources available for only part of a /day/, can't be directly accommodated.
-}

module WeekDaze.Temporal.Availability(
-- * Types
-- ** Data-types
	Availability(
		deconstruct
	),
-- * Constants
	tag,
--	fulltime,
--	unavailable,
-- * Functions
--	availabilityParser,
	calculateAvailabilityRatio,
	countDaysPerWeekAvailable,
	countInternalAvailabilityGaps,
	findIntersection,
	findIntersections,
--	findUnion,
	findUnions,
-- ** Constructor
	mkAvailability,
-- ** Predicates
	isFulltime,
	isUnavailable,
	isAvailableOn
#ifdef USE_HDBC
-- ** Translation
	,fromMySqlSet
	,toMySqlSet
#endif
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Size			as Size
import qualified	WeekDaze.Temporal.Day		as Temporal.Day

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	WeekDaze.Database.Selector	as Database.Selector

instance Data.Convertible.Convertible Database.HDBC.SqlValue Availability {-multi-parameter type-class-} where
	safeConvert	= fmap fromMySqlSet . Data.Convertible.safeConvert

-- | Constructs an /Availability/ from a MySql set.
fromMySqlSet :: String -> Availability
fromMySqlSet	= mkAvailability . Database.Selector.fromMySqlSet

-- | Constructs a MySql set from an /Availability/.
toMySqlSet :: Availability -> String
toMySqlSet	= Database.Selector.toMySqlSet . Data.Set.toList . deconstruct
#endif /* USE_HDBC */

-- | Used to qualify SQL & XML.
tag :: String
tag	= "availability"

-- | The set of /day/s on which an arbitrary /resource/ is /available/.
newtype Availability	= MkAvailability {
	deconstruct	:: Data.Set.Set Temporal.Day.Day
} deriving (Eq, Ord)

instance Show Availability where
	showsPrec _	= shows . Data.Set.toList . deconstruct	-- Hides the data-constructor.

instance Control.DeepSeq.NFData Availability where
	rnf	= Control.DeepSeq.rnf . deconstruct

-- | Constant.
fulltime :: Availability
fulltime	= mkAvailability Temporal.Day.range

-- | Constant.
unavailable :: Availability
unavailable	= MkAvailability Data.Set.empty

instance Data.Default.Default Availability where
	def	= fulltime

{- |
	* Smart constructor.

	* Prevents one directly constructing an un/available/ /resource/, though one can indirectly as a result of an operation which returns a new one; e.g. 'findIntersection'.
-}
mkAvailability :: [Temporal.Day.Day] -> Availability
mkAvailability days
	| ToolShed.SelfValidate.isValid availability	= availability
	| otherwise					= error $ "WeekDaze.Temporal.Availability.mkAvailability:\t" ++ ToolShed.SelfValidate.getFirstError availability ++ "."
	where
		availability	= MkAvailability $ Data.Set.fromList days

instance ToolShed.SelfValidate.SelfValidator Availability where
	getErrors availability	= ToolShed.SelfValidate.extractErrors [(isUnavailable availability, "no availability")]

instance HXT.XmlPickler Availability where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		mkAvailability,			-- Construct from a List.
		Data.Set.toList . deconstruct	-- Deconstruct to a List.
	 ) $ HXT.xpList1 {-can't be null-} HXT.xpickle

-- | The number of /day/s per week, which can be considered /available/.
countDaysPerWeekAvailable :: Availability -> Size.NDays
countDaysPerWeekAvailable	= Data.Set.size . deconstruct

-- | Counts non-terminal blocks of consecutive un/available/ /day/s.
countInternalAvailabilityGaps :: Availability -> Size.NDays
countInternalAvailabilityGaps available
	| isUnavailable available	= 0
	| otherwise			= length . filter (
		> 1		-- Select non-consecutive available days.
	) . uncurry (
		zipWith (-)	-- Measure the unavailable duration between successive available days.
	) . (
		map fromEnum . tail &&& map fromEnum . init
	) . Data.Set.toAscList $ deconstruct available

-- | Returns the ratio of the available portion of the week, to the whole week (including weekends).
calculateAvailabilityRatio :: Fractional f => Availability -> f
calculateAvailabilityRatio	= (/ fromIntegral Temporal.Day.nDaysPerWeek) . fromIntegral . countDaysPerWeekAvailable

-- | Returns the /day/s on which both /resource/s are /available/.
findIntersection :: Availability -> Availability -> Availability
findIntersection (MkAvailability l) (MkAvailability r)	= MkAvailability $ Data.Set.intersection l r

-- | Returns the /day/s on which all the specified /resource/s are /available/.
findIntersections :: Data.Foldable.Foldable foldable => foldable Availability -> Availability
findIntersections	= Data.Foldable.foldr findIntersection fulltime

-- | Returns the /day/s on which either /resource/ is /available/.
findUnion :: Availability -> Availability -> Availability
findUnion (MkAvailability l) (MkAvailability r)	= MkAvailability $ Data.Set.union l r

-- | Returns the /day/s on which any of the specified /resource/s is /available/.
findUnions :: Data.Foldable.Foldable foldable => foldable Availability -> Availability
findUnions	= Data.Foldable.foldr findUnion unavailable

-- | True if the /resource/ is available every /day/.
isFulltime :: Availability -> Bool
isFulltime	= (== Temporal.Day.nDaysPerWeek) . Data.Set.size . deconstruct

{- |
	* 'True' is the /resource/ is available on zero /day/s.

	* CAVEAT: an 'Availability' which is completely un/available/, can't be constructed by 'mkAvailability', but it can result from 'findIntersection'.
-}
isUnavailable :: Availability -> Bool
isUnavailable	= Data.Set.null . deconstruct

-- | True if the unspecified /resource/ is available on the specified /day/.
isAvailableOn :: Temporal.Day.Day -> Availability -> Bool
isAvailableOn day	= (day `Data.Set.member`) . deconstruct

