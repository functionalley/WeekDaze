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

	* Defines the fecundity of two independent instances of a particular evolution-strategy;
	one composed from a /deterministic timetable-constructor/ & the other from a /random timetable-constructor/.

	* It quantifies the number of candidate solutions bred at each generation of the evolution of the /timetable/.

	* If the fecundity is zero, then the corresponding strategy is effectively switched-off.
-}

module WeekDaze.ExecutionConfiguration.TimetableBreederFecundity(
-- * Types
-- ** Type-synonyms
	Fecundity,
-- ** Data-types
	TimetableBreederFecundity(
--		MkTimetableBreederFecundity,
		getDeterministicConstructorFecundity,
		getRandomConstructorFecundity
	),
-- * Constants
--	tag,
--	deterministicConstructorFecundityTag,
--	randomConstructorFecundityTag,
	zero,
-- * Functions
-- ** Constructor
	mkTimetableBreederFecundity,
-- ** Operators
	(>*<)
) where

import			Control.Arrow((&&&))
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	ToolShed.SelfValidate

infixl 7 >*<	-- Like (*).

-- | Used to qualify XML.
tag :: String
tag					= "timetableBreederFecundity"

-- | Used to qualify XML.
deterministicConstructorFecundityTag :: String
deterministicConstructorFecundityTag	= "deterministicConstructorFecundity"

-- | Used to qualify XML.
randomConstructorFecundityTag :: String
randomConstructorFecundityTag		= "randomConstructorFecundity"

-- | The number of candidate solutions to breed in any one generation of the evolution of a /timetable/.
type Fecundity	= Int

-- | Describes a pair of boolean values, which independently switch on or off.
data TimetableBreederFecundity = MkTimetableBreederFecundity {
	getDeterministicConstructorFecundity	:: Fecundity,	-- ^ The /fecundity/ to use when breeding a /timetable/ for subsequent deterministic reconstruction.
	getRandomConstructorFecundity		:: Fecundity	-- ^ The /fecundity/ to use when breeding a /timetable/ for subsequent random reconstruction.
} deriving Eq

instance Read TimetableBreederFecundity where
	readsPrec _	= map (Control.Arrow.first $ uncurry mkTimetableBreederFecundity) . reads

instance Show TimetableBreederFecundity where
	showsPrec _	= shows . (getDeterministicConstructorFecundity &&& getRandomConstructorFecundity)

instance Data.Default.Default TimetableBreederFecundity where
	def	= zero

instance ToolShed.SelfValidate.SelfValidator TimetableBreederFecundity where
	getErrors timetableBreederFecundity	= ToolShed.SelfValidate.extractErrors [
		(
			any ((< 0) . ($ timetableBreederFecundity)) [getDeterministicConstructorFecundity, getRandomConstructorFecundity],
			"fecundity can't be negative; " ++ show timetableBreederFecundity
		) -- Pair.
	 ]

instance HXT.XmlPickler TimetableBreederFecundity where
	xpickle	= HXT.xpDefault defaultTimetableBreederFecundity . HXT.xpElem tag . HXT.xpWrap (
		uncurry mkTimetableBreederFecundity,					-- Construct from a Pair.
		getDeterministicConstructorFecundity &&& getRandomConstructorFecundity	-- Deconstruct to a Pair.
	 ) $ (
		getDeterministicConstructorFecundity defaultTimetableBreederFecundity `HXT.xpDefault` HXT.xpAttr deterministicConstructorFecundityTag HXT.xpInt
	 ) `HXT.xpPair` (
		getRandomConstructorFecundity defaultTimetableBreederFecundity `HXT.xpDefault` HXT.xpAttr randomConstructorFecundityTag HXT.xpInt
	 ) where
		defaultTimetableBreederFecundity	= Data.Default.def

instance Control.DeepSeq.NFData TimetableBreederFecundity where
	rnf	= Control.DeepSeq.rnf . (getDeterministicConstructorFecundity &&& getRandomConstructorFecundity)

-- | Smart constructor.
mkTimetableBreederFecundity :: Fecundity -> Fecundity -> TimetableBreederFecundity
mkTimetableBreederFecundity deterministicConstructorFecundity randomConstructorFecundity
	| ToolShed.SelfValidate.isValid fecundity	= fecundity
	| otherwise					= error $ "WeekDaze.ExecutionConfiguration.TimetableBreederFecundity.mkTimetableBreederFecundity:\t" ++ ToolShed.SelfValidate.getFirstError fecundity ++ "."
	where
		fecundity	= MkTimetableBreederFecundity deterministicConstructorFecundity randomConstructorFecundity

-- | Constant.
zero :: TimetableBreederFecundity
zero	= mkTimetableBreederFecundity 0 0

{- |
	* Combine two 'TimetableBreederFecundity's, returning the geometric mean of the individual 'Fecundity's.

	* The operation is commutative; <https://en.wikipedia.org/wiki/Commutative_property>.
-}
(>*<) :: TimetableBreederFecundity -> TimetableBreederFecundity -> TimetableBreederFecundity
MkTimetableBreederFecundity dl rl >*< MkTimetableBreederFecundity dr rr	= mkTimetableBreederFecundity (round $ getGeometricMean dl dr) (round $ getGeometricMean rl rr) where
	getGeometricMean :: Fecundity -> Fecundity -> Double
	getGeometricMean x y	= sqrt . fromIntegral $ x * y

