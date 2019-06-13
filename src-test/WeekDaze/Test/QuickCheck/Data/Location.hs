{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Data.Location.Location'.
-}

module WeekDaze.Test.QuickCheck.Data.Location(
-- * Types
-- ** Type-synonyms
	Profile,
-- * Constants
	results
) where

import qualified	Data.Set
import qualified	Test.QuickCheck
import qualified	WeekDaze.Data.Location		as Data.Location
import qualified	WeekDaze.Identifiers.Campus	as Identifiers.Campus
import			WeekDaze.Test.QuickCheck.Identifiers.Campus()
import			WeekDaze.Test.QuickCheck.Temporal.Availability()

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

-- | A concrete type for testing.
type Profile	= Data.Location.Profile Identifiers.Campus.Campus

instance (Show campus, Test.QuickCheck.Arbitrary campus) => Test.QuickCheck.Arbitrary (Data.Location.Profile campus) where
	arbitrary	= Data.Location.mkProfile <$> Test.QuickCheck.elements [1 .. 10 {-arbitrarily-}] {-Capacity-} <*> (
		Data.Set.fromList . return {-to Gen-monad-} . ('f' :) . show <$> Test.QuickCheck.elements [0 :: Int {-arbitrarily-} .. 9 {-arbitrarily-}]	-- FacilityNames.
	 ) <*> Test.QuickCheck.arbitrary {-Availability-} <*> Test.QuickCheck.arbitrary {-Campus-}

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [
	prop_isSuitable,
	prop_isSuitable',
	prop_isUnsuitable,
	prop_isAlwaysSuitable,
	prop_calculateRelativeWaste,
	prop_calculateRelativeUse
 ] where
	prop_isSuitable, prop_isSuitable', prop_isUnsuitable, prop_isAlwaysSuitable, prop_calculateRelativeWaste, prop_calculateRelativeUse :: Profile -> Test.QuickCheck.Property
	prop_isSuitable profile	= Test.QuickCheck.label "prop_isSuitable" $ Data.Location.isSuitable (Data.Location.getCapacity profile) (Data.Location.getFacilityNames profile) profile
	prop_isSuitable'	= Test.QuickCheck.label "prop_isSuitable'" . Data.Location.isSuitable 0 {-nStudents-} Data.Set.empty {-facilityNames-}
	prop_isUnsuitable	= Test.QuickCheck.label "prop_isUnsuitable" . not . Data.Location.isSuitable maxBound {-nStudents-} Data.Set.empty {-facilityNames-}
	prop_isAlwaysSuitable	= Test.QuickCheck.label "prop_isAlwaysSuitable" . Data.Location.isSuitable 0 {-nStudents-} Data.Set.empty {-facilityNames-}

	prop_calculateRelativeWaste profile	= Test.QuickCheck.label "prop_calculateRelativeWaste" $ Data.Location.calculateRelativeWaste profile (
		Data.Location.calculateWaste (Data.Location.getCapacity profile) (Data.Location.getFacilityNames profile) profile
	 ) == (0 :: Rational {-arbitrarily-}, 0)

	prop_calculateRelativeUse profile	= Test.QuickCheck.label "prop_calculateRelativeUse" $ Data.Location.calculateRelativeUse profile (
		Data.Location.calculateWaste 0 Data.Set.empty profile
	 ) == (0 :: Rational {-arbitrarily-}, 0)

