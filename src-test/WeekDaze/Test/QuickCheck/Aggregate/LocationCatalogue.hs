{-# LANGUAGE CPP #-}
{-
	Copyright (C) 2013-2016 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Aggregate.LocationCatalogue.LocationCatalogue'.
-}

module WeekDaze.Test.QuickCheck.Aggregate.LocationCatalogue(
-- * Constants
	results
) where

import qualified	Data.Map
import qualified	Data.Foldable
import qualified	Test.QuickCheck
import qualified	WeekDaze.Aggregate.LocationCatalogue	as Aggregate.LocationCatalogue
import qualified	WeekDaze.Data.Location			as Data.Location
import qualified	WeekDaze.Identifiers.Campus		as Identifiers.Campus
import qualified	WeekDaze.Identifiers.LocationId		as Identifiers.LocationId
import qualified	WeekDaze.Size				as Size
import			WeekDaze.Test.QuickCheck.Identifiers.LocationId()
import			WeekDaze.Test.QuickCheck.Data.Location()

#if !MIN_VERSION_QuickCheck(2,8,2)
import			ToolShed.Test.QuickCheck.Arbitrary.Map()
#endif

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [prop_findSuitableLocations, prop_countDaysByFacilityName, prop_extractDistinctFacilityNames] where
	prop_findSuitableLocations, prop_countDaysByFacilityName, prop_extractDistinctFacilityNames :: Aggregate.LocationCatalogue.LocationCatalogue Identifiers.LocationId.LocationId Identifiers.Campus.Campus -> Test.QuickCheck.Property
	prop_findSuitableLocations locationCatalogue	= Test.QuickCheck.label "prop_findSuitableLocations" $ Data.Foldable.all (
		\profile -> not . Data.Map.null $ Aggregate.LocationCatalogue.findSuitableLocations (
			Data.Location.getCapacity profile
		) (
			Data.Location.getFacilityNames profile
		) locationCatalogue
	 ) locationCatalogue

	prop_countDaysByFacilityName	= Test.QuickCheck.label "prop_countDaysByFacilityName" . Data.Foldable.all (>= (1 :: Size.NDays)) . Aggregate.LocationCatalogue.countDaysByFacilityName

	prop_extractDistinctFacilityNames locationCatalogue	= Test.QuickCheck.label "prop_extractDistinctFacilityNames" $ Aggregate.LocationCatalogue.extractDistinctFacilityNames locationCatalogue == Data.Map.keysSet (Aggregate.LocationCatalogue.countDaysByFacilityName locationCatalogue)

