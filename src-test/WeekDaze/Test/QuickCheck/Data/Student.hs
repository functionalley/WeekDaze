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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Data.Student.Student'.
-}

module WeekDaze.Test.QuickCheck.Data.Student(
-- * Types
-- ** Type-synonyms
	TeachingRatio,
--	Profile,
-- * Constants
	results
) where

import			Data.Set((\\))
import qualified	Data.Set
import qualified	Test.QuickCheck
import qualified	WeekDaze.Data.HumanResource	as Data.HumanResource
import qualified	WeekDaze.Data.Resource		as Data.Resource
import qualified	WeekDaze.Data.Student		as Data.Student
import qualified	WeekDaze.Identifiers.Level	as Identifiers.Level
import qualified	WeekDaze.Identifiers.Stream	as Identifiers.Stream
import			WeekDaze.Test.QuickCheck.Data.Subject()
import			WeekDaze.Test.QuickCheck.Identifiers.Stream()
import			WeekDaze.Test.QuickCheck.Temporal.Availability()
import			WeekDaze.Test.QuickCheck.Temporal.FreePeriodPreference()
import qualified	WeekDaze.Test.QuickCheck.Temporal.Time	as Test.Temporal.Time

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance (
	Fractional			teachingRatio,
	Ord				level,
	Show				level,
	Real				teachingRatio,
	Test.QuickCheck.Arbitrary	level,
	Test.QuickCheck.Arbitrary	stream
 ) => Test.QuickCheck.Arbitrary (Data.Student.Profile level stream teachingRatio) where
	arbitrary	= Data.Student.mkProfile <$> Test.QuickCheck.arbitrary {-Maybe level-} <*> (
		do
			knowledgeRequirement		<- Test.QuickCheck.arbitrary
			coreKnowledgeRequirements	<- Data.Set.fromList . (knowledgeRequirement :) <$> Test.QuickCheck.arbitrary	-- Non-empty Set.

			(,) coreKnowledgeRequirements . (\\ coreKnowledgeRequirements) . Data.Set.fromList <$> Test.QuickCheck.arbitrary
	 ) <*> Test.QuickCheck.arbitrary {-working week-} <*> (
		(/ fromIntegral Test.Temporal.Time.nTimeslotsPerWeek) . fromIntegral <$> Test.QuickCheck.elements [1 .. Test.Temporal.Time.nTimeslotsPerWeek]	-- TeachingRatio.
	 ) <*> (
		Data.Set.fromList . map (('g' :) . show . (`mod` (10 :: Int))) <$> Test.QuickCheck.arbitrary	-- Group-membership.
	 ) <*> Test.QuickCheck.arbitrary {-maybe free-timeslot preference-}

-- | Defines a concrete type for testing.
-- type TeachingRatio	= Rational	-- Arbitrarily.
type TeachingRatio	= Double	-- Arbitrarily.

-- | Defines a concrete type for testing.
type Profile	= Data.Student.Profile Identifiers.Level.Level Identifiers.Stream.Stream TeachingRatio

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 1024}
 ) [prop_nTimeSlotsPerWeek] where
	prop_nTimeSlotsPerWeek :: Profile -> Test.QuickCheck.Property
	prop_nTimeSlotsPerWeek profile	= Test.QuickCheck.label "prop_nTimeSlotsPerWeek" $ Data.HumanResource.getNTimeslotsPerWeekOfTeaching Test.Temporal.Time.nTimeslotsPerDay profile + Data.HumanResource.getNTimeslotsPerWeekOfNonTeaching Test.Temporal.Time.nTimeslotsPerDay profile == Test.Temporal.Time.nTimeslotsPerDay * Data.Resource.countDaysPerWeekAvailable profile

