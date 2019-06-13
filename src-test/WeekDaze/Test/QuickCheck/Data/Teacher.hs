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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Data.Teacher.Teacher'.
-}

module WeekDaze.Test.QuickCheck.Data.Teacher(
-- * Types
-- ** Type-synonyms
	TeachingRatio,
--	Profile,
-- * Constants
	results
) where

import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Test.QuickCheck
import qualified	ToolShed.Data.List
import qualified	WeekDaze.Data.Course			as Data.Course
import qualified	WeekDaze.Data.HumanResource		as Data.HumanResource
import qualified	WeekDaze.Data.Resource			as Data.Resource
import qualified	WeekDaze.Data.Subject			as Data.Subject
import qualified	WeekDaze.Data.Teacher			as Data.Teacher
import qualified	WeekDaze.Identifiers.Level		as Identifiers.Level
import qualified	WeekDaze.Identifiers.LocationId		as Identifiers.LocationId
import qualified	WeekDaze.Identifiers.SynchronisationId	as Identifiers.SynchronisationId
import qualified	WeekDaze.Identifiers.TimeslotId		as Identifiers.TimeslotId
import qualified	WeekDaze.Temporal.TimeslotRequest	as Temporal.TimeslotRequest
import qualified	WeekDaze.Test.QuickCheck.Temporal.Time	as Test.Temporal.Time
import			WeekDaze.Test.QuickCheck.Data.Course()
import			WeekDaze.Test.QuickCheck.Identifiers.LocationId()
import			WeekDaze.Test.QuickCheck.Temporal.FreePeriodPreference()

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance (
	Fractional			teachingRatio,
	Ord				level,
	Ord				synchronisationId,
	Ord				timeslotId,
	Real				teachingRatio,
	Show				level,
	Show				locationId,
	Show				synchronisationId,
	Show				timeslotId,
	Test.QuickCheck.Arbitrary	level,
	Test.QuickCheck.Arbitrary	locationId,
	Test.QuickCheck.Arbitrary	synchronisationId,
	Test.QuickCheck.Arbitrary	timeslotId
 ) => Test.QuickCheck.Arbitrary (Data.Teacher.Profile synchronisationId level timeslotId locationId teachingRatio) where
	arbitrary	= do
		course	<- Test.QuickCheck.arbitrary

		Data.Teacher.mkProfile <$> (
			Data.Set.fromList . Data.List.nubBy (
				\l r -> let
					l'	= Data.Course.getTimeslotRequest l
					r'	= Data.Course.getTimeslotRequest r
				in ToolShed.Data.List.equalityBy Data.Course.getSubject l r || (
					Temporal.TimeslotRequest.isSpecific l' && (l' == r')	-- Avoid a clash in the requested times for course in different subjects.
				)
			) . (course :) <$> Test.QuickCheck.arbitrary	-- Service.
		 ) <*> Test.QuickCheck.arbitrary {-getMaybeOwnLocationId-} <*> Test.QuickCheck.arbitrary {-working week-} <*> (
			(/ fromIntegral Test.Temporal.Time.nTimeslotsPerWeek) . fromIntegral <$> Test.QuickCheck.elements [1 .. Test.Temporal.Time.nTimeslotsPerWeek]	-- TeachingRatio.
		 ) <*> (
			Data.Set.fromList . map (('g' :) . show . (`mod` (10 :: Int))) <$> Test.QuickCheck.arbitrary	-- Group-membership.
		 ) <*> Test.QuickCheck.elements [
			Nothing,
			Just . Data.Subject.getTopic $ Data.Course.getSubject course
		 ] {-maybe specialtyTopic-} <*> Test.QuickCheck.arbitrary {-maybe free-timeslot preference-}

-- | Defines a concrete type for testing.
-- type TeachingRatio	= Rational	-- Arbitrarily.
type TeachingRatio	= Double	-- Arbitrarily.

-- | Defines a concrete type for testing.
type Profile	= Data.Teacher.Profile Identifiers.SynchronisationId.SynchronisationId Identifiers.Level.Level Identifiers.TimeslotId.TimeslotId Identifiers.LocationId.LocationId TeachingRatio

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_offersSuitableCourse,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_isSpecialistIn,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_inhabits,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 1024} prop_nTimeSlotsPerWeek
 ] where
	prop_isSpecialistIn, prop_offersSuitableCourse, prop_inhabits, prop_nTimeSlotsPerWeek :: Profile -> Test.QuickCheck.Property
	prop_isSpecialistIn profile		= Test.QuickCheck.label "prop_isSpecialistIn" . Data.Maybe.maybe True (`Data.Teacher.isSpecialistIn` profile) $ Data.Teacher.getMaybeSpecialtyTopic profile
	prop_offersSuitableCourse profile	= Test.QuickCheck.label "prop_offersSuitableCourse" . Data.Foldable.all (\subject -> Data.Teacher.offersSuitableCourse 0 subject profile) . Data.Set.map Data.Course.getSubject $ Data.Teacher.getService profile
	prop_inhabits profile			= Test.QuickCheck.label "prop_inhabits" . Data.Maybe.maybe True (`Data.Teacher.inhabits` profile) $ Data.Teacher.getMaybeOwnLocationId profile
	prop_nTimeSlotsPerWeek profile		= Test.QuickCheck.label "prop_nTimeSlotsPerWeek" $ Data.HumanResource.getNTimeslotsPerWeekOfTeaching Test.Temporal.Time.nTimeslotsPerDay profile + Data.HumanResource.getNTimeslotsPerWeekOfNonTeaching Test.Temporal.Time.nTimeslotsPerDay profile == Test.Temporal.Time.nTimeslotsPerDay * Data.Resource.countDaysPerWeekAvailable profile

