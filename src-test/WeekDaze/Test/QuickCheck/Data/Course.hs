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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Data.Course.Course'.
-}

module WeekDaze.Test.QuickCheck.Data.Course(
-- * Types
-- ** Type-synonyms
--	Course,
-- * Constants
	results
) where

import			Control.Arrow((&&&))
import qualified	Data.Maybe
import qualified	Test.QuickCheck
import qualified	WeekDaze.Data.Course			as Data.Course
import qualified	WeekDaze.Data.Location			as Data.Location
import qualified	WeekDaze.Identifiers.Level		as Identifiers.Level
import qualified	WeekDaze.Identifiers.SynchronisationId	as Identifiers.SynchronisationId
import qualified	WeekDaze.Identifiers.TimeslotId		as Identifiers.TimeslotId
import qualified	WeekDaze.Temporal.Day			as Temporal.Day
import qualified	WeekDaze.Temporal.TimeslotRequest	as Temporal.TimeslotRequest
import qualified	WeekDaze.Test.QuickCheck.Data.Location	as Test.Data.Location
import qualified	WeekDaze.Test.QuickCheck.Temporal.Time	as Test.Temporal.Time
import			WeekDaze.Test.QuickCheck.Data.Subject()
import			WeekDaze.Test.QuickCheck.Identifiers.SynchronisationId()
import			WeekDaze.Test.QuickCheck.Temporal.TimeslotRequest()

instance (
	Ord				timeslotId,
	Show				level,
	Show				synchronisationId,
	Show				timeslotId,
	Test.QuickCheck.Arbitrary	level,
	Test.QuickCheck.Arbitrary	synchronisationId,
	Test.QuickCheck.Arbitrary	timeslotId
 ) => Test.QuickCheck.Arbitrary (Data.Course.Course synchronisationId level timeslotId) where
	arbitrary	= do
		subject				<- Test.QuickCheck.arbitrary
		requiredLessonsPerWeek		<- Test.QuickCheck.elements [1 .. Temporal.Day.nDaysPerWeek]
		requiredFacilityNames		<- fmap Data.Location.getFacilityNames (Test.QuickCheck.arbitrary {-location-} :: Test.QuickCheck.Gen Test.Data.Location.Profile)
		timeslotRequest			<- Test.QuickCheck.arbitrary
		minimumConsecutiveLessons	<- (minimum . (: [requiredLessonsPerWeek, Test.Temporal.Time.nTimeslotsPerDay])) `fmap` Test.QuickCheck.elements [1 .. 3] {-arbitrarily-}
		maybeMaximumClassSize		<- Test.QuickCheck.oneof [
			return {-to Gen-monad-} Nothing,
			Just `fmap` Test.QuickCheck.elements [1 .. 4] {-arbitrarily-}
		 ]
		maybeSynchronisationId		<- Test.QuickCheck.arbitrary

		return {-to Gen-monad-} $ Data.Course.mkCourse subject requiredLessonsPerWeek requiredFacilityNames timeslotRequest minimumConsecutiveLessons maybeMaximumClassSize maybeSynchronisationId

-- | Defines a concrete type for testing.
type Course	= Data.Course.Course Identifiers.SynchronisationId.SynchronisationId Identifiers.Level.Level Identifiers.TimeslotId.TimeslotId

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= mapM (
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256}
 ) [prop_isSuitable, prop_isRigid, prop_isFluid, prop_calculateIdealConsecutiveLessons] where
	prop_isSuitable, prop_isRigid, prop_isFluid, prop_calculateIdealConsecutiveLessons :: Course -> Test.QuickCheck.Property
	prop_isSuitable course	= Test.QuickCheck.label "prop_isSuitable" $ Data.Course.isSuitable (Data.Maybe.fromMaybe maxBound $ Data.Course.getMaybeMaximumClassSize course) (Data.Course.getSubject course) course

	prop_isRigid course	= Test.QuickCheck.label "prop_isRigid" . not $ Data.Course.isRigid course && Temporal.TimeslotRequest.isIdeally (Data.Course.getTimeslotRequest course)

	prop_isFluid course	= Test.QuickCheck.label "prop_isFluid" $ not (Temporal.TimeslotRequest.isIdeally $ Data.Course.getTimeslotRequest course) || Data.Course.isFluid course

	prop_calculateIdealConsecutiveLessons course	= Test.QuickCheck.label "prop_calculateIdealConsecutiveLessons" $ if idealConsecutiveLessons == fromIntegral minimumConsecutiveLessons
		then Data.Course.getRequiredLessonsPerWeek course `rem` minimumConsecutiveLessons == 0
		else idealConsecutiveLessons > toRational minimumConsecutiveLessons
		where
			(idealConsecutiveLessons, minimumConsecutiveLessons)		= Data.Course.calculateIdealConsecutiveLessons &&& Data.Course.getMinimumConsecutiveLessons $ course

