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

 [@DESCRIPTION@]	Defines /QuickCheck/-properties for 'StudentView.Timetable.Timetable'.
-}

module WeekDaze.Test.QuickCheck.StudentView.Timetable(
-- * Types
-- ** Type-synonyms
--	Timetable,
-- * Constants
	results,
-- * Functions
--	removeIncompatibleLessons,
--	reindexStudentBodyRegister
) where

import			Control.Arrow((***), (&&&))
import			Data.Map((!))
import			Data.Set((\\))
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Data.Tuple
import qualified	Test.QuickCheck
import			Test.QuickCheck((==>))
import qualified	ToolShed.Data.List
import qualified	ToolShed.Data.List.Runlength
import			ToolShed.Test.QuickCheck.Arbitrary.Array()
import qualified	WeekDaze.Aggregate.StudentBody				as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentBodyRegister			as Aggregate.StudentBodyRegister
import qualified	WeekDaze.Aggregate.StudentClass				as Aggregate.StudentClass
import qualified	WeekDaze.Data.Resource					as Data.Resource
import qualified	WeekDaze.Identifiers.Level				as Identifiers.Level
import qualified	WeekDaze.Identifiers.LocationId				as Identifiers.LocationId
import qualified	WeekDaze.Identifiers.TeacherId				as Identifiers.TeacherId
import qualified	WeekDaze.Identifiers.TimeslotId				as Identifiers.TimeslotId
import qualified	WeekDaze.LinearModel.Timetable				as LinearModel.Timetable
import qualified	WeekDaze.LocationView.Timetable				as LocationView.Timetable
import qualified	WeekDaze.Model.GeneralisedBooking			as Model.GeneralisedBooking
import qualified	WeekDaze.Model.Lesson					as Model.Lesson
import qualified	WeekDaze.Model.ResourceUser				as Model.ResourceUser
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.Model.TimetableCoordinates			as Model.TimetableCoordinates
import qualified	WeekDaze.Model.TimetableForWeek				as Model.TimetableForWeek
import qualified	WeekDaze.StudentView.LessonResourceIds			as StudentView.LessonResourceIds
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.TeacherView.Timetable				as TeacherView.Timetable
import qualified	WeekDaze.Temporal.Day					as Temporal.Day
import qualified	WeekDaze.Temporal.Time					as Temporal.Time
import qualified	WeekDaze.Test.QuickCheck.Aggregate.StudentBodyRegister	as Test.Aggregate.StudentBodyRegister
import qualified	WeekDaze.Test.QuickCheck.StudentView.Lesson		as Test.StudentView.Lesson
import qualified	WeekDaze.Test.QuickCheck.Temporal.Time			as Test.Temporal.Time

-- | Remove incompatible /lesson/s.
removeIncompatibleLessons :: (
	Data.Array.IArray.Ix	timeslotId,
	Eq			locationId,
	Eq			level,
	Eq			teacherId
 ) => StudentView.Timetable.Timetable timeslotId locationId teacherId level -> StudentView.Timetable.Timetable timeslotId locationId teacherId level
removeIncompatibleLessons studentViewTimetable	= foldr (
	\(coordinates, _) studentViewTimetable'	-> if Model.ResourceUser.areMutuallyMergeable $ Model.Timetable.extractSynchronousLessonsAt (
		Model.TimetableCoordinates.getTime coordinates
	) studentViewTimetable'
		then studentViewTimetable'
		else Model.Timetable.undefineTimeslot coordinates studentViewTimetable'
 ) studentViewTimetable {-initial value-} $ LinearModel.Timetable.fromTimetable studentViewTimetable

-- | Change the keys (/student-bodies/) of an arbitrary /studentBodyRegister/ to match those of the specified /studentViewTimetable/.
reindexStudentBodyRegister
	:: StudentView.Timetable.Timetable timeslotId locationId teacherId level
	-> Aggregate.StudentBodyRegister.StudentBodyRegister level stream teachingRatio
	-> Aggregate.StudentBodyRegister.StudentBodyRegister level stream teachingRatio
reindexStudentBodyRegister studentViewTimetable studentBodyRegister
	| Data.Map.size studentViewTimetable > Data.Map.size studentBodyRegister	= error "WeekDaze.Test.QuickCheck.StudentView.Timetable.reindexStudentBodyRegister:\tStudentBodyRegister too small."
	| otherwise	= Data.Map.fromList $ zipWith (
		\(k, _) (_, v)	-> (k, v)
	) (
		Data.Map.toList studentViewTimetable
	) (
		Data.Map.toList studentBodyRegister
	)

-- | Defines a concrete type for testing.
type Timetable	= StudentView.Timetable.Timetable Identifiers.TimeslotId.TimeslotId Identifiers.LocationId.LocationId Identifiers.TeacherId.TeacherId Identifiers.Level.Level

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results = sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_purge,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_areBookedResourcesAt,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_teacherView,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_locationView,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_breaksRoutine,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_extractStudentClassAt,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_findStudentClassesByLesson,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_weightedMeanStudentBodyCombinationsPerLesson1,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_findStudentClassesByLessonFor,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_countLessons,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_weightedMeanStudentBodyCombinationsPerLesson2,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_extractSynchronousLessonsAt,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_findGeneralisedLessonRunlengthsByCoordinates,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_findSeparatedEqualLessonsWithinAnyDayByObserverId,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_weightedMeanStudentBodyCombinationsPerLesson3,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_calculateMeanStudentClassSize,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_calculateAverageAbsoluteDeviationOfFreeLessonsPerDay,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_calculateAverageAbsoluteDeviationOfFreeLessonsPerDay',
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_extractDistinctLessons
 ] where
	prop_purge, prop_areBookedResourcesAt, prop_teacherView, prop_locationView, prop_breaksRoutine, prop_extractStudentClassAt, prop_findStudentClassesByLesson, prop_weightedMeanStudentBodyCombinationsPerLesson1, prop_findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId :: Timetable -> Test.QuickCheck.Property

	prop_purge studentViewTimetable	= Test.QuickCheck.label "prop_purge" $ Model.Timetable.purge studentViewTimetable == Model.Timetable.undefineTimeslots studentViewTimetable (Model.Timetable.extractCoordinates studentViewTimetable)

	prop_areBookedResourcesAt studentViewTimetable	= Test.QuickCheck.label "prop_areBookedResourcesAt" $ all (
		($ studentViewTimetable) . StudentView.Timetable.areBookedResourcesAt . Control.Arrow.first Model.TimetableCoordinates.getTime
	 ) $ LinearModel.Timetable.fromTimetable studentViewTimetable

	prop_teacherView studentViewTimetable	= Test.QuickCheck.label "prop_teacherView" $ TeacherView.Timetable.toStudentViewTimetable (
		Model.Timetable.mkFreeTimetable (StudentView.Timetable.getStudentBodies validStudentViewTimetable) timeslotIdBounds
	 ) (
		TeacherView.Timetable.fromStudentViewTimetable True (
			Model.Timetable.mkFreeTimetable (
				Data.Set.toList . foldr (
					Data.Set.insert . StudentView.LessonResourceIds.getTeacherId . Model.Lesson.getResourceIds
				) Data.Set.empty $ Model.Timetable.extractLessons validStudentViewTimetable
			) timeslotIdBounds
		) validStudentViewTimetable
	 ) == validStudentViewTimetable where
		timeslotIdBounds		= (minBound, maxBound)
		validStudentViewTimetable	= removeIncompatibleLessons studentViewTimetable

	prop_locationView studentViewTimetable	= Test.QuickCheck.label "prop_locationView" $ LocationView.Timetable.toStudentViewTimetable (
		Model.Timetable.mkFreeTimetable (StudentView.Timetable.getStudentBodies validStudentViewTimetable) timeslotIdBounds
	 ) (
		LocationView.Timetable.fromStudentViewTimetable True (
			Model.Timetable.mkFreeTimetable (
				Data.Set.toList . foldr (
					Data.Set.insert . StudentView.LessonResourceIds.getLocationId . Model.Lesson.getResourceIds
				) Data.Set.empty $ Model.Timetable.extractLessons validStudentViewTimetable
			) timeslotIdBounds
		) validStudentViewTimetable
	 ) == validStudentViewTimetable where
		timeslotIdBounds		= (minBound, maxBound)
		validStudentViewTimetable	= removeIncompatibleLessons studentViewTimetable

	prop_breaksRoutine studentViewTimetable	= Test.QuickCheck.label "prop_breaksRoutine" . not . any (
		uncurry (StudentView.Timetable.breaksRoutine studentViewTimetable)
	 ) . map (
		Control.Arrow.first Model.TimetableCoordinates.getObserverId
	 ) $ LinearModel.Timetable.fromTimetable studentViewTimetable

	prop_extractStudentClassAt studentViewTimetable	= Test.QuickCheck.label "prop_extractStudentClassAt" . all (
		\(time, lesson) -> uncurry (&&) . (
			Data.Foldable.all (
				\studentBody -> Model.Timetable.getMaybeLesson (studentBody, time) studentViewTimetable == Just lesson
			) &&& Data.Foldable.all (
				\studentBody -> Model.Timetable.getMaybeLesson (studentBody, time) studentViewTimetable /= Just lesson
			) . (Data.Map.keysSet studentViewTimetable \\)
		) $ StudentView.Timetable.extractStudentClassAt (time, lesson) studentViewTimetable
	 ) . map (
		Control.Arrow.first Model.TimetableCoordinates.getTime
	 ) $ LinearModel.Timetable.fromTimetable studentViewTimetable

	prop_findStudentClassesByLesson studentViewTimetable	= Test.QuickCheck.label "prop_findStudentClassesByLesson" $ Data.Map.unionsWith Data.Set.union (
		map (StudentView.Timetable.findStudentClassesByLessonFor studentViewTimetable) $ StudentView.Timetable.getStudentBodies studentViewTimetable
	 ) == StudentView.Timetable.findStudentClassesByLesson studentViewTimetable

	prop_weightedMeanStudentBodyCombinationsPerLesson1 studentViewTimetable	= Test.QuickCheck.label "prop_weightedMeanStudentBodyCombinationsPerLesson1" $ StudentView.Timetable.calculateWeightedMeanStudentBodyCombinationsPerLesson studentViewTimetable == (calculateWeightedMeanStudentBodyCombinationsPerLesson' studentViewTimetable :: Rational) where
		calculateWeightedMeanStudentBodyCombinationsPerLesson' timetable
			| denominator == 0	= 1	-- (zero / zero) -> 1, at the limit of only one lesson-definition in the timetable.
			| otherwise		= fromIntegral numerator / fromIntegral denominator
			where
				(numerator, denominator)	= foldr (
					\(nStudentBodyCombinations, nStudents)	-> ((nStudentBodyCombinations * nStudents {-weighting-}) +) *** (nStudents +)
				 ) (0, 0) [
					(Data.Set.size studentBodyCombinations, Aggregate.StudentBody.getSize studentBody) |
						studentBody		<- StudentView.Timetable.getStudentBodies timetable,
						studentBodyCombinations	<- Data.Map.elems $ StudentView.Timetable.findStudentClassesByLessonFor timetable studentBody
				 ] -- List-comprehension. Scan the domain of all student-bodies & all the distinct lessons booked for them

	prop_findStudentClassesByLessonFor, prop_countLessons, prop_weightedMeanStudentBodyCombinationsPerLesson2, prop_extractSynchronousLessonsAt :: Timetable -> [(Test.Temporal.Time.Time, Test.StudentView.Lesson.Lesson)] -> Test.QuickCheck.Property
	prop_findStudentClassesByLessonFor studentViewTimetable linearTimetableForWeek	= Test.QuickCheck.label "prop_findStudentClassesByLessonFor" . (
		\studentViewTimetable'	-> all (
			Data.Foldable.all (== Data.Set.singleton (Data.Set.fromList studentBodies)) . StudentView.Timetable.findStudentClassesByLessonFor studentViewTimetable'
		) studentBodies
	 ) $ foldr Model.Timetable.defineTimeslot (
		Model.Timetable.purge studentViewTimetable	-- Start with a blank canvas.
	 ) [
		((studentBody, time), Just lesson) |
			(time, lesson)	<- take Test.Temporal.Time.nTimeslotsPerWeek linearTimetableForWeek,
			studentBody	<- studentBodies
	 ] {-list-comprehension-} where
		studentBodies	= StudentView.Timetable.getStudentBodies studentViewTimetable

	prop_countLessons studentViewTimetable linearTimetableForWeek	= not (Data.Map.null studentViewTimetable) ==> Test.QuickCheck.label "prop_countLessons" $ Model.Timetable.countLessons (
		foldr Model.Timetable.defineTimeslot (Model.Timetable.purge studentViewTimetable) bookings	-- Start with a blank canvas.
	 ) == length bookings where
		bookings	= Data.List.nubBy (
			ToolShed.Data.List.equalityBy Model.GeneralisedBooking.getCoordinates	-- Ensure no bookings are overwritten.
		 ) $ zipWith (
			\studentBody (time, lesson) -> ((studentBody, time), Just lesson)
		 ) (cycle $ StudentView.Timetable.getStudentBodies studentViewTimetable) linearTimetableForWeek

	prop_weightedMeanStudentBodyCombinationsPerLesson2 studentViewTimetable linearTimetableForWeek	= Test.QuickCheck.label "prop_weightedMeanStudentBodyCombinationsPerLesson2" . (
		== (1 :: Rational)
	 ) . StudentView.Timetable.calculateWeightedMeanStudentBodyCombinationsPerLesson $ foldr Model.Timetable.defineTimeslot (
		Model.Timetable.purge studentViewTimetable	-- Start with a blank canvas.
	 ) [
		((studentBody, time), Just lesson) |
			(time, lesson)	<- take Test.Temporal.Time.nTimeslotsPerWeek linearTimetableForWeek,
			studentBody	<- StudentView.Timetable.getStudentBodies studentViewTimetable
	 ] -- List-comprehension. Define lines, each using a constant lesson, through all student-bodies, at several times.

	prop_extractSynchronousLessonsAt studentViewTimetable linearTimetableForWeek	= Data.Map.size studentViewTimetable > 1 ==> Test.QuickCheck.label "prop_extractSynchronousLessonsAt" . all (
		(== 2) . length . (
			`Model.Timetable.extractSynchronousLessonsAt` (
				foldr Model.Timetable.defineTimeslot (
					Model.Timetable.purge studentViewTimetable	-- Start with a blank canvas.
				) $ concatMap (
					\pair -> map ($ pair) [
						(,) studentBodyF *** Just,
						(,) studentBodyL *** Just
					] -- Construct a booking for the same lesson, for two different student-bodies.
				) linearTimetableForWeek
			)
		) -- Section.
	 ) $ map Model.TimetableForWeek.getBookedTime linearTimetableForWeek where
		(studentBodyF, studentBodyL)	= head &&& last $ StudentView.Timetable.getStudentBodies studentViewTimetable

	prop_weightedMeanStudentBodyCombinationsPerLesson3 :: Test.Temporal.Time.Time -> Test.StudentView.Lesson.Lesson -> Timetable -> Test.QuickCheck.Property
	prop_weightedMeanStudentBodyCombinationsPerLesson3 time lesson studentViewTimetable	= Data.Map.size studentViewTimetable > 1 ==> Test.QuickCheck.label "prop_weightedMeanStudentBodyCombinationsPerLesson3" . (
		== result
	 ) . StudentView.Timetable.calculateWeightedMeanStudentBodyCombinationsPerLesson . foldr Model.Timetable.defineTimeslot (
		Model.Timetable.purge studentViewTimetable	-- Start with a blank canvas.
	 ) . uncurry (++) . (
		(`zip` lessons) . (`zip` repeat time) *** (`zip` lessons) . (`zip` repeat (rotate (Temporal.Time.getDay time) `Temporal.Time.mkTime` Temporal.Time.getTimeslotId time))
	 ) $ (
		init &&& tail	-- Create two classes.
	 ) studentBodies where
		rotate :: (Bounded a, Enum a, Eq a) => a -> a
		rotate x
			| x == maxBound	= minBound
			| otherwise	= succ x

		lessons :: [Maybe Test.StudentView.Lesson.Lesson]
		lessons	= repeat $ Just lesson

		studentBodies :: [Aggregate.StudentBody.StudentBody]
		studentBodies	= StudentView.Timetable.getStudentBodies studentViewTimetable

		result	= (/ fromIntegral (Aggregate.StudentClass.getSize studentBodies)) . toRational $ uncurry (+) (
			Aggregate.StudentBody.getSize . head &&& Aggregate.StudentBody.getSize . last $ studentBodies	-- Two student-bodies are members of only one class.
		 ) + (
			(* 2) . Aggregate.StudentClass.getSize . init $ tail studentBodies	-- All but two student-bodies, are members of two classes, for the single lesson-definition.
		 )

	prop_findGeneralisedLessonRunlengthsByCoordinates :: Timetable -> [Temporal.Day.Day] -> [Test.StudentView.Lesson.Lesson] -> Test.QuickCheck.Property
	prop_findGeneralisedLessonRunlengthsByCoordinates studentViewTimetable days lessons	= Test.QuickCheck.label "prop_findGeneralisedLessonRunlengthsByCoordinates" . all (
		(== Test.Temporal.Time.nTimeslotsPerDay) . ToolShed.Data.List.Runlength.getLength . snd {-runlength-code-}
	 ) $ Model.Timetable.findGeneralisedLessonRunlengthsByCoordinates (
		foldr Model.Timetable.defineTimeslot (
			Model.Timetable.purge studentViewTimetable	-- Start with a blank canvas.
		) [
			((studentBody, Temporal.Time.mkTime day timeslotId), Just lesson) |
				studentBody	<- StudentView.Timetable.getStudentBodies studentViewTimetable,	-- Range over all student-bodies.
				(day, lesson)	<- Data.List.nub days `zip` lessons,				-- Assign one type of lesson to each day.
				timeslotId	<- [minBound .. maxBound]					-- Range over all timeslots.
		] -- List-comprehension. Book surfaces of constant lesson, on a variety of days.
	 )

	prop_findSeparatedEqualLessonsWithinAnyDayByObserverId :: Timetable -> [Temporal.Day.Day] -> [Test.StudentView.Lesson.Lesson] -> Test.QuickCheck.Property
	prop_findSeparatedEqualLessonsWithinAnyDayByObserverId studentViewTimetable days lessons	= not (null lessons) ==> Test.QuickCheck.label "prop_findSeparatedEqualLessonsWithinAnyDayByObserverId" . Data.Foldable.all (
		all ((== 2) . ToolShed.Data.List.Runlength.getLength)
	 ) $ Model.Timetable.findSeparatedEqualLessonsWithinAnyDayByObserverId (
		foldr Model.Timetable.defineTimeslot (
			Model.Timetable.purge studentViewTimetable	-- Start with a blank canvas.
		 ) [
			((studentBody, Temporal.Time.mkTime day timeslotId), Just lesson) |
				studentBody	<- StudentView.Timetable.getStudentBodies studentViewTimetable,	-- Range over all student-bodies.
				(day, lesson)	<- Data.List.nub days `zip` cycle lessons,			-- Assign one type of lesson to each day
				timeslotId	<- [minBound, maxBound]						-- Duplicate the lesson at the beginning & the end of the day.
		 ] -- List-comprehension.
	 )

	prop_findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId studentViewTimetable	= Test.QuickCheck.label "prop_findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId" $ Data.Map.map (
		Data.Foldable.concatMap (
			map (
				Control.Arrow.first length . Data.Tuple.swap	-- Discard the startingTimeslotId & individual runlengths.
			)
		) -- Discard the 'day'.
	 ) (
		Model.Timetable.findSeparatedEqualLessonRunlengthsByStartingTimeslotIdByLessonByDayByObserverId studentViewTimetable
	 ) == Model.Timetable.findSeparatedEqualLessonsWithinAnyDayByObserverId studentViewTimetable

	prop_calculateAverageAbsoluteDeviationOfFreeLessonsPerDay :: Test.StudentView.Lesson.Lesson -> Timetable -> Test.Aggregate.StudentBodyRegister.StudentBodyRegister -> Test.QuickCheck.Property
	prop_calculateAverageAbsoluteDeviationOfFreeLessonsPerDay lesson studentViewTimetable studentBodyRegister	= Data.Map.size studentBodyRegister >= Data.Map.size studentViewTimetable ==> Test.QuickCheck.label "prop_calculateAverageAbsoluteDeviationOfFreeLessonsPerDay" . (== (0 :: Rational)) . Model.Timetable.calculateAverageAbsoluteDeviationOfFreeLessonsPerDay (

		reindexStudentBodyRegister studentViewTimetable studentBodyRegister
	 ) . foldr Model.Timetable.defineTimeslot studentViewTimetable . zip (Model.Timetable.extractCoordinates studentViewTimetable) . map Just $ repeat lesson

	prop_calculateMeanStudentClassSize :: Test.StudentView.Lesson.Lesson -> Timetable -> Test.QuickCheck.Property
	prop_calculateMeanStudentClassSize lesson studentViewTimetable = not (Data.Map.null studentViewTimetable) ==> Test.QuickCheck.label "prop_calculateMeanStudentClassSize" . (
		== toRational (Aggregate.StudentClass.getSize $ StudentView.Timetable.getStudentBodies studentViewTimetable)
	 ) . TeacherView.Timetable.calculateMeanStudentClassSize . TeacherView.Timetable.fromStudentViewTimetable True (
		Model.Timetable.mkFreeTimetable [
			StudentView.LessonResourceIds.getTeacherId $ Model.Lesson.getResourceIds lesson
		] (minBound, maxBound)
	 ) . foldr Model.Timetable.defineTimeslot (Model.Timetable.purge studentViewTimetable) . zip (Model.Timetable.extractCoordinates studentViewTimetable) . map Just $ repeat lesson

	prop_calculateAverageAbsoluteDeviationOfFreeLessonsPerDay' :: Test.StudentView.Lesson.Lesson -> Timetable -> Test.Aggregate.StudentBodyRegister.StudentBodyRegister -> Test.QuickCheck.Property
	prop_calculateAverageAbsoluteDeviationOfFreeLessonsPerDay' lesson studentViewTimetable studentBodyRegister	= Data.Map.size studentBodyRegister >= Data.Map.size studentViewTimetable && not (Data.Map.null studentViewTimetable') ==> Test.QuickCheck.label "prop_calculateAverageAbsoluteDeviationOfFreeLessonsPerDay'" . (
		== (1 :: Rational)
	 ) . Model.Timetable.calculateAverageAbsoluteDeviationOfFreeLessonsPerDay studentBodyRegister' $ Data.Map.mapWithKey (
		\studentBody timetableForWeek	-> foldr Model.TimetableForWeek.defineTimeslot timetableForWeek . (
			`zip` repeat (Just lesson)	-- Construct a booking.
		) . concatMap (
			\l -> let
				(day1, day2)	= head &&& last $ l
			in Temporal.Time.mkTime day1 maxBound : map (Temporal.Time.mkTime day2) (take 3 [minBound ..])
		) . ToolShed.Data.List.chunk 2 $ filter (`Data.Resource.isAvailableOn` (studentBodyRegister' ! studentBody)) Temporal.Day.range
	 ) studentViewTimetable' where
		studentBodyRegister'	= reindexStudentBodyRegister studentViewTimetable studentBodyRegister

		studentViewTimetable'	= Data.Map.filterWithKey (
			\studentBody _ -> even . Data.Resource.countDaysPerWeekAvailable $ studentBodyRegister' ! studentBody
		 ) $ Model.Timetable.purge studentViewTimetable	-- Select student-bodies who're available on an even number of days.

	prop_extractDistinctLessons :: [Test.Temporal.Time.Time] -> [Test.StudentView.Lesson.Lesson] -> Timetable -> Test.QuickCheck.Property
	prop_extractDistinctLessons times lessons studentViewTimetable	= Test.QuickCheck.label "prop_extractDistinctLessons" . (== Data.Set.fromList (Data.Maybe.mapMaybe Model.GeneralisedBooking.getMaybeLesson generalisedBookings)) . Model.Timetable.extractDistinctLessons $ foldr Model.Timetable.defineTimeslot (
		Model.Timetable.purge studentViewTimetable	-- Start with a blank canvas.
	 ) generalisedBookings where
		generalisedBookings	= zip (zip (StudentView.Timetable.getStudentBodies studentViewTimetable) $ Data.List.nub times) . map Just $ Data.List.nub lessons

