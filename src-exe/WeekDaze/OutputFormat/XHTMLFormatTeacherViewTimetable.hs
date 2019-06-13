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

	* Formats a TeacherViewTimetable in XHTML, according to the specified format.
-}

module WeekDaze.OutputFormat.XHTMLFormatTeacherViewTimetable(
-- * Constants
	teacherViewTimetableTag,
-- * Functions
	composeTeacherViewTimetable
) where

import			Control.Arrow((&&&), (***))
import qualified	Data.Array.IArray
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Ord
import qualified	Text.Printf
import qualified	Text.XHtml.Strict
import qualified	WeekDaze.Data.Teacher					as Data.Teacher
import qualified	WeekDaze.Dynamic.TeacherViewTimetableUtilities		as Dynamic.TeacherViewTimetableUtilities
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.Model.TimetableForWeek				as Model.TimetableForWeek
import qualified	WeekDaze.OutputConfiguration.Options			as OutputConfiguration.Options
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.TeacherView.Timetable				as TeacherView.Timetable
import qualified	WeekDaze.Text.CSS					as Text.CSS
import qualified	WeekDaze.Text.XHTML					as Text.XHTML
import			Text.XHtml.Strict((<<), (+++), (!))

-- | A CSS-identifier & base file-name.
teacherViewTimetableTag :: String
teacherViewTimetableTag	= "teacherViewTimetable"

-- | Compose XHTML representing a TeacherViewTimetable.
composeTeacherViewTimetable :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Eq			campus,
	Fractional		minimumContrastRatio,
	Ord			level,
	Ord			locationId,
	Ord			minimumContrastRatio,
	Ord			teacherId,
	RealFrac		teachingRatio,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId,
	Text.XHtml.Strict.HTML	level,
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	synchronisationId,
	Text.XHtml.Strict.HTML	teacherId,
	Text.XHtml.Strict.HTML	timeslotId
 )
	=> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> OutputConfiguration.Options.Options minimumContrastRatio
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Model.Timetable.AugmentMarkup
	-> Model.TimetableForWeek.GenericTimetableToMarkup minimumContrastRatio (StudentView.Timetable.Timetable timeslotId locationId teacherId level)
composeTeacherViewTimetable executionOptions outputOptions problemParameters problemAnalysis displaySupplementaryInformation mergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom studentViewTimetable	= let
	displayState	= False
 in Text.XHTML.mkShowHideButton displayState teacherViewTimetableTag +++ Text.XHtml.Strict.h2 ! [
	Text.XHtml.Strict.title "From the perspective of each teacher."
 ] << "By Teacher-id" +++ Text.XHtml.Strict.thediv ! (
	if displayState
		then id
		else (Text.XHtml.Strict.thestyle "display: none" :)
 ) [
	Text.XHtml.Strict.identifier teacherViewTimetableTag
 ] << (
	(
		if displaySupplementaryInformation
			then Text.XHtml.Strict.thediv ! [Text.XHtml.Strict.theclass Text.CSS.boxCSSIdentifier] << (
				Text.XHtml.Strict.h3 << "Summary" +++ Text.XHtml.Strict.defList [
					pair | (True, pair) <- [
						(
							True,
							(
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
									Text.XHtml.Strict.title "The sum over all teachers, of available but unallocated time-slots, excluding those required for administration & meetings."
								] << "Total free-periods:",
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
								] << Dynamic.TeacherViewTimetableUtilities.countFreeLessons problemParameters problemAnalysis teacherViewTimetable
							)
						), (
							True,
							(
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier,
									Text.XHtml.Strict.title "The mean over all bookings, of the size of student-classes."
								] << "Mean student-class size:",
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
								] << Text.XHtml.Strict.stringToHtml (
									Text.Printf.printf "%.*f" (
										OutputConfiguration.Options.getNDecimalDigits outputOptions
									) (
										TeacherView.Timetable.calculateMeanStudentClassSize teacherViewTimetable :: Double
									)
								)
							)
						), let
							utilisationRatioExtremesByTeacherId	= (
								head {-least-} &&& last {-most-}
							 ) . Data.List.sortBy (
								Data.Ord.comparing snd {-utilisation-ratio-}
							 ) . filter (
								Data.Teacher.offersService . (teacherRegister Data.Map.!) . fst {-teacherId-}	-- Remove pure administrators, whose utilisation-ratio is undefined.
							 ) . Data.Map.assocs $ Dynamic.TeacherViewTimetableUtilities.calculateUtilisationRatioByTeacherId problemParameters problemAnalysis teacherViewTimetable
						in (
							Data.Map.size teacherRegister > 1 && uncurry (/=) (
								snd {-utilisation-ratio-} *** snd {-utilisation-ratio-} $ utilisationRatioExtremesByTeacherId
							), (
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier,
									Text.XHtml.Strict.title "Time allocated to teaching / time available for teaching."
								] << "(Least,Most) utilised teacher:",
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.theclass Text.CSS.dataCSSIdentifier
								] << show (
									fst {-teacherId-} *** fst {-teacherId-} $ utilisationRatioExtremesByTeacherId
								)
							)
						) -- Pair.
					]
				] ! [
					Text.XHtml.Strict.theclass Text.CSS.timetableViewCSSIdentifier
				]
			)
			else Text.XHtml.Strict.noHtml
	) +++ TeacherView.Timetable.toXHtml (
		Dynamic.TeacherViewTimetableUtilities.countInterCampusMigrationsByTeacherId problemParameters teacherViewTimetable
	) (
		Dynamic.TeacherViewTimetableUtilities.findUnbookedSpecifiedTimesByTeacherId problemParameters teacherViewTimetable
	) (
		ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis
	) (
		ProblemConfiguration.ProblemAnalysis.findCourseForTeacherViewLesson problemParameters	-- Partially apply.
	) teacherRegister displaySupplementaryInformation (
		ProblemConfiguration.ProblemAnalysis.getMeetingsByTime problemAnalysis
	) mergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom teacherViewTimetable +++ Text.XHtml.Strict.br ! [
		Text.XHtml.Strict.theclass Model.TimetableForWeek.observerViewTerminatorCSSIdentifier
	]
 ) where
	teacherRegister		= ProblemConfiguration.ProblemParameters.getTeacherRegister problemParameters
	teacherViewTimetable	= Dynamic.TeacherViewTimetableUtilities.fromStudentViewTimetable executionOptions problemAnalysis studentViewTimetable

