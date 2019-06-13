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

	* Formats a StudentViewTimetable in XHTML, according to the specified format.
-}

module WeekDaze.OutputFormat.XHTMLFormatStudentViewTimetable(
-- * Constants
	studentViewTimetableTag,
-- * Functions
	composeStudentViewTimetable
) where

import			Control.Arrow((&&&))
import qualified	Control.Arrow
import qualified	Data.Array.IArray
import qualified	Data.Map
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import qualified	ToolShed.Data.Pair
import qualified	WeekDaze.Aggregate.StudentBodyRegister			as Aggregate.StudentBodyRegister
import qualified	WeekDaze.Data.Requirements				as Data.Requirements
import qualified	WeekDaze.Dynamic.StudentViewTimetableUtilities		as Dynamic.StudentViewTimetableUtilities
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.Model.TimetableForWeek				as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.Text.CSS					as Text.CSS
import qualified	WeekDaze.Text.XHTML					as Text.XHTML
import			Text.XHtml.Strict((<<), (+++), (!))

-- | A CSS-identifier & base file-name.
studentViewTimetableTag :: String
studentViewTimetableTag	= "studentViewTimetable"

-- | Compose XHTML representing the specified StudentViewTimetable.
composeStudentViewTimetable :: (
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
	Show			stream,
	Show			teacherId,
	Text.XHtml.Strict.HTML	level,
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	stream,
	Text.XHtml.Strict.HTML	synchronisationId,
	Text.XHtml.Strict.HTML	teacherId,
	Text.XHtml.Strict.HTML	timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Model.Timetable.AugmentMarkup
	-> Model.TimetableForWeek.GenericTimetableToMarkup minimumContrastRatio (StudentView.Timetable.Timetable timeslotId locationId teacherId level)
composeStudentViewTimetable problemParameters problemAnalysis displaySupplementaryInformation mergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom studentViewTimetable	= let
	displayState	= True
 in Text.XHTML.mkShowHideButton displayState studentViewTimetableTag +++ Text.XHtml.Strict.h2 ! [
	Text.XHtml.Strict.title "From the perspective of each student-body."
 ] << "By Student-body" +++ Text.XHtml.Strict.thediv ! (
	if displayState
		then id
		else (Text.XHtml.Strict.thestyle "display: none" :)
 ) [
	Text.XHtml.Strict.identifier studentViewTimetableTag
 ] << (
	(
		if displaySupplementaryInformation
			then Text.XHtml.Strict.thediv ! [
				Text.XHtml.Strict.theclass Text.CSS.boxCSSIdentifier
			] << (
				Text.XHtml.Strict.h3 << "Summary" +++ (
					Text.XHtml.Strict.thediv ! [
						Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier
					] << Text.XHtml.Strict.defList (
						map (
							Control.Arrow.second (
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
								] <<
							)
						) [
							(
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.title "The sum over all student-bodies, of the number of core knowledge-requirements which are incompletely booked, multiplied by the number of members in the student-body."
								] << "Incompletely booked core knowledge-requirements:",
								Aggregate.StudentBodyRegister.countSubjectsRequired incompletelyBookedCoreKnowledgeRequirementsByStudentBody
							), (
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.title "The sum over all student-bodies, of the number of optional knowledge-requirements which are incompletely booked, multiplied by the number of members in the student-body."
								] << "Incompletely booked optional knowledge-requirements:",
								Aggregate.StudentBodyRegister.countSubjectsRequired incompletelyBookedOptionalKnowledgeRequirementsByStudentBody
							), (
								Text.XHtml.Strict.thespan ! [
									Text.XHtml.Strict.title "The sum over all student-bodies, of available but unallocated time-slots, excluding those required for free study & meetings."
								] << "Total free-periods:",
								Dynamic.StudentViewTimetableUtilities.countFreeLessons problemParameters problemAnalysis studentViewTimetable
							) -- Pair.
						]
					) ! [
						Text.XHtml.Strict.theclass Text.CSS.timetableViewCSSIdentifier
					]
				)
			)
			else Text.XHtml.Strict.noHtml
	) +++ StudentView.Timetable.toXHtml (
		Dynamic.StudentViewTimetableUtilities.countInterCampusMigrationsByStudentBody problemParameters studentViewTimetable
	) incompletelyBookedKnowledgeRequirementsByStudentBody (
		Dynamic.StudentViewTimetableUtilities.findExcessiveLessonRunlengthsByTimeByStudentBody problemParameters studentViewTimetable
	) (
		Dynamic.StudentViewTimetableUtilities.findShortLessonRunlengthsByTimeByStudentBody problemParameters studentViewTimetable
	) (
		ProblemConfiguration.ProblemAnalysis.getNTimeslotsPerDay problemAnalysis
	) (
		ProblemConfiguration.ProblemAnalysis.findCourseFor problemParameters	-- Partially apply.
	) (
		ProblemConfiguration.ProblemParameters.getStudentBodyRegister problemParameters
	) displaySupplementaryInformation (
		ProblemConfiguration.ProblemAnalysis.getMeetingsByTime problemAnalysis
	) mergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom studentViewTimetable +++ Text.XHtml.Strict.br ! [
		Text.XHtml.Strict.theclass Model.TimetableForWeek.observerViewTerminatorCSSIdentifier
	]
 ) where
	incompletelyBookedKnowledgeRequirementsByStudentBody@(incompletelyBookedCoreKnowledgeRequirementsByStudentBody, incompletelyBookedOptionalKnowledgeRequirementsByStudentBody)	= ToolShed.Data.Pair.mirror (
		Data.Map.filter (not . Data.Set.null)
	 ) . (
		Data.Map.map Data.Requirements.getCore &&& Data.Map.map Data.Requirements.getOptional
	 ) $ Dynamic.StudentViewTimetableUtilities.findIncompletelyBookedKnowledgeRequirementsByStudentBody problemParameters studentViewTimetable

