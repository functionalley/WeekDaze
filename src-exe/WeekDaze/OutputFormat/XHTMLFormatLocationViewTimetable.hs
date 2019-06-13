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

	* Formats a LocationViewTimetable in XHTML, according to the specified format.
-}

module WeekDaze.OutputFormat.XHTMLFormatLocationViewTimetable(
-- * Constants
	locationViewTimetableTag,
-- * Functions
	composeLocationViewTimetable
) where

import qualified	Data.Array.IArray
import qualified	Text.XHtml.Strict
import qualified	WeekDaze.Dynamic.LocationViewTimetableUtilities		as Dynamic.LocationViewTimetableUtilities
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.LocationView.Timetable				as LocationView.Timetable
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.Model.TimetableForWeek				as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.Text.CSS					as Text.CSS
import qualified	WeekDaze.Text.XHTML					as Text.XHTML
import			Text.XHtml.Strict((<<), (+++), (!))

-- | A CSS-identifier & base file-name.
locationViewTimetableTag :: String
locationViewTimetableTag	= "locationViewTimetable"

-- | Compose XHTML representing a LocationViewTimetable.
composeLocationViewTimetable :: (
	Data.Array.IArray.Ix	timeslotId,
	Enum			timeslotId,
	Fractional		minimumContrastRatio,
	Ord			level,
	Ord			locationId,
	Ord			minimumContrastRatio,
	Ord			teacherId,
	Show			level,
	Show			locationId,
	Show			teacherId,
	Show			timeslotId,
	Text.XHtml.Strict.HTML	campus,
	Text.XHtml.Strict.HTML	level,
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	synchronisationId,
	Text.XHtml.Strict.HTML	teacherId,
	Text.XHtml.Strict.HTML	timeslotId
 )
	=> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Model.Timetable.AugmentMarkup
	-> Model.TimetableForWeek.GenericTimetableToMarkup minimumContrastRatio (StudentView.Timetable.Timetable timeslotId locationId teacherId level)
composeLocationViewTimetable executionOptions problemParameters problemAnalysis displaySupplementaryInformation mergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom studentViewTimetable	= let
	displayState	= False
 in Text.XHTML.mkShowHideButton displayState locationViewTimetableTag +++ Text.XHtml.Strict.h2 ! [
	Text.XHtml.Strict.title "From the perspective of each location."
 ] << "By Location-id" +++ Text.XHtml.Strict.thediv ! (
	if displayState
		then id
		else (Text.XHtml.Strict.thestyle "display: none" :)
 ) [
	Text.XHtml.Strict.identifier locationViewTimetableTag
 ] << (
	(
		if displaySupplementaryInformation
			then Text.XHtml.Strict.thediv ! [Text.XHtml.Strict.theclass Text.CSS.boxCSSIdentifier] << (
				Text.XHtml.Strict.h3 << "Summary" +++ Text.XHtml.Strict.defList [
					(
						Text.XHtml.Strict.thespan ! [
							Text.XHtml.Strict.theclass Text.CSS.infoCSSIdentifier,
							Text.XHtml.Strict.title "The sum over all locations, of available but unallocated time-slots."
						] << "Unallocated time-slots:",
						Text.XHtml.Strict.thespan ! [
							Text.XHtml.Strict.theclass Text.CSS.numericDataCSSIdentifier
						] << Model.Timetable.countUnallocatedAvailableTimeslots locationCatalogue locationViewTimetable
					) -- Pair.
				] ! [
					Text.XHtml.Strict.theclass Text.CSS.timetableViewCSSIdentifier
				]
			)
			else Text.XHtml.Strict.noHtml
	) +++ LocationView.Timetable.toXHtml (
		ProblemConfiguration.ProblemAnalysis.findCourseForLocationViewLesson problemParameters	-- Partially apply.
	) locationCatalogue displaySupplementaryInformation (
		ProblemConfiguration.ProblemAnalysis.getMeetingsByTime problemAnalysis
	) mergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom locationViewTimetable +++ Text.XHtml.Strict.br ! [
		Text.XHtml.Strict.theclass Model.TimetableForWeek.observerViewTerminatorCSSIdentifier
	]
 ) where
	locationCatalogue	= ProblemConfiguration.ProblemParameters.getLocationCatalogue problemParameters
	locationViewTimetable	= Dynamic.LocationViewTimetableUtilities.fromStudentViewTimetable executionOptions problemAnalysis studentViewTimetable

