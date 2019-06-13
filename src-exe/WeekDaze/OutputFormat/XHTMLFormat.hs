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

	* Composes an XHTML-document, according to the specified format.

	* Optionally depicts the specified /timetable/ from alternative points of view.
-}

module WeekDaze.OutputFormat.XHTMLFormat(
-- * Functions
	composeBody,
	renderDocument
) where

import qualified	Data.Array.IArray
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import qualified	WeekDaze.ExecutionConfiguration.ExecutionOptions	as ExecutionConfiguration.ExecutionOptions
import qualified	WeekDaze.Model.Timetable				as Model.Timetable
import qualified	WeekDaze.Model.TimetableForWeek				as Model.TimetableForWeek
import qualified	WeekDaze.OutputConfiguration.Options			as OutputConfiguration.Options
import qualified	WeekDaze.OutputConfiguration.View			as OutputConfiguration.View
import qualified	WeekDaze.OutputFormat.XHTMLFormatLocationViewTimetable	as OutputFormat.XHTMLFormatLocationViewTimetable
import qualified	WeekDaze.OutputFormat.XHTMLFormatStudentViewTimetable	as OutputFormat.XHTMLFormatStudentViewTimetable
import qualified	WeekDaze.OutputFormat.XHTMLFormatTeacherViewTimetable	as OutputFormat.XHTMLFormatTeacherViewTimetable
import qualified	WeekDaze.ProblemConfiguration.ProblemAnalysis		as ProblemConfiguration.ProblemAnalysis
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters		as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.StudentView.Timetable				as StudentView.Timetable
import qualified	WeekDaze.Text.CSS					as Text.CSS
import qualified	WeekDaze.Text.XHTML					as Text.XHTML
import			System.FilePath((</>), (<.>))
import			Text.XHtml.Strict((<<), (+++), (!))

-- | Format the body of an XHTML-document, from the required views of the /timetable/.
composeBody :: (
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
	Show			timeslotId,
	Text.XHtml.Strict.HTML	campus,
	Text.XHtml.Strict.HTML	level,
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	stream,
	Text.XHtml.Strict.HTML	synchronisationId,
	Text.XHtml.Strict.HTML	teacherId,
	Text.XHtml.Strict.HTML	timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> ExecutionConfiguration.ExecutionOptions.ExecutionOptions criterionWeight fecundityDecayRatio populationDiversityRatio
	-> OutputConfiguration.Options.Options minimumContrastRatio
	-> ProblemConfiguration.ProblemAnalysis.ProblemAnalysis level locationId synchronisationId teacherId timeslotId
	-> Maybe ([String], Text.XHtml.Strict.Html)	-- ^ A List of warning-messages & a progress-log.
	-> Data.Set.Set OutputConfiguration.View.View
	-> Model.Timetable.AugmentMarkup
	-> Model.TimetableForWeek.GenericTimetableToMarkup minimumContrastRatio (StudentView.Timetable.Timetable timeslotId locationId teacherId level)
composeBody problemParameters executionOptions outputOptions problemAnalysis maybeRuntimeInformation displayViews displaySupplementaryInformation mergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom studentViewTimetable	= foldr1 (+++) $ Data.Maybe.maybe id (
	\(warnings, runtimeLog)	-> (
		box << (
			let
				displayState	= False
			in Text.XHTML.mkShowHideButton displayState Text.CSS.consoleOutputCSSIdentifier +++ Text.XHtml.Strict.h2 ! [
				Text.XHtml.Strict.title "Runtime-information."
			] << "Runtime-information" +++ Text.XHtml.Strict.thediv ! (
				if displayState
					then id
					else (Text.XHtml.Strict.thestyle "display: none" :)
			) [
				Text.XHtml.Strict.identifier Text.CSS.consoleOutputCSSIdentifier
			] << (
				Text.XHtml.Strict.unordList (
					map (
						Text.XHtml.Strict.paragraph ! [
							Text.XHtml.Strict.theclass Text.CSS.warningCSSIdentifier
						] <<
					) warnings
				) +++ runtimeLog
			)
		) :
	) -- Section.
 ) maybeRuntimeInformation [
	box << f problemParameters problemAnalysis displaySupplementaryInformation mergeDuplicateTimeslots displayAxisLabels weekend maybeGenerateLessonColourFrom studentViewTimetable |
		(True, f)	<- [
			(
				Data.Set.member OutputConfiguration.View.StudentView displayViews,
				OutputFormat.XHTMLFormatStudentViewTimetable.composeStudentViewTimetable
			), (
				Data.Set.member OutputConfiguration.View.TeacherView displayViews,
				OutputFormat.XHTMLFormatTeacherViewTimetable.composeTeacherViewTimetable executionOptions outputOptions
			), (
				Data.Set.member OutputConfiguration.View.LocationView displayViews,
				OutputFormat.XHTMLFormatLocationViewTimetable.composeLocationViewTimetable executionOptions
			)
		]
 ] where
	box :: Text.XHtml.Strict.Html -> Text.XHtml.Strict.Html
	box	= Text.XHtml.Strict.thediv ! [Text.XHtml.Strict.theclass Text.CSS.boxCSSIdentifier]

-- | Build an XHTML-document, from a title & an arbitrary XHTML-body.
renderDocument
	:: Maybe String			-- ^ An optional URL referencing a /CSS/-file.
	-> String			-- ^ Author.
	-> String			-- ^ Description.
	-> String			-- ^ Keywords.
	-> String			-- ^ The program-name.
	-> String			-- ^ The title of the document-body.
	-> Text.XHtml.Strict.Html	-- ^ The XHTML-content of the document-body.
	-> String			-- ^ The resulting XHTML for the document.
renderDocument maybeCSSURL author description keywords progName documentTitle content	= Text.XHtml.Strict.renderHtmlWithLanguage "en" {- ISO 639-1 -} $ Text.XHtml.Strict.header << [
	Text.XHtml.Strict.meta ! [
		Text.XHtml.Strict.httpequiv "Content-type",
		Text.XHtml.Strict.content "application/xhtml+xml;charset=UTF-8"
	],
	Text.XHtml.Strict.meta ! [
		Text.XHtml.Strict.name "author",
		Text.XHtml.Strict.content author
	],
	Text.XHtml.Strict.meta ! [
		Text.XHtml.Strict.name "description",
		Text.XHtml.Strict.content description
	],
	Text.XHtml.Strict.meta ! [
		Text.XHtml.Strict.name "keywords",
		Text.XHtml.Strict.content keywords
	],
	Text.XHtml.Strict.thelink ! [
		Text.XHtml.Strict.rel "icon",
		Text.XHtml.Strict.href $ ".." </> "images" </> "favicon" <.> "png",
		Text.XHtml.Strict.thetype "image/png"
	] << Text.XHtml.Strict.noHtml,
	Text.XHtml.Strict.thelink ! [
		Text.XHtml.Strict.rel "stylesheet",
		Text.XHtml.Strict.href $ ".." </> Text.CSS.cssSuffix {-use as directory-name-} </> progName <.> Text.CSS.cssSuffix,	-- Default.
		Text.XHtml.Strict.thetype referencedContentType
	] << Text.XHtml.Strict.noHtml,
	Data.Maybe.maybe Text.XHtml.Strict.noHtml (
		\cssURL -> Text.XHtml.Strict.thelink ! [
			Text.XHtml.Strict.rel "stylesheet",
			Text.XHtml.Strict.href cssURL,
			Text.XHtml.Strict.thetype referencedContentType
		] << Text.XHtml.Strict.noHtml
	) maybeCSSURL,
	Text.XHtml.Strict.thetitle << progName
 ] +++ Text.XHtml.Strict.body << [
	Text.XHtml.Strict.h1 ! [
		Text.XHtml.Strict.identifier "commandLine"
	] << documentTitle,
	content
 ] where
	referencedContentType :: String
	referencedContentType	= "text/css"

