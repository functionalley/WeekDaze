{-# LANGUAGE CPP #-}
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

 [@DESCRIPTION@]	Defines the style of the output.
-}

module WeekDaze.OutputConfiguration.Style(
-- * Types
-- ** Type-synonyms
	DisplayRuntimeInformation,
	DisplaySupplementaryInformation,
--	GenerateLessonColour,
-- ** Data-types
	Style(
--		MkStyle,
		getDisplayViews,
		getMaybeCSSURL,
		getMergeDuplicateTimeslots,
		getDisplayAxisLabels,
		getDisplayRuntimeInformation,
		getDisplaySupplementaryInformation,
		getWeekend,
		getMaybeGenerateLessonColour
	),
-- * Constants
--	tag,
--	byDayTag,
--	byTimeslotTag,
--	cssURLTag,
--	displayAxisLabelsTag,
	displayViewsTag,
	displayRuntimeInformationTag,
--	displaySupplementaryInformationTag,
--	mergeDuplicateTimeslotsTag,
--	perspectiveTag,
	weekendTag,
--	minimumContrastRatioTag,
--	minimumContrastRatioDefault,
-- * Functions
-- ** Constructor
	mkStyle
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Model.GenerateLessonColourFrom	as Model.GenerateLessonColourFrom
import qualified	WeekDaze.OutputConfiguration.View	as OutputConfiguration.View
import qualified	WeekDaze.Temporal.Day			as Temporal.Day
import qualified	WeekDaze.Temporal.TimeAxes		as Temporal.TimeAxes
import			WeekDaze.Enhanced.EnhancedBool()

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	WeekDaze.Database.Selector		as Database.Selector

instance Fractional minimumContrastRatio => Database.Selector.Selector (Style minimumContrastRatio) where
	fromDatabase connection	projectIdSql	= let
		tableName :: Database.Selector.TableName
		tableName	= showString Database.Selector.tablePrefix tag
	 in do
		styleRows	<- Database.Selector.select connection [
--			displayViews,
			cssURLTag,
			showString mergeDuplicateTimeslotsTag byDayTag,
			showString mergeDuplicateTimeslotsTag byTimeslotTag,
			showString displayAxisLabelsTag byDayTag,
			showString displayAxisLabelsTag byTimeslotTag,
			displayRuntimeInformationTag,
			displaySupplementaryInformationTag,
			weekendTag,
			Model.GenerateLessonColourFrom.tag,
			minimumContrastRatioTag
		 ] [tableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		return {-to IO-monad-} $ case styleRows of
			[]		-> Data.Default.def
			[styleRow]	-> case styleRow of
				[
--					displayViewsSql,
					cssURLSql,
					mergeDuplicateTimeslotsByDaySql,
					mergeDuplicateTimeslotsByTimeslotSql,
					displayAxisLabelsByDaySql,
					displayAxisLabelsByTimeslotSql,
					displayRuntimeInformationSql,
					displaySupplementaryInformationSql,
					weekendSql,
					generateLessonColourFromSql,
					minimumContrastRatioSql
				 ] -> Data.Default.def {
--					getDisplayViews,
					getMaybeCSSURL				= Database.HDBC.fromSql cssURLSql,
					getMergeDuplicateTimeslots		= either (
						error . showString "WeekDaze.OutputConfiguration.Style.fromDatabase:\tfailed to parse the value for " . shows (mergeDuplicateTimeslotsTag ++ byDayTag) . showString " read from the database; " . show
					) id (
						Database.HDBC.safeFromSql mergeDuplicateTimeslotsByDaySql
					) `Temporal.TimeAxes.mkTimeAxes` either (
						error . showString "WeekDaze.OutputConfiguration.Style.fromDatabase:\tfailed to parse the value for " . shows (mergeDuplicateTimeslotsTag ++ byTimeslotTag) . showString " read from the database; " . show
					) id (
						Database.HDBC.safeFromSql mergeDuplicateTimeslotsByTimeslotSql
					),
					getDisplayAxisLabels			= either (
						error . showString "WeekDaze.OutputConfiguration.Style.fromDatabase:\tfailed to parse the value for " . shows (displayAxisLabelsTag ++ byDayTag) . showString " read from the database; " . show
					) id (
						Database.HDBC.safeFromSql displayAxisLabelsByDaySql
					) `Temporal.TimeAxes.mkTimeAxes` either (
						error . showString "WeekDaze.OutputConfiguration.Style.fromDatabase:\tfailed to parse the value for " . shows (displayAxisLabelsTag ++ byTimeslotTag) . showString " read from the database; " . show
					) id (
						Database.HDBC.safeFromSql displayAxisLabelsByTimeslotSql
					),
					getDisplayRuntimeInformation		= either (
						error . showString "WeekDaze.OutputConfiguration.Style.fromDatabase:\tfailed to parse the value for " . shows displayRuntimeInformationTag . showString " read from the database; " . show
					) id $ Database.HDBC.safeFromSql displayRuntimeInformationSql,
					getDisplaySupplementaryInformation	= either (
						error . showString "WeekDaze.OutputConfiguration.Style.fromDatabase:\tfailed to parse the value for " . shows displaySupplementaryInformationTag . showString " read from the database; " . show
					) id $ Database.HDBC.safeFromSql displaySupplementaryInformationSql,
					getWeekend				= Database.HDBC.fromSql weekendSql,
					getMaybeGenerateLessonColour		= flip (,) (
						Data.Maybe.maybe minimumContrastRatioDefault realToFrac (
							either (
								error . showString "WeekDaze.OutputConfiguration.Style.fromDatabase:\tfailed to parse the value for " . shows minimumContrastRatioTag . showString " read from the database; " . show
							) id (
								Database.HDBC.safeFromSql minimumContrastRatioSql :: Data.Convertible.ConvertResult (Maybe Double)
							)
						)
					) `fmap` either (
						error . showString "WeekDaze.OutputConfiguration.Style.fromDatabase:\tfailed to parse the value for " . shows Model.GenerateLessonColourFrom.tag . showString " read from the database; " . show
					) id (
						Database.HDBC.safeFromSql generateLessonColourFromSql
					)
				}
				_	-> error $ "WeekDaze.OutputConfiguration.Style.fromDatabase:\tunexpected number of columns=" ++ show (length styleRow) ++ " in row of table " ++ show tableName ++ "."
			_		-> error $ "WeekDaze.OutputConfiguration.Style.fromDatabase:\tunexpected number of rows=" ++ show (length styleRows) ++ " selected from table " ++ show tableName ++ "."
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag					= "style"

-- | Used to qualify SQL.
byDayTag :: String
byDayTag				= "ByDay"

-- | Used to qualify SQL.
byTimeslotTag :: String
byTimeslotTag				= "ByTimeslot"

-- | Used to qualify XML.
displayViewsTag :: String
displayViewsTag				= "displayViews"

-- | Used to qualify SQL & XML.
cssURLTag :: String
cssURLTag				= "cssURL"

-- | Used to qualify SQL & XML.
mergeDuplicateTimeslotsTag :: String
mergeDuplicateTimeslotsTag		= "mergeDuplicateTimeslots"

-- | Used to qualify SQL & XML.
displayAxisLabelsTag :: String
displayAxisLabelsTag			= "displayAxisLabels"

-- | Used to qualify SQL & XML.
displayRuntimeInformationTag :: String
displayRuntimeInformationTag		= "displayRuntimeInformation"

-- | Used to qualify SQL & XML.
displaySupplementaryInformationTag :: String
displaySupplementaryInformationTag	= "displaySupplementaryInformation"

-- | Used to qualify XML.
perspectiveTag :: String
perspectiveTag				= "perspective"

-- | Used to qualify SQL & XML.
weekendTag :: String
weekendTag				= "weekend"

-- | Used to qualify SQL & XML.
minimumContrastRatioTag :: String
minimumContrastRatioTag			= "minimumContrastRatio"

-- | Default value.
minimumContrastRatioDefault :: Fractional minimumContrastRatio => minimumContrastRatio
minimumContrastRatioDefault		= recip 16

-- | Whether to apply a concept to each of the two Cartesian axes of a timetable.
type TimeAxes				= Temporal.TimeAxes.TimeAxes Bool

-- | Whether to display the runtime-log in the output markup.
type DisplayRuntimeInformation		= Bool

-- | Whether to display the runtime-log in the output markup.
type DisplaySupplementaryInformation	= Bool

-- | The attribute from which to generate the colour of a /lesson/ & the minimum acceptable contrast-ratio wrt to its complementary background colour.
type GenerateLessonColour minimumContrastRatio	= (Model.GenerateLessonColourFrom.GenerateLessonColourFrom, minimumContrastRatio)	-- Pair.

-- | Defines the style in which the timetable is rendered.
data Style minimumContrastRatio	= MkStyle {
	getDisplayViews				:: Data.Set.Set OutputConfiguration.View.View,		-- ^ The /view/s from which to display the /timetable/.
	getMaybeCSSURL				:: Maybe String,					-- ^ Optional URL to a Cascading Style-Sheet.
	getMergeDuplicateTimeslots		:: TimeAxes,						-- ^ Options to merge duplicate /lesson/s between adjacent /day/s, & between consecutive /time-slot/s respectively, in the presentation of the solution.
	getDisplayAxisLabels			:: TimeAxes,						-- ^ Options to display /day/ & /timeslot-id/ axis-labels respectively, in the presentation of the solution.
	getDisplayRuntimeInformation		:: DisplayRuntimeInformation,				-- ^ Option to display both warnings & the runtime-log, in the presentation of the solution.
	getDisplaySupplementaryInformation	:: DisplaySupplementaryInformation,			-- ^ Option to augment the output with supplementary information.
	getWeekend				:: Temporal.Day.Weekend,				-- ^ The set of /day/s generally considered to constitute the weekend rather than work-days.
	getMaybeGenerateLessonColour		:: Maybe (GenerateLessonColour minimumContrastRatio)	-- ^ Whether to generate the colour of a /lesson/ from one of its attributes or to delegate colouring to the CSS-file specified via 'getMaybeCSSURL'.
} deriving (Eq, Show)

instance (
	Num	minimumContrastRatio,
	Ord	minimumContrastRatio
 ) => ToolShed.SelfValidate.SelfValidator (Style minimumContrastRatio) where
	getErrors MkStyle {
		getDisplayViews			= displayViews,
		getMaybeGenerateLessonColour	= maybeGenerateLessonColour
	} = ToolShed.SelfValidate.extractErrors [
		(
			Data.Set.null displayViews,
			"at least one view of the results must be specified; " ++ show displayViewsTag
		), (
			Data.Maybe.maybe False (
				(
					\minimumContrastRatio -> any ($ minimumContrastRatio) [(< 0), (> 1)]
				) . snd
			) maybeGenerateLessonColour,
			show minimumContrastRatioTag ++ "' must be within the closed unit-interval '[0,1]'"
		)
	 ]

instance Data.Default.Default (Style minimumContrastRatio) where
	def = MkStyle {
		getDisplayViews				= Data.Set.fromList OutputConfiguration.View.range,
		getMaybeCSSURL				= Nothing,
		getMergeDuplicateTimeslots		= Temporal.TimeAxes.mkTimeAxes True True,
		getDisplayAxisLabels			= Temporal.TimeAxes.mkTimeAxes True True,
		getDisplayRuntimeInformation		= True,
		getDisplaySupplementaryInformation	= True,
		getWeekend				= Data.Set.fromList [minBound, maxBound],
		getMaybeGenerateLessonColour		= Nothing
	}

instance Control.DeepSeq.NFData minimumContrastRatio => Control.DeepSeq.NFData (Style minimumContrastRatio) where
	rnf (MkStyle x0 x1 x2 x3 x4 x5 x6 x7)	= Control.DeepSeq.rnf (x0, x1, x2, x3, x4, x5, x6, x7)

-- | Smart constructor.
mkStyle :: (
	Num	minimumContrastRatio,
	Ord	minimumContrastRatio
 )
	=> Data.Set.Set OutputConfiguration.View.View	-- ^ The /view/s from which to display the /timetable/, when rendering as xhtml.
	-> Maybe String					-- ^ The optional URL to a CSS-file.
	-> TimeAxes
	-> TimeAxes
	-> DisplayRuntimeInformation
	-> DisplaySupplementaryInformation
	-> Temporal.Day.Weekend				-- ^ Those /day/s of the weekend, which may be visually distingished by CSS.
	-> Maybe (Model.GenerateLessonColourFrom.GenerateLessonColourFrom, minimumContrastRatio)
	-> Style minimumContrastRatio
mkStyle displayViews maybeCSSURL mergeDuplicateTimeslots displayAxisLabels displayRuntimeInformation displaySupplementaryInformation weekend maybeGenerateLessonColour
	| ToolShed.SelfValidate.isValid style	= style
	| otherwise				= error $ "WeekDaze.OutputConfiguration.Style.mkStyle:\t" ++ ToolShed.SelfValidate.getFirstError style ++ "."
	where
		style = MkStyle {
			getDisplayViews				= displayViews,
			getMaybeCSSURL				= maybeCSSURL,
			getMergeDuplicateTimeslots		= mergeDuplicateTimeslots,
			getDisplayAxisLabels			= displayAxisLabels,
			getDisplayRuntimeInformation		= displayRuntimeInformation,
			getDisplaySupplementaryInformation	= displaySupplementaryInformation,
			getWeekend				= weekend,
			getMaybeGenerateLessonColour		= maybeGenerateLessonColour
		}

instance (
	Fractional	minimumContrastRatio,
	HXT.XmlPickler	minimumContrastRatio,
	Ord		minimumContrastRatio
 ) => HXT.XmlPickler (Style minimumContrastRatio) where
	xpickle	= HXT.xpDefault defaultStyle . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, g, h)	-> mkStyle a b c d e f g h,	-- Construct from a tuple.
		\MkStyle {
			getDisplayViews				= displayViews,
			getMaybeCSSURL				= maybeCSSURL,
			getMergeDuplicateTimeslots		= mergeDuplicateTimeslots,
			getDisplayAxisLabels			= displayAxisLabels,
			getDisplayRuntimeInformation		= displayRuntimeInformation,
			getDisplaySupplementaryInformation	= displaySupplementaryInformation,
			getWeekend				= weekend,
			getMaybeGenerateLessonColour		= maybeGenerateLessonColour
		} -> (displayViews, maybeCSSURL, mergeDuplicateTimeslots, displayAxisLabels, displayRuntimeInformation, displaySupplementaryInformation, weekend, maybeGenerateLessonColour) -- Deconstruct into a tuple.
	 ) $ HXT.xp8Tuple (
		HXT.xpDefault (getDisplayViews Data.Default.def) . HXT.xpElem displayViewsTag . HXT.xpWrap (
			Data.Set.fromList,
			Data.Set.toList
		) . HXT.xpList1 {-can't be null-} $ HXT.xpElem perspectiveTag HXT.xpickle {-View-}
	 ) (
		HXT.xpOption $ HXT.xpTextAttr cssURLTag {-can't be null-}
	 ) (
		getMergeDuplicateTimeslots defaultStyle `HXT.xpDefault` HXT.xpElem mergeDuplicateTimeslotsTag HXT.xpickle {-TimeAxes-}
	 ) (
		getDisplayAxisLabels defaultStyle `HXT.xpDefault` HXT.xpElem displayAxisLabelsTag HXT.xpickle {-TimeAxes-}
	 ) (
		getDisplayRuntimeInformation defaultStyle `HXT.xpDefault` HXT.xpAttr displayRuntimeInformationTag HXT.xpickle {-Bool-}
	 ) (
		getDisplaySupplementaryInformation defaultStyle `HXT.xpDefault` HXT.xpAttr displaySupplementaryInformationTag HXT.xpickle {-Bool-}
	 ) (
		HXT.xpDefault (getWeekend defaultStyle) . HXT.xpElem weekendTag . HXT.xpWrap (
			Data.Set.fromList,	-- Construct from a List.
			Data.Set.toList		-- Deconstruct to a List.
		) $ HXT.xpList {-can be null-} HXT.xpickle {-Day-}
	 ) (
		HXT.xpOption . HXT.xpElem "generateLessonColour" $ HXT.xpickle {-GenerateLessonColourFrom-} `HXT.xpPair` HXT.xpDefault minimumContrastRatioDefault (
			HXT.xpAttr minimumContrastRatioTag HXT.xpickle
		) -- GenerateLessonColour.
	 ) where
		defaultStyle	= Data.Default.def

