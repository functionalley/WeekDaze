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

 [@DESCRIPTION@]	Exports labels used to name CSS-classes, & functions to encode arbitrary CSS-identifiers.
-}

module WeekDaze.Text.CSS(
-- * Types
-- ** Type-synonyms
	CSSIdentifier,
-- * Constants
	boxCSSIdentifier,
	cssSuffix,
	consoleOutputCSSIdentifier,
	dataCSSIdentifier,
	numericDataCSSIdentifier,
	infoCSSIdentifier,
	warningCSSIdentifier,
	observerSummaryCSSIdentifier,
	timetableViewCSSIdentifier,
--	validIdentifierCharacterSet,
-- * Functions
-- ** Constructor
	mkIdentifier
) where

import qualified	Data.Char
import qualified	Data.Set
import qualified	System.FilePath

-- | The suffix used for CSS-files.
cssSuffix :: System.FilePath.FilePath
cssSuffix	= "css"

-- | Merely aids self-documentation.
type CSSIdentifier	= String

-- | A CSS class-label, for an HTML /box/.
boxCSSIdentifier :: CSSIdentifier
boxCSSIdentifier		= "box"

-- | A CSS class-label, denoting console-output.
consoleOutputCSSIdentifier :: CSSIdentifier
consoleOutputCSSIdentifier	= "consoleOutput"

-- | A CSS class-label, for arbitrary data.
dataCSSIdentifier :: CSSIdentifier
dataCSSIdentifier		= "data"

-- | A CSS class-label, for arbitrary data.
numericDataCSSIdentifier :: CSSIdentifier
numericDataCSSIdentifier	= "numericData"

-- | A CSS class-label, denoting some form of informative message.
infoCSSIdentifier :: CSSIdentifier
infoCSSIdentifier		= "info"

-- | A CSS class-label, denoting some form of warning message.
warningCSSIdentifier :: CSSIdentifier
warningCSSIdentifier		= "warning"

-- | A CSS class-label, for the individual /timetable/ for a week, as seen by some observer.
observerSummaryCSSIdentifier :: CSSIdentifier
observerSummaryCSSIdentifier	= "observerSummary"

-- | A CSS class-label, for a view of a /timetable/ for the week.
timetableViewCSSIdentifier :: CSSIdentifier
timetableViewCSSIdentifier	= "timetableView"

{- |
	* An identifier in CSS, can only be composed from a restricted character-set.

	* CAVEAT: valid HTML-identifiers also include ':' & '.'.
-}
validIdentifierCharacterSet :: Data.Set.Set Char
validIdentifierCharacterSet	= Data.Set.unions [
	Data.Set.fromDistinctAscList ['a' .. 'z'],
	Data.Set.fromDistinctAscList ['A' .. 'Z'],
	Data.Set.fromDistinctAscList ['0' .. '9'],
	Data.Set.fromList "-_"
 ]

{- |
	* Normalises an arbitrary string into a valid CSS-identifier.

	* <http://www.w3.org/TR/CSS2/syndata.html#characters>
-}
mkIdentifier :: String -> CSSIdentifier
mkIdentifier s	= removeIllegalPrefix $ filter (`Data.Set.member` validIdentifierCharacterSet) s where
	removeIllegalPrefix :: String -> String
	removeIllegalPrefix ('-' : s'@('-' : _))	= removeIllegalPrefix {-recurse-} s'	-- It can't start with a double hyphen.
	removeIllegalPrefix s'@('-' : (x : xs))
		| Data.Char.isDigit x	= removeIllegalPrefix {-recurse-} xs			-- It can't start with either, a hyphen followed by a digit, or with a digit.
		| otherwise		= s'							-- Terminate.
	removeIllegalPrefix s'@(x : xs)
		| Data.Char.isDigit x	= removeIllegalPrefix {-recurse-} xs			-- It can't start with a digit.
		| otherwise		= s'							-- Terminate.
	removeIllegalPrefix []				= error $ "WeekDaze.Text.CSS.mkIdentifier:\tnormalisation of '" ++ s ++ "', resulted in a null string."
