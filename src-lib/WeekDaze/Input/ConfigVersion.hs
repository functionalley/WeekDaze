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

 [@DESCRIPTION@]	Describes the version of the configuration-structure, & facilitates translation to & from a dotted-decimal string representation.
-}

module WeekDaze.Input.ConfigVersion(
-- * Types
-- ** Type-synonyms
	ConfigVersion,
-- * Constants
	tag,
	elementTag,
	defaultConfigVersion,
--	separator,
-- * Functions
	normalise,
-- ** Translation
	fromString,
	toString
) where

import qualified	Data.List

-- | Used to qualify SQL & XML.
tag :: String
tag		= "configVersion"

-- | Used to qualify XML.
elementTag :: String
elementTag	= "versionElement"

-- | The type of the version of the configuration-file format; major to minor.
type ConfigVersion	= [Int]

-- | The default value for 'getConfigVersion'.
defaultConfigVersion :: ConfigVersion
defaultConfigVersion	= []

-- | Discard irrelevant trailing zeroes.
normalise :: ConfigVersion -> ConfigVersion
normalise	= Data.List.dropWhileEnd (== 0)

-- | The separator to use when representing 'ConfigVersion' as a dotted-decimal 'String'.
separator :: Char
separator	= '.'

{- |
	* Splits a dotted-decimal string into a list of integers.

	* CAVEAT: negative version-elements aren't accepted & trailing zeroes are dropped.
-}
fromString :: String -> ConfigVersion
fromString	= normalise . slave where
	slave s
		| null remainder	= [i]	-- Terminate.
		| otherwise		= i : slave (tail {-drop the separator-} remainder) {-recurse-}
		where
			(versionElement, remainder)	= break (== separator) s	-- CAVEAT: either part may be null.

			i
				| null versionElement	= 0	-- Interpret a blank element as zero.
				| num < 0		= error $ "WeekDaze.Input.ConfigVersion.fromString:\tnegative version-element" ++ show num ++ "."
				| otherwise		= num
				where
					num	= read versionElement	-- Parse.

-- | Represent a /ConfigVersion/ as a dotted-decimal string.
toString :: ConfigVersion -> String
toString	= Data.List.intercalate [separator] . map show

