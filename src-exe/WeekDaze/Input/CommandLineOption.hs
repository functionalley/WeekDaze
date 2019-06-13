{-
	Copyright (C) 2014 Dr. Alistair Ward

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

 [@DESCRIPTION@]	To facilitate processing of command-line options.
-}

module WeekDaze.Input.CommandLineOption(
-- * Types
-- ** Type-synonyms
	Flag,
-- ** Data-types
	CommandLineOption(
--		IOAction,
--		OptionsMutator,
--		ConfigLocationParameter
	),
-- * Constants
	longFlagPrefix,
-- * Functions
	partition3,
	getArgs,
--	read',
	readArg,
	readBoundedIntegral,
-- ** Constructors
	mkIOAction,
	mkOptionsMutator,
	mkConfigLocationParameter
) where

import qualified	Data.List
import qualified	Data.Map

-- | Synonym.
type Flag	= String

-- | The varieties of command-line option.
data CommandLineOption options
	= IOAction (IO ())			-- ^ A command-line option which requests an IO-action.
	| OptionsMutator (options -> options)	-- ^ A command-line option which directly specifies a configuration-parameter.
	| ConfigLocationParameter Flag String	-- ^ A command-line option which specifies the location of some configuration.

-- | Construct an 'IOAction' command-line option, from the required action.
mkIOAction :: IO () -> CommandLineOption options
mkIOAction	= IOAction

-- | Construct an 'OptionsMutator' command-line option from a handler-function.
mkOptionsMutator :: (options -> options) -> CommandLineOption options
mkOptionsMutator	= OptionsMutator

-- | Construct a 'ConfigLocationParameter' command-line option, from the command-line flag used to denote it, & a string explaining its argument-type.
mkConfigLocationParameter :: Flag -> String -> CommandLineOption options
mkConfigLocationParameter	= ConfigLocationParameter

-- | Partition a list of 'CommandLineOption' according to their data-constructor.
partition3 :: [CommandLineOption options] -> ([IO ()], [options -> options], Data.Map.Map Flag String)
partition3 categories	= (
	[action | IOAction action <- categories],
	[mutator | OptionsMutator mutator <- categories],
	Data.Map.fromList [(f, s) | ConfigLocationParameter f s <- categories]
 )

-- | The prefix used to denote the long form of a command-line flag.
longFlagPrefix :: Flag
longFlagPrefix	= "--"

{- |
	* Return the list of arguments extracted from the command-line, which match the specified long flag preceded by "--".

	* CAVEAT:
		to match a long flag, all unique abbreviations must also be supplied.
		doesn't cope with short flags preceded by '-'.
-}
getArgs :: [Flag] -> [String] -> [String]
getArgs flags	= slave where
	slave []	= []
	slave (x : xs)
		| x `elem` flags	= case xs of
			s : remainder	-> s : slave remainder {-recurse-}
			[]		-> error $ "option " ++ show x ++ " requires an argument."
		| any (
			(`Data.List.isPrefixOf` x) . (++ "=")
		) flags			= case dropWhile (/= '=') x of
			_ : remainder	-> remainder : slave xs
			[]		-> error $ "option " ++ show x ++ " requires a non-null argument."
		| otherwise		= slave xs	-- Recurse.

-- | On failure to parse the specified string, returns an explanatory error-message.
read' :: Read a => String -> String -> a
read' errorMessage s	= case reads s of
	[(x, "")]	-> x
	_		-> error . showString errorMessage $ shows s "."

-- | On failure to parse a command-line argument, returns an explanatory error.
readArg :: Read a => String -> a
readArg	= read' "failed to parse command-line argument "

-- | Reads a bounded integral from the command-line, guarding against overflow.
readBoundedIntegral :: Integral i => String -> i
readBoundedIntegral s
	| fromIntegral bounded /= unbounded	= error $ "WeekDaze.Input.CommandLineOption.readBoundedIntegral:\tintegral value exceeds permissible bounds; " ++ show unbounded ++ "."
	| otherwise				= bounded
	where
		unbounded	= readArg s
		bounded		= fromInteger unbounded

