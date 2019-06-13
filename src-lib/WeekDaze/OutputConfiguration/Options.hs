{-# LANGUAGE CPP, FlexibleContexts #-}
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

 [@DESCRIPTION@]	Defines options related to program-output.
-}

module WeekDaze.OutputConfiguration.Options(
-- * Types
-- ** Type-synonyms
	NDecimalDigits,
-- ** Data-types
	Options(
--		MkOptions,
		getFileFormats,
		getMaybeOutputConfigFilePath,
		getNDecimalDigits,
		getStudentBodyMnemonicSeparator,
		getVerbosity
	),
-- * Constants
--	tag,
--	fileFormatsTag,
	nDecimalDigitsTag,
	outputConfigFilePathTag,
--	studentBodyMnemonicSeparatorTag,
-- * Functions
-- ** Constructor
	mkOptions
) where

import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	Distribution.Verbosity
import qualified	System.FilePath
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Aggregate.StudentClass		as Aggregate.StudentClass
import qualified	WeekDaze.OutputConfiguration.FileFormat	as OutputConfiguration.FileFormat

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	WeekDaze.Database.Selector		as Database.Selector
import qualified	WeekDaze.OutputConfiguration.Format	as OutputConfiguration.Format
import qualified	WeekDaze.OutputConfiguration.Verbosity	as OutputConfiguration.Verbosity

instance (Fractional minimumContrastRatio, Show minimumContrastRatio) => Database.Selector.Selector (Options minimumContrastRatio) where
	fromDatabase connection	projectIdSql	= let
		tableName :: Database.Selector.TableName
		tableName	= Database.Selector.tablePrefix ++ tag
	 in do
		optionsRows	<- Database.Selector.select connection [
			nDecimalDigitsTag,
			studentBodyMnemonicSeparatorTag,
			OutputConfiguration.Verbosity.tag
		 ] [tableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

		let
			options'	= case optionsRows of
				[]		-> Data.Default.def
				[optionsRow]	-> case optionsRow of
					[nDecimalDigitsSql, mnemonicSeparatorSql, verbositySql]	-> (
						\options -> Data.Maybe.maybe options (
							\s -> options { getStudentBodyMnemonicSeparator = s }
						) $ Database.HDBC.fromSql mnemonicSeparatorSql
					 ) $ Data.Default.def {
						getNDecimalDigits	= either (
							error . showString "WeekDaze.OutputConfiguration.Options.fromDatabase:\tfailed to parse the value for " . shows nDecimalDigitsTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql nDecimalDigitsSql,
						getVerbosity		= Database.HDBC.fromSql verbositySql
					}
					_							-> error $ "WeekDaze.OutputConfiguration.Options.fromDatabase:\tunexpected number of columns=" ++ show (length optionsRow) ++ " in row of table " ++ show tableName ++ "."
				_		-> error $ "WeekDaze.OutputConfiguration.Options.fromDatabase:\tunexpected number of rows=" ++ show (length optionsRows) ++ " selected from table " ++ show tableName ++ "."

		style	<- Database.Selector.fromDatabase connection projectIdSql

		return {-to IO-monad-} options' {
			getFileFormats	= [OutputConfiguration.FileFormat.mkFileFormat OutputConfiguration.FileFormat.stdoutProxy {-CAVEAT: hard-coded-} $ OutputConfiguration.Format.XHTML style]
		}
#else
import			WeekDaze.OutputConfiguration.Verbosity()
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag				= "outputOptions"

-- | Used to qualify XML.
fileFormatsTag :: String
fileFormatsTag			= "fileFormats"

-- | Used to qualify SQL & XML.
nDecimalDigitsTag :: String
nDecimalDigitsTag		= "nDecimalDigits"

-- | Used to qualify XML.
outputConfigFilePathTag :: String
outputConfigFilePathTag		= "outputConfigFilePath"

-- | Used to qualify SQL & XML.
studentBodyMnemonicSeparatorTag :: String
studentBodyMnemonicSeparatorTag	= "studentBodyMnemonicSeparator"

-- | A number of decimals digits.
type NDecimalDigits		= Int

-- | Defines the set of output-options.
data Options minimumContrastRatio = MkOptions {
	getFileFormats			:: [OutputConfiguration.FileFormat.FileFormat minimumContrastRatio],	-- ^ Defines the formats of the required output-files.
	getMaybeOutputConfigFilePath	:: Maybe System.FilePath.FilePath,					-- ^ Optional path to a file, into which the unprocessed configuration, formatted as XML, should be written (obliterating any existing file-contents).
	getNDecimalDigits		:: NDecimalDigits,							-- ^ The precision to which fractional auxiliary data is displayed.
	getStudentBodyMnemonicSeparator	:: Aggregate.StudentClass.MnemonicSeparator,				-- ^ The separator used when merging the mnemonics of /student-bodies/.
	getVerbosity			:: Distribution.Verbosity.Verbosity					-- ^ Set the threshold for ancillary information-output.
} deriving (Eq, Show)

instance Show minimumContrastRatio => ToolShed.SelfValidate.SelfValidator (Options minimumContrastRatio) where
	getErrors MkOptions {
		getFileFormats			= fileFormats,
		getMaybeOutputConfigFilePath	= maybeOutputConfigFilePath,
		getNDecimalDigits		= nDecimalDigits
	} = ToolShed.SelfValidate.extractErrors [
		let
--			incompatibleFileFormats :: [[OutputConfiguration.FileFormat.FileFormat minimumContrastRatio]]
			incompatibleFileFormats	= filter ((/= 1) . length) $ Data.List.Extra.groupSortOn OutputConfiguration.FileFormat.getFilePath fileFormats
		in (
			not $ null incompatibleFileFormats,
			"duplicate file-paths; " ++ show incompatibleFileFormats
		), (
			Data.Maybe.maybe False (not . System.FilePath.isValid {-i.e. non-null on POSIX-}) maybeOutputConfigFilePath,
			"invalid path to output config-file; " ++ show (Data.Maybe.fromJust maybeOutputConfigFilePath)
		), (
			nDecimalDigits < 1,
			show nDecimalDigitsTag ++ "=" ++ show nDecimalDigits ++ ", must exceed zero"
		),
		let
			maxNDecimalDigits	= floor $ fromIntegral (
				floatDigits (
					undefined :: Double	-- CAVEAT: the actual type could be merely 'Float', but that's currently unknown.
				)
			 ) * (logBase 10 2 :: Double)
		in (
			nDecimalDigits > maxNDecimalDigits,
			nDecimalDigitsTag ++ "=" ++ show nDecimalDigits ++ ", shouldn't exceed " ++ show maxNDecimalDigits
		)
	 ]

instance Data.Default.Default (Options minimumContrastRatio) where
	def = MkOptions {
		getFileFormats			= [Data.Default.def],
		getMaybeOutputConfigFilePath	= Nothing,
		getNDecimalDigits		= 3,
		getStudentBodyMnemonicSeparator	= " / ",
		getVerbosity			= Data.Default.def
	}

instance Control.DeepSeq.NFData minimumContrastRatio => Control.DeepSeq.NFData (Options minimumContrastRatio) where
	rnf (MkOptions x0 x1 x2 x3 x4)	= Control.DeepSeq.rnf (x0, x1, x2, x3, x4)

-- | Smart constructor.
mkOptions
	:: Show minimumContrastRatio
	=> [OutputConfiguration.FileFormat.FileFormat minimumContrastRatio]	-- ^ The formats in which to file the results.
	-> Maybe System.FilePath.FilePath					-- ^ An optional path to a file, into which the unprocessed configuration, formatted as XML, should be written.
	-> NDecimalDigits							-- ^ The number of decimal digits with which to log the value of /lesson-criteria/ & /timetable-criteria/.
	-> Aggregate.StudentClass.MnemonicSeparator				-- ^ The separator to use when /student-bodies/ with identical profiles have been merged automatically during runtime.
	-> Distribution.Verbosity.Verbosity					-- ^ The amount of auxiliary data to log.
	-> Options minimumContrastRatio
mkOptions fileFormats maybeOutputConfigFilePath nDecimalDigits studentBodyMnemonicSeparator verbosity
	| ToolShed.SelfValidate.isValid options	= options
	| otherwise				= error $ "WeekDaze.OutputConfiguration.Options.mkOptions:\t" ++ ToolShed.SelfValidate.getFirstError options ++ "."
	where
		options	= MkOptions fileFormats maybeOutputConfigFilePath nDecimalDigits studentBodyMnemonicSeparator verbosity

instance (
	Fractional	minimumContrastRatio,
	HXT.XmlPickler	minimumContrastRatio,
	Ord		minimumContrastRatio,
	Show		minimumContrastRatio
 ) => HXT.XmlPickler (Options minimumContrastRatio) where
	xpickle	= HXT.xpDefault Data.Default.def . HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e)	-> mkOptions a b c d e,	-- Construct from a tuple.
		\MkOptions {
			getFileFormats			= fileFormats,
			getMaybeOutputConfigFilePath	= maybeOutputConfigFilePath,
			getNDecimalDigits		= nDecimalDigits,
			getStudentBodyMnemonicSeparator	= studentBodyMnemonicSeparator,
			getVerbosity			= verbosity
		} -> (fileFormats, maybeOutputConfigFilePath, nDecimalDigits, studentBodyMnemonicSeparator, verbosity)	-- Deconstruct into a tuple.
	 ) $ HXT.xp5Tuple (
		HXT.xpDefault (getFileFormats defaultOptions) . HXT.xpElem fileFormatsTag $ HXT.xpList1 {-can't be null-} HXT.xpickle {-FileFormat-}
	 ) (
		HXT.xpOption $ HXT.xpTextAttr outputConfigFilePathTag {-can't be null-}
	 ) (
		getNDecimalDigits defaultOptions `HXT.xpDefault` HXT.xpAttr nDecimalDigitsTag HXT.xpInt
	 ) (
		getStudentBodyMnemonicSeparator defaultOptions `HXT.xpDefault` HXT.xpAttr studentBodyMnemonicSeparatorTag HXT.xpText0 {-can be null-}
	 ) (
		getVerbosity defaultOptions `HXT.xpDefault` HXT.xpickle
	 ) where
		defaultOptions	= Data.Default.def

