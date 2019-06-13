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

 [@DESCRIPTION@]	Defines the format of a file.
-}

module WeekDaze.OutputConfiguration.FileFormat(
-- * Types
-- ** Data-types
	FileFormat(
--		MkFileFormat,
		getFilePath,
		getFormat
	),
-- * Constants
	stdoutProxy,
--	tag,
--	filePathTag,
-- * Functions
-- ** Constructor
	mkFileFormat
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	System.FilePath
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.OutputConfiguration.Format	as OutputConfiguration.Format

-- | A special file-name interpreted as a request for /stdout/.
stdoutProxy :: String
stdoutProxy	= "-"

-- | Used to qualify XML.
tag :: String
tag		= "fileFormat"

-- | Used to qualify XML.
filePathTag :: String
filePathTag	= "filePath"

-- | The format associated with a specified file-path.
data FileFormat minimumContrastRatio	= MkFileFormat {
	getFilePath	:: System.FilePath.FilePath,	-- ^ The special file-path 'stdoutProxy', will be interpreted as /stdout/.
	getFormat	:: OutputConfiguration.Format.Format minimumContrastRatio
} deriving (Eq, Show)

instance Show minimumContrastRatio => ToolShed.SelfValidate.SelfValidator (FileFormat minimumContrastRatio) where
	getErrors fileFormat	= ToolShed.SelfValidate.extractErrors [
		(
			not . System.FilePath.isValid $ getFilePath fileFormat,
			"invalid file-path; " ++ show fileFormat
		)
	 ]

instance Data.Default.Default (FileFormat minimumContrastRatio) where
	def	= MkFileFormat stdoutProxy Data.Default.def

-- | Smart constructor.
mkFileFormat
	:: Show minimumContrastRatio
	=> System.FilePath.FilePath					-- ^ A path to a file, into which the unprocessed configuration, formatted as XML, should be written.
	-> OutputConfiguration.Format.Format minimumContrastRatio	-- ^ The number of decimal digits with which to log the value of /lesson-criteria/ & /timetable-criteria/.
	-> FileFormat minimumContrastRatio
mkFileFormat filePath format
	| ToolShed.SelfValidate.isValid fileFormat	= fileFormat
	| otherwise					= error $ "WeekDaze.OutputConfiguration.FileFormat.mkFileFormat:\t" ++ ToolShed.SelfValidate.getFirstError fileFormat ++ "."
	where
		fileFormat	= MkFileFormat filePath format

instance (
	Fractional	minimumContrastRatio,
	HXT.XmlPickler	minimumContrastRatio,
	Ord		minimumContrastRatio,
	Show		minimumContrastRatio
 ) => HXT.XmlPickler (FileFormat minimumContrastRatio) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		uncurry mkFileFormat,		-- Construct from a Pair.
		getFilePath &&& getFormat	-- Deconstruct to a Pair.
	 ) $ HXT.xpTextAttr filePathTag `HXT.xpPair` HXT.xpickle {-Format-}

instance Control.DeepSeq.NFData	minimumContrastRatio => Control.DeepSeq.NFData (FileFormat minimumContrastRatio) where
	rnf	= Control.DeepSeq.rnf . (getFilePath &&& getFormat)
