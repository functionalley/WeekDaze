{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'OutputConfiguration.Options.Options'.
-}

module WeekDaze.Test.QuickCheck.OutputConfiguration.Options() where

import qualified	Test.QuickCheck
import			ToolShed.Test.QuickCheck.Arbitrary.Set()
import qualified	WeekDaze.OutputConfiguration.Options		as OutputConfiguration.Options
import			WeekDaze.Test.QuickCheck.OutputConfiguration.FileFormat()
import			WeekDaze.Test.QuickCheck.OutputConfiguration.Verbosity()

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance (
	Fractional	minimumContrastRatio,
	Ord		minimumContrastRatio,
	Show		minimumContrastRatio
 ) => Test.QuickCheck.Arbitrary (OutputConfiguration.Options.Options minimumContrastRatio) where
	arbitrary	= OutputConfiguration.Options.mkOptions <$> Test.QuickCheck.arbitrary {-FileFormats-} <*> (
		fmap (('c' :) . show . (`mod` (10 :: Int))) <$> Test.QuickCheck.arbitrary	-- maybeOutputConfigFilePath.
	 ) <*> Test.QuickCheck.elements [1 .. 15] {-NDecimalDigits-} <*> (
		('m' :) . show . (`mod` (10 :: Int)) <$> Test.QuickCheck.arbitrary	-- mnemonicSeparator.
	 ) <*> Test.QuickCheck.arbitrary {-Verbosity-}

