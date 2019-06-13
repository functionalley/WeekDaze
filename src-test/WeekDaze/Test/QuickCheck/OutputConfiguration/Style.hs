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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'OutputConfiguration.Style.Style'.
-}

module WeekDaze.Test.QuickCheck.OutputConfiguration.Style() where

import qualified	Control.Arrow
import qualified	Test.QuickCheck
import qualified	WeekDaze.OutputConfiguration.Style	as OutputConfiguration.Style
import			ToolShed.Test.QuickCheck.Arbitrary.Set()
import			WeekDaze.Test.QuickCheck.Model.GenerateLessonColourFrom()
import			WeekDaze.Test.QuickCheck.OutputConfiguration.View()
import			WeekDaze.Test.QuickCheck.Temporal.Day()
import			WeekDaze.Test.QuickCheck.Temporal.TimeAxes()

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance (
	Fractional	minimumContrastRatio,
	Ord		minimumContrastRatio
 ) => Test.QuickCheck.Arbitrary (OutputConfiguration.Style.Style minimumContrastRatio) where
	arbitrary	= OutputConfiguration.Style.mkStyle <$> Test.QuickCheck.arbitrary {-Set View-} <*> (
		fmap (('c' :) . show . (`mod` (10 :: Int))) <$> Test.QuickCheck.arbitrary	-- maybeCSSFilePath.
	 ) <*> Test.QuickCheck.arbitrary {-getMergeDuplicateTimeslots-} <*> Test.QuickCheck.arbitrary {-getDisplayAxisLabels-} <*> Test.QuickCheck.arbitrary {-getDisplayRuntimeLog-} <*> Test.QuickCheck.arbitrary {-getDisplaySupplementaryInformation-} <*> Test.QuickCheck.arbitrary {-getWeekend-} <*> (
		fmap (
			Control.Arrow.second $ recip . fromInteger . succ . (`mod` 16)	-- Constrain to the unit interval.
		) <$> Test.QuickCheck.arbitrary	--getMaybeGenerateLessonColour.
	 )

