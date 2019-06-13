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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'Temporal.FreePeriodPreference.FreePeriodPreference'.
-}

module WeekDaze.Test.QuickCheck.Temporal.FreePeriodPreference() where

import qualified	Test.QuickCheck
import qualified	WeekDaze.Temporal.FreePeriodPreference	as Temporal.FreePeriodPreference

instance Test.QuickCheck.Arbitrary Temporal.FreePeriodPreference.FreePeriodPreference where
	arbitrary	= Test.QuickCheck.elements [minBound .. maxBound]

