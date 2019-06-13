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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Data.Group.Group'.
-}

module WeekDaze.Test.QuickCheck.Data.Group() where

import qualified	Test.QuickCheck
import			ToolShed.Test.QuickCheck.Arbitrary.Set()
import qualified	WeekDaze.Data.Group	as Data.Group
import			WeekDaze.Test.QuickCheck.Identifiers.LocationId()
import			WeekDaze.Test.QuickCheck.Temporal.Time()

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance (
	Ord				timeslotId,
	Test.QuickCheck.Arbitrary	locationId,
	Test.QuickCheck.Arbitrary	timeslotId
 ) => Test.QuickCheck.Arbitrary (Data.Group.Profile timeslotId locationId) where
	arbitrary	= Data.Group.mkProfile <$> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary <*> Test.QuickCheck.arbitrary

