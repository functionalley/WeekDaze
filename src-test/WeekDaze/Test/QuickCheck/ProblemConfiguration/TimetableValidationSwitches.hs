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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' for 'ProblemConfiguration.TimetableValidationSwitches.TimetableValidationSwitches'.
-}

module WeekDaze.Test.QuickCheck.ProblemConfiguration.TimetableValidationSwitches() where

import qualified	Test.QuickCheck
import qualified	WeekDaze.ProblemConfiguration.TimetableValidationSwitches	as ProblemConfiguration.TimetableValidationSwitches

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance Test.QuickCheck.Arbitrary ProblemConfiguration.TimetableValidationSwitches.TimetableValidationSwitches where
	arbitrary	= ProblemConfiguration.TimetableValidationSwitches.MkTimetableValidationSwitches
		<$> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary
		<*> Test.QuickCheck.arbitrary

