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

 [@DESCRIPTION@]	Implements 'Test.QuickCheck.Arbitrary' & defines /QuickCheck/-properties for 'Aggregate.StudentBody.StudentBody'.
-}

module WeekDaze.Test.QuickCheck.Aggregate.StudentBody(
-- * Constants
	results
) where

import qualified	Data.Set
import qualified	Test.QuickCheck
import qualified	ToolShed.Test.ReversibleIO
import qualified	WeekDaze.Aggregate.StudentBody	as Aggregate.StudentBody

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

instance Test.QuickCheck.Arbitrary Aggregate.StudentBody.StudentBody where
	arbitrary	= Aggregate.StudentBody.mkStudentBody <$> (
		('m' :) . show <$> Test.QuickCheck.elements [0 :: Int .. 9]	-- Mnemonic.
	 ) <*> (
		Data.Set.fromList . map (('s' :) . show :: Int -> String) <$> (Test.QuickCheck.elements [1 .. 10 {-arbitrarily-}] >>= Test.QuickCheck.vector)	-- Student-Ids.
	 )

-- | The constant test-results for this data-type.
results :: IO [Test.QuickCheck.Result]
results	= sequence [
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_readPrependedWhiteSpace,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_read,
	Test.QuickCheck.quickCheckWithResult Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 256} prop_readTrailingGarbage
 ] where
	prop_readPrependedWhiteSpace :: Aggregate.StudentBody.StudentBody -> Test.QuickCheck.Property
	prop_readPrependedWhiteSpace	= Test.QuickCheck.label "prop_readPrependedWhiteSpace" . ToolShed.Test.ReversibleIO.readPrependedWhiteSpace

	prop_read :: String -> Test.QuickCheck.Property
	prop_read garbage	= Test.QuickCheck.label "prop_read" $ case (reads garbage :: [(Aggregate.StudentBody.StudentBody, String)]) of
		[_]	-> True
		_	-> True	-- Unless the read-implementation throws an exception.

	prop_readTrailingGarbage :: Aggregate.StudentBody.StudentBody -> String -> Test.QuickCheck.Property
	prop_readTrailingGarbage studentBody	= Test.QuickCheck.label "prop_readTrailingGarbage" . ToolShed.Test.ReversibleIO.readTrailingGarbage (const False) studentBody

