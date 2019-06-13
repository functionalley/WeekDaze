{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2013-2014 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Instance-declaration for 'Data.Set.Set', which renders the elements as a (potentially null) List.

 [@CAVEATS@]	The individual list-elements are decorated further & therefore can't be text.
-}

module WeekDaze.Enhanced.EnhancedSet() where

import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT

instance (Ord a, HXT.XmlPickler a) => HXT.XmlPickler (Data.Set.Set a) where
	xpickle	= HXT.xpWrap (
		Data.Set.fromList,	-- Construct from a List.
		Data.Set.toList		-- Deconstruct to a List.
	 ) $ HXT.xpList {-can be null-} HXT.xpickle

