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

 [@DESCRIPTION@]	Defines a toggle-switch used to enable/disable a validity-check.
-}

module WeekDaze.ProblemConfiguration.ValidationSwitch(
-- * Type-classes
	ValidationSwitchSet(..),
-- * Types
-- ** Type-synonyms
	Check,
-- * Constants
	defaultCheck
) where

-- | Defines whether to check a specific feature.
type Check	= Bool

-- | Default value for a 'Check'.
defaultCheck :: Check
defaultCheck	= True

-- | An interface to which a set of checks may conform.
class ValidationSwitchSet a where
	areAllOff	:: a -> Bool	-- ^ True if all switches are turned-off.
