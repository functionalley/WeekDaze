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

 [@DESCRIPTION@]

	* Describes an aspect of either a /teacher/ or /student/.

	* These /resource/s have a requirement to devote a portion of their week to teaching,
	whereas a /location/ doesn't care what it's being used for.

	* They can also be members of heterogenous groups of other /human-resources/.
-}

module WeekDaze.Data.HumanResource(
-- * Type-classes
	HumanResource(..),
-- * Constants
	groupMembershipTag,
	defaultGroupMembership,
-- * Functions
	calculateNTimeslotsPerWeekAvailable,
	extractDistinctGroupMembership,
	extractGroupMembersOf,
	extractCombinedGroupMembership,
-- ** Predicates
	hasFreePeriodPreference
) where

import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	WeekDaze.Data.Group			as Data.Group
import qualified	WeekDaze.Data.Resource			as Data.Resource
import qualified	WeekDaze.Size				as Size
import qualified	WeekDaze.Temporal.FreePeriodPreference	as Temporal.FreePeriodPreference

-- | Used to qualify XML.
groupMembershipTag :: String
groupMembershipTag	= "groupMembership"

-- | The default value for 'getGroupMembership'.
defaultGroupMembership :: Data.Group.Membership
defaultGroupMembership	= Data.Set.empty

-- | Describes a /resource/, which is also a person.
class HumanResource h where
	getNTimeslotsPerWeekOfTeaching
		:: Size.NTimeslots										-- ^ The number of /time-slot/s per /day/.
		-> h
		-> Size.NTimeslots										-- ^ The number of those /time-slot/s when the person is available, devoted to teaching or being taught.

	getNTimeslotsPerWeekOfNonTeaching
		:: Size.NTimeslots										-- ^ The number of /time-slot/s per /day/.
		-> h
		-> Size.NTimeslots										-- ^ The number of those /time-slot/s when the person is available, devoted to any activity other than teaching; e.g. lunch, meetings, administration, free-study.

	getGroupMembership		:: h -> Data.Group.Membership						-- ^ The /group/s to which this /human-resource/ belongs.

	getMaybeFreePeriodPreference	:: h -> Maybe Temporal.FreePeriodPreference.FreePeriodPreference	-- ^ Any preference for the position within each /day/, of unallocated /time-slot/s.

-- | The number of /time-slot/s in each week, that this /resource/ is regularly scheduled to be /available/.
calculateNTimeslotsPerWeekAvailable
	:: Data.Resource.Resource resource
	=> Size.NTimeslots
	-> resource
	-> Size.NTimeslots
calculateNTimeslotsPerWeekAvailable nTimeslotsPerDay	= (nTimeslotsPerDay *) . Data.Resource.countDaysPerWeekAvailable

-- | Finds the set of /group/s, of which at least one /human-resource/ claims membership.
extractDistinctGroupMembership :: HumanResource humanResource => Data.Resource.ResourceMap humanResourceId humanResource -> Data.Group.Membership
extractDistinctGroupMembership	= Data.Map.foldr (Data.Set.union . getGroupMembership) Data.Set.empty

-- | Identify those /human-resource/s, in the specified /resource/-map, who belong to the specified /group/.
extractGroupMembersOf
	:: HumanResource humanResource
	=> Data.Group.Id
	-> Data.Resource.ResourceMap humanResourceId humanResource
	-> Data.Resource.ResourceMap humanResourceId humanResource
extractGroupMembersOf groupId	= Data.Map.filter (Data.Set.member groupId . getGroupMembership)

-- | Get the union of all group-memberships.
extractCombinedGroupMembership
	:: HumanResource humanResource
	=> Data.Resource.ResourceMap humanResourceId humanResource
	-> Data.Group.Membership
extractCombinedGroupMembership	= Data.Set.unions . map getGroupMembership . Data.Map.elems

-- | Whether the /human-resource/ has specified a preference for the position, within each /day/ on which they're /available/, of free /time-slot/s.
hasFreePeriodPreference :: HumanResource humanResource => humanResource -> Bool
hasFreePeriodPreference	= Data.Maybe.isJust . getMaybeFreePeriodPreference

