{-# LANGUAGE CPP #-}
{-
	Copyright (C) 2013-2016 Dr. Alistair Ward

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

	* An abstract description of the properties common to /student/s, /teacher/s or /location/s.

	* Currently just their /availability/, but in principle other common facets of these entities may be added as they're discovered.

	* The simultaneous /availability/ of largely arbitrary collections of tuples can also be determined.
-}

module WeekDaze.Data.Resource(
-- * Type-classes
	Resource(..),
-- * Types
-- ** Type-synonyms
	ResourceMap,
-- * Functions
--	findCommonAvailability,
	countDaysPerWeekAvailable,
	isAvailable,
	extractAvailableResources
) where

import qualified	Data.Foldable
import qualified	Data.Functor
import qualified	Data.Map
import qualified	Data.Set
import qualified	WeekDaze.Size			as Size
import qualified	WeekDaze.Temporal.Availability	as Temporal.Availability
import qualified	WeekDaze.Temporal.Day		as Temporal.Day

-- | Describes a finite resource, or group of finite resources.
class Resource resource where
	getAvailability :: resource -> Temporal.Availability.Availability	-- ^ The set of /day/s on which the /resource/s are all regularly /available/ to be booked.

	isAvailableOn :: Temporal.Day.Day -> resource -> Bool			-- ^ True if on the specified /day/, all /resource/s are regularly scheduled to be /available/.
	isAvailableOn day	= Temporal.Availability.isAvailableOn day . getAvailability	-- Default implementation.

-- | The number of /day/s in any week, on which all the /resource/s are simultaneously /available/.
countDaysPerWeekAvailable :: Resource resource => resource -> Size.NDays
countDaysPerWeekAvailable	= Temporal.Availability.countDaysPerWeekAvailable . getAvailability

-- | True if on any /day/, all the /resource/s are regularly scheduled to be simultaneously /available/.
isAvailable :: Resource resource => resource -> Bool
isAvailable	= not . Temporal.Availability.isUnavailable . getAvailability

-- Properties of homogeneous resources ...

-- | Returns the /day/s on which all the specified /resource/s are /available/.
findCommonAvailability :: (Data.Foldable.Foldable f, Data.Functor.Functor f, Resource resource) => f resource -> Temporal.Availability.Availability
findCommonAvailability	= Temporal.Availability.findIntersections . fmap getAvailability

instance Resource resource => Resource [resource] where
	getAvailability		= findCommonAvailability
	isAvailableOn day	= all (isAvailableOn day)

instance Resource resource => Resource (Data.Map.Map k resource) where
	getAvailability		= findCommonAvailability
	isAvailableOn day	= Data.Foldable.all (isAvailableOn day)

instance (
#if !MIN_VERSION_containers(0,5,2)
	Ord		resource,	-- Not required after "Data.Set-7.8.1".
#endif
	Resource	resource
 ) => Resource (Data.Set.Set resource) where
	getAvailability		= Temporal.Availability.findIntersections . Data.Set.map getAvailability
	isAvailableOn day	= Data.Foldable.all (isAvailableOn day)

-- Properties of heterogenous resources ...

instance (Resource a, Resource b) => Resource (a, b) where
	getAvailability (x, y)		= getAvailability x `Temporal.Availability.findIntersection` getAvailability y
	isAvailableOn day (x, y)	= all (Temporal.Availability.isAvailableOn day) [getAvailability x, getAvailability y]

instance (Resource a, Resource b, Resource c) => Resource (a, b, c) where
	getAvailability (x, y, z)	= Temporal.Availability.findIntersections [getAvailability x, getAvailability y, getAvailability z]
	isAvailableOn day (x, y, z)	= all (Temporal.Availability.isAvailableOn day) [getAvailability x, getAvailability y, getAvailability z]

-- | Describes a map indexed by unique /resource/-identifiers.
type ResourceMap resourceId resource	= Data.Map.Map resourceId resource

-- | Extract those /resource/s from the map provided, which are /available/ on the specified /day/.
extractAvailableResources :: Resource resource => Temporal.Day.Day -> ResourceMap resourceId resource -> ResourceMap resourceId resource
extractAvailableResources day	= Data.Map.filter (isAvailableOn day)

