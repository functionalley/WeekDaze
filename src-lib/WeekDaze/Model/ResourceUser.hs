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

 [@DESCRIPTION@]	Describes the interface to an abstract user of /resource/s.

-}

module WeekDaze.Model.ResourceUser(
-- * Type-classes
	ResourceUser(..),
-- * Functions
-- ** Predicates
	areMergeableWith,
	areMutuallyMergeable
) where

import			Control.Arrow((&&&))
import qualified	Data.Foldable
import qualified	Data.Set
import qualified	WeekDaze.Model.Lesson	as Model.Lesson

-- | An interface to which users of /resource/s can conform.
class ResourceUser resourceUser where
	areIndependent	:: resourceUser -> resourceUser -> Bool	-- ^ Whether the /resource/s used by the first instance, are independent of those used by the second.

instance Ord resource => ResourceUser (Data.Set.Set resource) where
	areIndependent s	= Data.Set.null . Data.Set.intersection s	-- To support student-classes.

{- |
	* Check whether the specified /lesson/ is compatible with the incumbent collection of synchronous /lesson/s.

	* Each of the incumbent /lesson/s must either be identical to the proposed /lesson/, or use completely different /resource/s.

	* This property allows the specified /lesson/ to be booked into a /timetable/ at the same /time/.

	* CAVEAT: performance-hotspot.
-}
areMergeableWith :: (
	Data.Foldable.Foldable	foldable,
	Eq			resourceIds,
	Eq			level,
	ResourceUser		resourceIds
 )
	=> Model.Lesson.Lesson resourceIds level		-- ^ The proposed /lesson/.
	-> foldable (Model.Lesson.Lesson resourceIds level)	-- ^ The /lesson/s already booked in the /timetable/, at the proposed /time/.
	-> Bool
areMergeableWith lesson	= Data.Foldable.all (
	uncurry (||) . (
		(== lesson) &&& areIndependent (Model.Lesson.getResourceIds lesson) . Model.Lesson.getResourceIds
	)
 )

{- |
	* 'True' if all the listed /lesson/s are identical, or none of their /resource-Id/s are common.

	* This property allows them to co-exist, when booked into the weekly /timetable/s of distinct /observer/s, but at the same /time/.

	* CAVEAT: is pretty inefficient; all we're really want to do is 'nub' the list of /lesson/s, then check that neither of the two /resource/s per /lesson/, is used more than once,
	it's just that we can't access the two /resource/s, because they're behind the class-interface.
-}
areMutuallyMergeable :: (
	Eq		resourceIds,
	Eq		level,
	ResourceUser	resourceIds
 ) => [Model.Lesson.Lesson resourceIds level] -> Bool
areMutuallyMergeable []		= True
areMutuallyMergeable (x : xs)	= areMergeableWith x xs && areMutuallyMergeable xs {-recurse-}

