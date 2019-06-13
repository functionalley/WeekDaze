{-
	Copyright (C) 2015 Dr. Alistair Ward

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

	Contains utilities dependent on both the static data derived from either configuration or command-line options,
	& on the dynamic state of the current /timetable/.
-}

module WeekDaze.Dynamic.TimetableForWeekUtilities(
-- * Functions
--	getMaybeCampus,
	countInterCampusMigrations,
	extractTemporallyAdjacentCampuses
) where

import			Control.Arrow((&&&))
import			Data.Map((!))
import qualified	Data.Array.IArray
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	ToolShed.Data.Pair
import qualified	WeekDaze.Data.Group				as Data.Group
import qualified	WeekDaze.Data.HumanResource			as Data.HumanResource
import qualified	WeekDaze.Data.Location				as Data.Location
import qualified	WeekDaze.Data.Resource				as Data.Resource
import qualified	WeekDaze.Model.Lesson				as Model.Lesson
import qualified	WeekDaze.Model.TimetableForDay			as Model.TimetableForDay
import qualified	WeekDaze.Model.TimetableForWeek			as Model.TimetableForWeek
import qualified	WeekDaze.ProblemConfiguration.ProblemParameters	as ProblemConfiguration.ProblemParameters
import qualified	WeekDaze.Size					as Size
import qualified	WeekDaze.Temporal.Day				as Temporal.Day
import qualified	WeekDaze.Temporal.Time				as Temporal.Time

-- | Extracts the /campus/ of any /location/, for either the specified /lesson/ or any /meeting/ at the specified /time/.
getMaybeCampus :: (
	Data.HumanResource.HumanResource	resource,
	Ord					locationId,
	Ord					timeslotId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> resource
	-> (resourceIds -> locationId)	-- ^ Accessor.
	-> Temporal.Day.Day
	-> Model.TimetableForDay.Association timeslotId resourceIds level
	-> Maybe campus
getMaybeCampus problemParameters resource getLocationId day (timeslotId, maybeLesson)	= (
	Data.Location.getCampus . (locationCatalogue !)
 ) `fmap` Data.Maybe.maybe (
	Data.Maybe.listToMaybe . Data.Maybe.mapMaybe Data.Group.getMaybeLocationId . filter (
		Data.Set.member (Temporal.Time.mkTime day timeslotId) . Data.Group.getMeetingTimes
	) . map (
		groupCatalogue !
	) . Data.Set.toList $ Data.HumanResource.getGroupMembership resource
 ) (
	Just . getLocationId . Model.Lesson.getResourceIds
 ) maybeLesson where
	(locationCatalogue, groupCatalogue)	= ProblemConfiguration.ProblemParameters.getLocationCatalogue &&& ProblemConfiguration.ProblemParameters.getGroupCatalogue $ problemParameters

{- |
	* Counts the total number of times the specified /human-resource/ changes /campus/ within a day, during the /week/.

	* Accounts for /meeting/s which designate a /location/.

	* CAVEAT: a /campus/-migration which occurs over a /day/-boundary isn't counted since the proximity of the /human-resource/'s home to any /campus/ is unknown.

	* If a /day/ is either completely unbooked the number of migrations is zero, since the /human-resource/ can stay at home.
	If a /day/ is entirely booked within one /campus/, then the number of migrations is also counted as zero,
	since though each /human-resource/ must at migrate to and from home at either end of the /day/, their home may be on-/campus/.
	As an aside, this philosophy prevents an unbooked /day/ being considered superior to a /day/ booked entirely within one /campus/, though that shouldn't be considered here.
-}
countInterCampusMigrations :: (
	Data.Array.IArray.Ix			timeslotId,
	Data.HumanResource.HumanResource	resource,
	Data.Resource.Resource			resource,
	Eq					campus,
	Ord					locationId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> resource			-- ^ Actually a /human-resource/; either a /student/ or a /teacher/.
	-> (resourceIds -> locationId)	-- ^ Accessor.
	-> Model.TimetableForWeek.TimetableForWeek timeslotId resourceIds level
	-> Size.NTimeslots
countInterCampusMigrations problemParameters resource getLocationId	= foldr (
	\(day, timetableForDay)	-> if Data.Resource.isAvailableOn day resource	-- Only required for efficiency.
		then (
			(
				max 0 {-an unbooked day requires zero migration-} . pred {-a day spent on just one campus requires zero migration-} . length . Data.List.group . Data.Maybe.mapMaybe (
					getMaybeCampus problemParameters resource getLocationId day
				) $ Data.Array.IArray.assocs timetableForDay
			) +
		) -- Section.
		else id	-- The resource is unavailable today.
 ) 0 . Data.Array.IArray.assocs

{- |
	* Extracts the /campus/ of any /location/, for either the /booking/ or /meeting/, which is nearest before & after the specified /time/.

	* CAVEAT: the information about whether the /location/ was required for the /meeting/ of a /group/ or for a /lesson/, is abandoned.
-}
extractTemporallyAdjacentCampuses :: (
	Data.Array.IArray.Ix			timeslotId,
	Data.HumanResource.HumanResource	resource,
	Ord					locationId
 )
	=> ProblemConfiguration.ProblemParameters.ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> resource			-- ^ Actually a /human-resource/; either a /student/ or a /teacher/.
	-> (resourceIds -> locationId)	-- ^ Accessor.
	-> Temporal.Time.Time timeslotId
	-> Model.TimetableForWeek.TimetableForWeek timeslotId resourceIds level
	-> (Maybe campus, Maybe campus) -- Pair.
extractTemporallyAdjacentCampuses problemParameters resource getLocationId time	= ToolShed.Data.Pair.mirror (
	Data.Maybe.listToMaybe . Data.Maybe.mapMaybe (
		getMaybeCampus problemParameters resource getLocationId day
	)
 ) . Model.TimetableForDay.bisectAt timeslotId . (
	Data.Array.IArray.! day
 ) where
	(day, timeslotId)	= Temporal.Time.getDay &&& Temporal.Time.getTimeslotId $ time	-- Deconstruct.

