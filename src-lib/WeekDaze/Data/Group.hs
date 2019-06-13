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

	* Describes groups of /student/s or /teacher/s.

	* Any number of different /group/s can exist.

	* /Student/s & /teacher/s can belong to any number of /group/s, & any /group/ can include both /student/s & /teacher/s.

	* This concept exists because of the probable requirement to book staff-meetings interleaved with the booking of normal classes;
	the requirement for /student/-groups isn't so clear, though perhaps Muslim student may need to pray at certain times within the normal working day.

	* Practice for a drama or choir, lunch or morning-assembly, might be other examples of /group-meeting/s,
	because in contrast to a normal /lesson/, more than one /teacher/ might be required to attend.
-}

module WeekDaze.Data.Group(
-- * Types
-- ** Type-synonyms
	Id,
	Membership,
-- ** Data-types
	Profile(
--		MkProfile,
		getMeetingTimes,
		getMaybeLocationId,
		getMandatesAttendance
	),
-- * Constants
--	tag,
	groupIdTag,
	memberTag,
	mandatesAttendanceTag,
	meetingTimesTag,
--	defaultMeetingTimes,
	defaultMandatesAttendance,
-- * Functions
	countNTimeslotsPerWeek,
-- ** Constructor
	mkProfile,
-- ** Predicates
--	hasLocation
) where

import qualified	Control.DeepSeq
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	Text.XML.HXT.DOM.Util
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Size			as Size
import qualified	WeekDaze.Temporal.Time		as Temporal.Time
import			WeekDaze.Enhanced.EnhancedBool()

-- | Used to qualify XML.
tag :: String
tag			= "groupProfile"

-- | Used to qualify CSS, SQL & XML.
groupIdTag :: String
groupIdTag		= "groupId"

-- | Used to qualify XML.
memberTag :: String
memberTag		= "member"

-- | Used to qualify SQL & XML.
mandatesAttendanceTag :: String
mandatesAttendanceTag	= "mandatesAttendance"

-- | Used to qualify XML.
meetingTimesTag :: String
meetingTimesTag		= "meetingTimes"

-- | The default value for 'getMeetingTimes'.
defaultMeetingTimes :: Temporal.Time.TimeSet timeslotId
defaultMeetingTimes		= Data.Set.empty

-- | The default value for 'getMandatesAttendance'.
defaultMandatesAttendance :: Bool
defaultMandatesAttendance	= False

-- | Names a /group/ to which /human-resource/ may belong.
type Id	= String

-- | Aggregates the attributes of a /group/.
data Profile timeslotId locationId	= MkProfile {
	getMeetingTimes		:: Temporal.Time.TimeSet timeslotId,	-- ^ The list of meeting-times required of members of this group.
	getMaybeLocationId	:: Maybe locationId,			-- ^ The optional (it might be a teleconference, or there might currently be no meeting-times) /location/ at which all meetings will be held.
	getMandatesAttendance	:: Bool					-- ^ Whether each group-member is required to attend all meetings.
} deriving (Eq, Read, Show)

instance ToolShed.SelfValidate.SelfValidator (Profile timeslotId locationId) where
	getErrors _	= []

instance (
	HXT.XmlPickler	locationId,
	HXT.XmlPickler	timeslotId,
	Ord		timeslotId
 ) => HXT.XmlPickler (Profile timeslotId locationId) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		Text.XML.HXT.DOM.Util.uncurry3 mkProfile,	-- Construct from a Triple.
		\MkProfile {
			getMeetingTimes		= meetingTimes,
			getMaybeLocationId	= maybeLocationId,
			getMandatesAttendance	= mandatesAttendance
		} -> (
			meetingTimes,
			maybeLocationId,
			mandatesAttendance
		) -- Deconstruct to a Triple.
	 ) $ HXT.xpTriple (
		HXT.xpDefault defaultMeetingTimes . HXT.xpElem meetingTimesTag . HXT.xpWrap (
			Data.Set.fromList,	-- Construct from a List.
			Data.Set.toList		-- Deconstruct into a List.
		) $ HXT.xpList1 {-the default is null-} HXT.xpickle
	 ) (
		HXT.xpOption HXT.xpickle	-- maybeLocationId.
	 ) (
		defaultMandatesAttendance `HXT.xpDefault` HXT.xpAttr mandatesAttendanceTag HXT.xpickle {-Bool-}
	 )

instance (
	Control.DeepSeq.NFData	locationId,
	Control.DeepSeq.NFData	timeslotId
 ) => Control.DeepSeq.NFData (Profile timeslotId locationId) where
	rnf (MkProfile x0 x1 x2)	= Control.DeepSeq.rnf (x0, x1, x2)

-- | Smart constructor.
mkProfile :: Temporal.Time.TimeSet timeslotId -> Maybe locationId -> Bool -> Profile timeslotId locationId
mkProfile meetingTimes locationId mandatesAttendance
	| ToolShed.SelfValidate.isValid profile	= profile
	| otherwise				= error $ "WeekDaze.Data.Group.mkProfile:\t" ++ ToolShed.SelfValidate.getFirstError profile ++ "."
	where
		profile	= MkProfile meetingTimes locationId mandatesAttendance

{- |
	* Counts the number of /time-slot/s per week required for /meeting/s of this /group/.

	* CAVEAT: this total is independent of the individual member, but if attendance isn't mandated, it should be reduced according to individual availability.
-}
countNTimeslotsPerWeek :: Profile timeslotId locationId -> Size.NTimeslots
countNTimeslotsPerWeek	= Data.Set.size . getMeetingTimes

-- | Whether the /meeting/s of the specified /group/ have a designated /location/.
hasLocation :: Profile timeslotId locationId -> Bool
hasLocation	= Data.Maybe.isJust . getMaybeLocationId

-- | The /group/s to which a /human-resource/ belongs.
type Membership	= Data.Set.Set Id

