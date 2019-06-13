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

 [@DESCRIPTION@]	Defines a meeting of a /group/, suitable for any /time-slot/ in the generic /timetable/.

-}

module WeekDaze.Model.Meeting(
-- * Types
-- ** Type-synonyms
	MeetingsByTime,
--	MeetingsByTimeMutator,
-- ** Data-types
	Meeting(
--		MkMeeting,
		getMaybeLocationId,
		getStudentClass,
		getTeacherIds,
		getGroupId
	),
-- * Constants
--	cssIdentifier,
--	resourceIdsCSSIdentifier,
-- * Functions
	deleteLocationId,
	deleteStudentBody,
	deleteTeacherId,
-- ** Constructor
	mkMeeting
) where

import			Control.Arrow((&&&))
import qualified	Data.List
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Aggregate.StudentBody	as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentClass	as Aggregate.StudentClass
import qualified	WeekDaze.Data.Group		as Data.Group
import qualified	WeekDaze.Data.HumanResource	as Data.HumanResource
import qualified	WeekDaze.Temporal.Time		as Temporal.Time
import qualified	WeekDaze.Text.CSS		as Text.CSS
import			Text.XHtml.Strict((+++), (<<), (!))

-- | Used to qualify output.
cssIdentifier :: Text.CSS.CSSIdentifier
cssIdentifier			= "meeting"

-- | Used to qualify output.
resourceIdsCSSIdentifier :: Text.CSS.CSSIdentifier
resourceIdsCSSIdentifier	= "resourceIds"	-- CAVEAT: coordinate with 'Model.Lesson.resourceIdsCSSIdentifier'.

{- |
	* Defines a potential /booking/ of the /resource/s required for a /group/ to meet.

	* It doesn't directly specify the /time-slot/ at which it's to be booked, because that's defined by it's position within a /timetable/.
-}
data Meeting locationId teacherId = MkMeeting {
	getGroupId		:: Data.Group.Id,			-- ^ The name of the /group/ which is meeting.
	getMaybeLocationId	:: Maybe locationId,			-- ^ The optional /location/ at which the meeting will be held. Whilst this could be found by looking-up the /group/, containing it permits implementation of 'Text.XHtml.Strict.HTML'.
	getStudentClass		:: Aggregate.StudentClass.StudentClass,	-- ^ The /student-bodies/ that will be attending.
	getTeacherIds		:: Data.Set.Set teacherId		-- ^ The /teacher/s who will be attending.
} deriving (Eq, Ord, Show)

instance (
	Text.XHtml.Strict.HTML	locationId,
	Text.XHtml.Strict.HTML	teacherId
 ) => Text.XHtml.Strict.HTML (Meeting locationId teacherId) where
	toHtml MkMeeting {
		getGroupId		= groupId,
		getMaybeLocationId	= maybeLocationId,
		getStudentClass		= studentClass,
		getTeacherIds		= teacherIds
	} = Text.XHtml.Strict.thediv ! [
		Text.XHtml.Strict.theclass cssIdentifier,
		Text.XHtml.Strict.title cssIdentifier
	 ] << (
		Text.XHtml.Strict.thediv ! [
			Text.XHtml.Strict.theclass Data.Group.groupIdTag,
			Text.XHtml.Strict.title Data.Group.groupIdTag
		] << (groupId +++ '.') +++ Text.XHtml.Strict.unordList (
			Data.Maybe.catMaybes [
				fmap terminate maybeLocationId,
				if Data.Set.null studentClass
					then Nothing
					else Just . terminate . Data.List.intersperse separator . map Text.XHtml.Strict.toHtml $ Data.Set.toList studentClass,
				if Data.Set.null teacherIds
					then Nothing
					else Just . terminate . Data.List.intersperse separator . map Text.XHtml.Strict.toHtml $ Data.Set.toList teacherIds
			]
		) ! [
			Text.XHtml.Strict.theclass resourceIdsCSSIdentifier,
			Text.XHtml.Strict.title resourceIdsCSSIdentifier
		]
	 ) where
		separator	= Text.XHtml.Strict.toHtml ", "	-- There may be many list-elements, so don't use a non-breaking space.

		terminate :: Text.XHtml.Strict.HTML html => html -> Text.XHtml.Strict.Html
		terminate	= (+++ '.')

instance (Show locationId, Show teacherId) => ToolShed.SelfValidate.SelfValidator (Meeting locationId teacherId) where
	getErrors meeting	= ToolShed.SelfValidate.extractErrors [
		(
			uncurry (&&) $ (Data.Set.null . getStudentClass &&& Data.Set.null . getTeacherIds) meeting,	-- CAVEAT: arguably two is the minimum.
			"a meeting can't have an empty " ++ show Data.HumanResource.groupMembershipTag ++ "; " ++ show meeting
		) -- Pair.
	 ]

-- | Smart constructor.
mkMeeting
	:: (Show locationId, Show teacherId)
	=> Data.Group.Id			-- ^ The identifier of the /group/ which it meeting.
	-> Maybe locationId			-- ^ The optional /location/ of the rendezvous.
	-> Aggregate.StudentClass.StudentClass	-- ^ The potentially empty set of /student/-members of the /group/.
	-> Data.Set.Set teacherId		-- ^ The potentially empty set of /teacher/-members of the /group/.
	-> Meeting locationId teacherId
mkMeeting groupId maybeLocationId studentClass teacherIds
	| ToolShed.SelfValidate.isValid meeting	= meeting
	| otherwise				= error $ "WeekDaze.Model.Meeting.mkMeeting:\t" ++ ToolShed.SelfValidate.getFirstError meeting ++ "."
	where
		meeting	= MkMeeting groupId maybeLocationId studentClass teacherIds

-- | A map indexed by /time/, of sets of /meeting/s.
type MeetingsByTime timeslotId locationId teacherId	= Data.Map.Map (Temporal.Time.Time timeslotId) (Data.Set.Set (Meeting locationId teacherId))

-- | A function used to mutate a 'MeetingsByTime'.
type MeetingsByTimeMutator timeslotId locationId teacherId	= MeetingsByTime timeslotId locationId teacherId -> MeetingsByTime timeslotId locationId teacherId

-- | Delete the specified /location-Id/ from all specified meetings.
deleteLocationId :: (
	Ord	locationId,
	Ord	teacherId
 ) => locationId -> MeetingsByTimeMutator timeslotId locationId teacherId
deleteLocationId locationId	= Data.Map.map (
	Data.Set.map (
		\meeting -> meeting {
			getMaybeLocationId	= Nothing	-- Remove the locationId.
		}
	)
 ) . Data.Map.filter (
	not . Data.Set.null	-- Leave only times at which there's a relevant meeting.
 ) . Data.Map.map (
	Data.Set.filter (
		(== Just locationId) . getMaybeLocationId	-- Leave only meetings relevant to this location.
	)
 )

-- | Delete the specified /student-body/ from the members of all specified meetings.
deleteStudentBody :: (
	Ord	locationId,
	Ord	teacherId
 ) => Aggregate.StudentBody.StudentBody -> MeetingsByTimeMutator timeslotId locationId teacherId
deleteStudentBody studentBody	= Data.Map.map (
	Data.Set.map (
		\meeting -> meeting {
			getStudentClass	= Data.Set.delete studentBody $ getStudentClass meeting
		}
	)
 ) . Data.Map.filter (
	not . Data.Set.null	-- Leave only times at which there's a relevant meeting.
 ) . Data.Map.map (
	Data.Set.filter (
		Data.Set.member studentBody . getStudentClass	-- Leave only meetings relevant to this student.
	)
 )

-- | Delete the specified /teacher-Id/ from the members of all specified meetings.
deleteTeacherId :: (
	Ord	locationId,
	Ord	teacherId
 ) => teacherId -> MeetingsByTimeMutator timeslotId locationId teacherId
deleteTeacherId teacherId	= Data.Map.map (
	Data.Set.map (
		\meeting -> meeting {
			getTeacherIds	= Data.Set.delete teacherId $ getTeacherIds meeting
		}
	)
 ) . Data.Map.filter (
	not . Data.Set.null	-- Leave only times at which there's a relevant meeting.
 ) . Data.Map.map (
	Data.Set.filter (
		Data.Set.member teacherId . getTeacherIds	-- Leave only meetings relevant to this teacher.
	)
 )

