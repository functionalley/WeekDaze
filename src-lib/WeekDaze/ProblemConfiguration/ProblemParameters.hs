{-# LANGUAGE CPP, FlexibleContexts #-}
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

 [@DESCRIPTION@]	Encapsulates the data which defines the problem, rather than the solution-mechanism, or its presentation.
-}

module WeekDaze.ProblemConfiguration.ProblemParameters(
-- * Types
-- ** Data-types
	ProblemParameters(..),
-- * Constants
--	tag,
	timeslotIdBoundsTag,
--	groupTag,
--	locationTag,
--	studentBodyToProfileAssociationTag,
--	teacherTag,
--	minTag,
--	maxTag,
--	defaultGroupCatalogue,
-- * Functions
	calculateNTimeslotsPerDay,
--	calculateStudentWorkloadBounds,
	extractDistinctGroupMembership,
	findExcessTotalWorkloadByStudentBody,
	findHumanResourceIdsByGroupId,
--	findHumanResourcesOverloadedWithMeetings,
--	findSynchronousSeparateMandatoryMeetingsByTimeByHumanResourceId,
--	findSynchronousMeetingsByTimeByHumanResourceId,
	findSynchronousMeetingsByTimeByStudentBodyMnemonic,
	findSynchronousMeetingsByTimeByTeacherId,
--	findUnavailableDaysByGroupIdByHumanResourceId,
--	countNTimeslotsPerWeekForMeetings,
	reduceStudentBodyRegister,
	removeRedundantCourses,
	removePointlessGroups,
	removeUnsubscribedGroups,
	mergeConstraintsOnSynchronisedCourses,
	disableAnyValidationInappropriateForTemporaryStudentBodyMerger,
-- ** Predicates
	hasAnyFreePeriodPreference,
	hasVariousMinimumConsecutiveLessons
) where

import			Control.Arrow((&&&), (***))
import			Data.Map((!))
import			Data.Set((\\))
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Monad.Writer
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.List.Extra
import qualified	Data.Map
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	Data.Set
import qualified	Factory.Data.Interval
import qualified	Text.XML.HXT.Arrow.Pickle					as HXT
import qualified	ToolShed.Data.Foldable
import qualified	ToolShed.Data.List
import qualified	ToolShed.Data.Pair
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Aggregate.GroupCatalogue				as Aggregate.GroupCatalogue
import qualified	WeekDaze.Aggregate.LocationCatalogue				as Aggregate.LocationCatalogue
import qualified	WeekDaze.Aggregate.StudentBody					as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentBodyRegister				as Aggregate.StudentBodyRegister
import qualified	WeekDaze.Aggregate.StudentClass					as Aggregate.StudentClass
import qualified	WeekDaze.Aggregate.TeacherRegister				as Aggregate.TeacherRegister
import qualified	WeekDaze.Configuration						as Configuration
import qualified	WeekDaze.Data.Course						as Data.Course
import qualified	WeekDaze.Data.Group						as Data.Group
import qualified	WeekDaze.Data.HumanResource					as Data.HumanResource
import qualified	WeekDaze.Data.Location						as Data.Location
import qualified	WeekDaze.Data.Requirements					as Data.Requirements
import qualified	WeekDaze.Data.Resource						as Data.Resource
import qualified	WeekDaze.Data.Student						as Data.Student
import qualified	WeekDaze.Data.Subject						as Data.Subject
import qualified	WeekDaze.Data.Teacher						as Data.Teacher
import qualified	WeekDaze.ProblemConfiguration.ProblemValidationSwitches		as ProblemConfiguration.ProblemValidationSwitches
import qualified	WeekDaze.ProblemConfiguration.TimetableValidationSwitches	as ProblemConfiguration.TimetableValidationSwitches
import qualified	WeekDaze.ProblemConfiguration.ValidationSwitch			as ProblemConfiguration.ValidationSwitch
import qualified	WeekDaze.Size							as Size
import qualified	WeekDaze.Temporal.Availability					as Temporal.Availability
import qualified	WeekDaze.Temporal.Day						as Temporal.Day
import qualified	WeekDaze.Temporal.FreePeriodPreference				as Temporal.FreePeriodPreference
import qualified	WeekDaze.Temporal.Time						as Temporal.Time
import qualified	WeekDaze.Temporal.TimeslotRequest				as Temporal.TimeslotRequest
import qualified	WeekDaze.Temporal.Workload					as Temporal.Workload
import			WeekDaze.Temporal.Workload((<+>))

#ifdef USE_HDBC
#ifdef QUERY_DB_CONCURRENTLY
import qualified	Control.Concurrent
#endif
import qualified	Control.Monad
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector					as Database.Selector

instance (
#ifdef QUERY_DB_CONCURRENTLY
	Control.DeepSeq.NFData			campus,
	Control.DeepSeq.NFData			level,
	Control.DeepSeq.NFData			locationId,
	Control.DeepSeq.NFData			stream,
	Control.DeepSeq.NFData			synchronisationId,
	Control.DeepSeq.NFData			teacherId,
	Control.DeepSeq.NFData			teachingRatio,
	Control.DeepSeq.NFData			timeslotId,
#endif
	Data.Convertible.Convertible		Database.HDBC.SqlValue campus,			-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue level,			-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue locationId,		-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue stream,			-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue synchronisationId,	-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue teacherId,		-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue teachingRatio,		-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue timeslotId,		-- Flexible context.
	Data.Default.Default			campus,
	Data.Default.Default			stream,
	Data.Typeable.Typeable			teachingRatio,
	Ord					level,
	Ord					locationId,
	Ord					synchronisationId,
	Ord					teacherId,
	Ord					timeslotId,
	RealFrac				teachingRatio,
	Show					campus,
	Show					level,
	Show					locationId,
	Show					synchronisationId,
	Show					timeslotId
 ) => Database.Selector.Selector (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId) where
	fromDatabase connection	projectIdSql	= let
		timeslotIdBoundsTableName :: Database.Selector.TableName
		timeslotIdBoundsTableName	= Database.Selector.tablePrefix ++ timeslotIdBoundsTag
	 in do
		(problemValidationSwitches, timetableValidationSwitches)
#ifdef QUERY_DB_CONCURRENTLY
			<- Database.Selector.fromDatabaseConcurrently connection projectIdSql
#else
			<- Database.Selector.fromDatabase connection projectIdSql
#endif
		timeslotIdBoundsRows	<- map (
			map $ Data.Maybe.fromMaybe (
				error $ "WeekDaze.ProblemConfiguration.ProblemParameters.fromDatabase:\tnull " ++ show minTag ++ " or " ++ show maxTag ++ "."
			) . either (
				error . showString "WeekDaze.ProblemConfiguration.ProblemParameters.fromDatabase:\tfailed to parse the value for a timeslotId read from the database; " . show
			) id . Database.HDBC.safeFromSql
		 ) `fmap` Database.Selector.select connection [minTag, maxTag] [timeslotIdBoundsTableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

#ifdef QUERY_DB_CONCURRENTLY
		l	<- Control.Concurrent.newEmptyMVar
		t	<- Control.Concurrent.newEmptyMVar
		s	<- Control.Concurrent.newEmptyMVar
		g	<- Control.Concurrent.newEmptyMVar

		Database.Selector.queryConcurrently Aggregate.LocationCatalogue.fromDatabase connection projectIdSql l
		Database.Selector.queryConcurrently Aggregate.TeacherRegister.fromDatabase connection projectIdSql t
		Database.Selector.queryConcurrently Aggregate.StudentBodyRegister.fromDatabase connection projectIdSql s
		Database.Selector.queryConcurrently Aggregate.GroupCatalogue.fromDatabase connection projectIdSql g

		locationCatalogue	<- Control.Concurrent.takeMVar l
		teacherRegister		<- Control.Concurrent.takeMVar t
		studentBodyRegister	<- Control.Concurrent.takeMVar s
		groupCatalogue		<- Control.Concurrent.takeMVar g
#else
		locationCatalogue	<- Aggregate.LocationCatalogue.fromDatabase connection projectIdSql
		teacherRegister		<- Aggregate.TeacherRegister.fromDatabase connection projectIdSql
		studentBodyRegister	<- Aggregate.StudentBodyRegister.fromDatabase connection projectIdSql
		groupCatalogue		<- Aggregate.GroupCatalogue.fromDatabase connection projectIdSql
#endif
		Control.Monad.when (null timeslotIdBoundsRows) . error . showString "WeekDaze.ProblemConfiguration.ProblemParameters.fromDatabase:\tzero rows were selected from table " . shows timeslotIdBoundsTableName . showString " where " . showString Database.Selector.projectIdColumnName . showChar '=' $ Database.HDBC.fromSql projectIdSql

		let timeslotIdBoundsRow	= head timeslotIdBoundsRows	-- There can't be more than one if the database is correctly constrained.

		case timeslotIdBoundsRow of
			[minTimeslotId, maxTimeslotId]	-> return {-to IO-Monad-} MkProblemParameters {
				getProblemValidationSwitches	= problemValidationSwitches,
				getTimetableValidationSwitches	= timetableValidationSwitches,
				getTimeslotIdBounds		= (minTimeslotId, maxTimeslotId),
				getLocationCatalogue		= locationCatalogue,
				getTeacherRegister		= teacherRegister,
				getStudentBodyRegister		= studentBodyRegister,
				getGroupCatalogue		= groupCatalogue
			}
			_				-> error . showString "WeekDaze.ProblemConfiguration.ProblemParameters.fromDatabase:\tunexpected number of columns=" . shows (length timeslotIdBoundsRow) . showString " in row of table " $ shows timeslotIdBoundsTableName "."
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag					= "problemParameters"

-- | Used to qualify XML.
timeslotIdBoundsTag :: String
timeslotIdBoundsTag			= "timeslotIdBounds"

-- | Used to qualify XML.
groupTag :: String
groupTag				= "group"

-- | Used to qualify XML.
locationTag :: String
locationTag				= "location"

-- | Used to qualify XML.
studentBodyToProfileAssociationTag :: String
studentBodyToProfileAssociationTag	= "studentBodyToProfileAssociation"

-- | Used to qualify XML.
teacherTag :: String
teacherTag				= "teacher"

-- | Used to qualify SQL & XML.
minTag :: String
minTag					= "min"

-- | Used to qualify SQL & XML.
maxTag :: String
maxTag					= "max"

-- | Default value for a 'getGroupCatalogue'.
defaultGroupCatalogue :: Aggregate.GroupCatalogue.GroupCatalogue timeslotId locationId
defaultGroupCatalogue	= Data.Map.empty

-- | Encapsulates the data which defines the problem.
data ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId	= MkProblemParameters {
	getProblemValidationSwitches	:: ProblemConfiguration.ProblemValidationSwitches.ProblemValidationSwitches,						-- ^ Defines toggle-switches governing validity-checks.
	getTimetableValidationSwitches	:: ProblemConfiguration.TimetableValidationSwitches.TimetableValidationSwitches,					-- ^ Defines toggle-switches governing validity-checks.
	getTimeslotIdBounds		:: Factory.Data.Interval.Interval timeslotId,										-- ^ The closed bounds, defining the /timeslot-id/s, in any /day/.
	getLocationCatalogue		:: Aggregate.LocationCatalogue.LocationCatalogue locationId campus,							-- ^ Defines the profiles of available /location/s, indexed by /location-id/.
	getTeacherRegister		:: Aggregate.TeacherRegister.TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio,	-- ^ Defines the profiles of /teacher/s, indexed by /teacher-id/.
	getStudentBodyRegister		:: Aggregate.StudentBodyRegister.StudentBodyRegister level stream teachingRatio,					-- ^ Defines the common profile of /student/s, indexed by the mnemonic of the /student-body/ of which they're all members.
	getGroupCatalogue		:: Aggregate.GroupCatalogue.GroupCatalogue timeslotId locationId							-- ^ Defines the /group/s of which any /student/ or /teacher/ may be a member.
} deriving (Eq, Show)

instance (
	Data.Default.Default	campus,
	Data.Default.Default	stream,
	Eq			campus,
	Eq			stream,
	HXT.XmlPickler		campus,
	HXT.XmlPickler		level,
	HXT.XmlPickler		locationId,
	HXT.XmlPickler		stream,
	HXT.XmlPickler		synchronisationId,
	HXT.XmlPickler		teacherId,
	HXT.XmlPickler		teachingRatio,
	HXT.XmlPickler		timeslotId,
	Ord			level,
	Ord			locationId,
	Ord			synchronisationId,
	Ord			teacherId,
	Ord			timeslotId,
	Real			teachingRatio,
	Show			campus,
	Show			level,
	Show			synchronisationId,
	Show			timeslotId
 ) => HXT.XmlPickler (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, g)	-> MkProblemParameters a b c d e f g,	-- Construct from a tuple.
		\MkProblemParameters {
			getProblemValidationSwitches	= problemValidationSwitches,
			getTimetableValidationSwitches	= timetableValidationSwitches,
			getTimeslotIdBounds		= timeslotIdBounds,
			getLocationCatalogue		= locationCatalogue,
			getTeacherRegister		= teacherRegister,
			getStudentBodyRegister		= studentBodyRegister,
			getGroupCatalogue		= groupCatalogue
		} -> (
			problemValidationSwitches,
			timetableValidationSwitches,
			timeslotIdBounds,
			locationCatalogue,
			teacherRegister,
			studentBodyRegister,
			groupCatalogue
		) -- Deconstruct to a tuple.
	 ) $ HXT.xp7Tuple (
		HXT.xpDefault Data.Default.def HXT.xpickle {-ProblemValidationSwitches-}
	 ) (
		HXT.xpDefault Data.Default.def HXT.xpickle {-TimetableValidationSwitches-}
	 ) (
		HXT.xpElem timeslotIdBoundsTag $ HXT.xpElem minTag HXT.xpickle `HXT.xpPair` HXT.xpElem maxTag HXT.xpickle
	 ) (
		HXT.xpElem Aggregate.LocationCatalogue.tag . HXT.xpWrap (
			Data.Map.fromList,	-- Construct from an association-list.
			Data.Map.toList		-- Deconstruct to an association-list.
		) . HXT.xpList1 {-can't be null-} $ HXT.xpElem locationTag HXT.xpickle {-Pair-}
	 ) (
		HXT.xpElem Aggregate.TeacherRegister.tag . HXT.xpWrap (
			Data.Map.fromList,	-- Construct from an association-list.
			Data.Map.toList		-- Deconstruct to an association-list.
		) . HXT.xpList1 {-can't be null-} $ HXT.xpElem teacherTag HXT.xpickle {-Pair-}
	 ) (
		HXT.xpElem Aggregate.StudentBodyRegister.tag . HXT.xpWrap (
			Data.Map.fromList,	-- Construct from an association-list.
			Data.Map.toList		-- Deconstruct to an association-list.
		) . HXT.xpList1 {-can't be null-} $ HXT.xpElem studentBodyToProfileAssociationTag HXT.xpickle {-Pair-}
	 ) (
		HXT.xpDefault defaultGroupCatalogue . HXT.xpElem Aggregate.GroupCatalogue.tag $ HXT.xpMap groupTag Data.Group.groupIdTag HXT.xpText {-can't be null-} HXT.xpickle {-Profile-}
	 )

instance (
	Control.DeepSeq.NFData	campus,
	Control.DeepSeq.NFData	level,
	Control.DeepSeq.NFData	locationId,
	Control.DeepSeq.NFData	stream,
	Control.DeepSeq.NFData	synchronisationId,
	Control.DeepSeq.NFData	teacherId,
	Control.DeepSeq.NFData	teachingRatio,
	Control.DeepSeq.NFData	timeslotId
 ) => Control.DeepSeq.NFData (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId) where
	rnf (MkProblemParameters x0 x1 x2 x3 x4 x5 x6)	= Control.DeepSeq.rnf (x0, x1, x2, x3, x4, x5, x6)

{- |
	* Get the length of the list defined by the specified bounds.

	* CAVEAT: this implementation accounts for the potential fence-post error when called for a closed interval of 'Integral' timeslots,
	but will result in an equivalent error if (bizarrely) called for an interval of 'Fractional' quantities.
	Regrettably, it can't be restricted to 'Integral', because it may legitimately be required to work with 'Char'.
-}
calculateNTimeslotsPerDay :: Enum timeslotId => Factory.Data.Interval.Interval timeslotId -> Size.NTimeslots
calculateNTimeslotsPerDay	= succ . uncurry (flip (-)) . (fromEnum *** fromEnum)

{- |
	* Total the workload of the core & optional /subject-requirements/, for the specified /student-profile/.

	* Since each /course/ may be offered by more than one /teacher/, the actual workload can only be constrained within bounds.

	* CAVEAT: assumes that all requested /subject/s, exist amongst the /course/s offered by /teacher/s.
-}
calculateStudentWorkloadBounds :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 )
	=> Aggregate.TeacherRegister.TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio
	-> Data.Student.Profile level stream teachingRatio
	-> Data.Requirements.Requirements Temporal.Workload.Bounds
calculateStudentWorkloadBounds teacherRegister	= ToolShed.Data.Pair.mirror (
	Data.Foldable.foldr (
		\subject -> ((Aggregate.TeacherRegister.calculateWorkloadBoundsBySubject teacherRegister ! subject) <+>)
	) Temporal.Workload.unloaded
 ) . Data.Student.getKnowledgeRequirements

-- | Finds /student-bodies/, who've requested more /subject/s (both core & optional), than can be taught by the /course/s on offer, in the time allocated to tuition.
findExcessTotalWorkloadByStudentBody :: (
	Enum		timeslotId,
	Ord		level,
	Ord		synchronisationId,
	Ord		timeslotId,
	RealFrac	teachingRatio
 ) => ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> Data.Map.Map Aggregate.StudentBody.StudentBody Size.NTimeslots
findExcessTotalWorkloadByStudentBody problemParameters	= Data.Map.filter (> 0) $ Data.Map.map (
	uncurry (-) . (
		(
			Temporal.Workload.getMinimum {-select the shortest courses offered-} . uncurry (<+>)	-- Add the workloads associated with core & optional subject-requirements.
		) . calculateStudentWorkloadBounds (
			getTeacherRegister problemParameters
		) &&& Data.HumanResource.getNTimeslotsPerWeekOfTeaching (
			calculateNTimeslotsPerDay $ getTimeslotIdBounds problemParameters
		)
	)
 ) $ getStudentBodyRegister problemParameters

-- | Finds the set of /group/s, of which a /human-resource/ claims membership.
extractDistinctGroupMembership :: RealFrac teachingRatio => ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> Data.Group.Membership
extractDistinctGroupMembership	= uncurry Data.Set.union . (Data.HumanResource.extractDistinctGroupMembership . getStudentBodyRegister &&& Data.HumanResource.extractDistinctGroupMembership . getTeacherRegister)

{- |
	* For each /human-resource/, identifies /time/s at which they're required to attend more than one meeting, in separate /location/s,
	for any of the /group/s of which they're a member.

	* Also returns the offending /group-id/s & /location-id/s.
-}
findSynchronousSeparateMandatoryMeetingsByTimeByHumanResourceId :: (
	Data.HumanResource.HumanResource	humanResource,
	Ord					locationId,
	Ord					timeslotId
 )
	=> Aggregate.GroupCatalogue.GroupCatalogue timeslotId locationId
	-> Data.Resource.ResourceMap humanResourceId humanResource
	-> Data.Map.Map humanResourceId (Data.Map.Map (Temporal.Time.Time timeslotId) [(Data.Group.Id, locationId)])
findSynchronousSeparateMandatoryMeetingsByTimeByHumanResourceId groupCatalogue	= Data.Map.filter (
	not . Data.Map.null	-- Leave only those human-resources with a scheduling-conflict.
 ) . Data.Map.map (
	Data.Map.filter (
		(> 1) . length . Data.List.Extra.groupSortOn snd {-locationId-}	-- More than one location specified for this human-resource & time.
	) . Data.Map.map (
		Data.Map.toList . Data.Map.map (
			Data.Maybe.fromJust . fst {-locationId-}	-- Reduce to 'Map groupId locationId'.
		) . Data.Map.filter (
			uncurry (&&) . Control.Arrow.first {-Maybe locationId-} Data.Maybe.isJust	-- Ensure there's both a defined location & that attendance is mandated.
		) . Data.Map.map (
			Data.Group.getMaybeLocationId &&& Data.Group.getMandatesAttendance	-- Construct 'Map groupId (maybe locationId, mandatesAttendance)'.
		) . Data.Map.fromList {-Construct 'Map groupId groupProfile'-}
	) . Data.Foldable.foldr (
		\groupId m	-> let
			groupProfile	= groupCatalogue ! groupId
		in Data.Foldable.foldr (
			Data.Map.insertWith (++) `flip` [(groupId, groupProfile)]
		) m $ Data.Group.getMeetingTimes groupProfile
	) Data.Map.empty . Data.HumanResource.getGroupMembership
 )

{- |
	* For each /human-resource/, identifies /time/s at which they have attend more than one meeting.
	for any of the /group/s of which they're a member.

	* Returns the offending /group-id/s.
-}
findSynchronousMeetingsByTimeByHumanResourceId :: (
	Data.HumanResource.HumanResource	humanResource,
	Ord					timeslotId
 )
	=> Aggregate.GroupCatalogue.GroupCatalogue timeslotId locationId
	-> Data.Resource.ResourceMap humanResourceId humanResource
	-> Data.Map.Map humanResourceId (Data.Map.Map (Temporal.Time.Time timeslotId) [Data.Group.Id])
findSynchronousMeetingsByTimeByHumanResourceId groupCatalogue	= Data.Map.filter (
	not . Data.Map.null	-- Leave only those human-resources with a scheduling-conflict.
 ) . Data.Map.map (
	Data.Map.filter (
		(> 1) . length	-- Select human-resources with more than one meeting @ this time.
	) . Data.Foldable.foldr (
		\groupId m	-> Data.Foldable.foldr (
			Data.Map.insertWith (++) `flip` [groupId]
		) m . Data.Group.getMeetingTimes $ groupCatalogue ! groupId
	) Data.Map.empty . Data.HumanResource.getGroupMembership
 )

{- |
	* For each /student-body/, identifies /time/s at which they have attend more than one meeting.
	for any of the /group/s of which they're a member.

	* Returns the offending /group-id/s.
-}
findSynchronousMeetingsByTimeByStudentBodyMnemonic :: (
	Ord		timeslotId,
	RealFrac	teachingRatio
 )
	=> ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Data.Map.Map Aggregate.StudentBody.StudentBody (Data.Map.Map (Temporal.Time.Time timeslotId) [Data.Group.Id])
findSynchronousMeetingsByTimeByStudentBodyMnemonic	= uncurry findSynchronousMeetingsByTimeByHumanResourceId . (getGroupCatalogue &&& getStudentBodyRegister)

{- |
	* For each /teacherId/, identifies /time/s at which they have attend more than one meeting.
	for any of the /group/s of which they're a member.

	* Returns the offending /group-id/s.
-}
findSynchronousMeetingsByTimeByTeacherId :: (
	Ord		timeslotId,
	RealFrac	teachingRatio
 )
	=> ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Data.Map.Map teacherId (Data.Map.Map (Temporal.Time.Time timeslotId) [Data.Group.Id])
findSynchronousMeetingsByTimeByTeacherId	= uncurry findSynchronousMeetingsByTimeByHumanResourceId . (getGroupCatalogue &&& getTeacherRegister)

-- | Identifies those /human-resource/s commited to more /meeting/s than their non-teaching time can accommodate.
findHumanResourcesOverloadedWithMeetings :: (
	Data.Resource.Resource			humanResource,
	Data.HumanResource.HumanResource	humanResource
 )
	=> Size.NTimeslots
	-> Aggregate.GroupCatalogue.GroupCatalogue timeslotId locationId
	-> Data.Resource.ResourceMap humanResourceId humanResource
	-> Data.Map.Map humanResourceId (Size.NTimeslots, Size.NTimeslots)
findHumanResourcesOverloadedWithMeetings nTimeslotsPerDay groupCatalogue	= Data.Map.filter (
	uncurry (>)
 ) . Data.Map.map (
	countNTimeslotsPerWeekForMeetings groupCatalogue &&& Data.HumanResource.getNTimeslotsPerWeekOfNonTeaching nTimeslotsPerDay
 )

{- |
	* Identifies for each /human-resource/, any /day/s on which they're unavailable for the meetings of the mandatory-attendance /group/s, of which they're members.

	* CAVEAT: only /human-resource/s unable to attend a meeting, appear in the results.
-}
findUnavailableDaysByGroupIdByHumanResourceId :: (
#if !MIN_VERSION_containers(0,5,2)
	Ord					timeslotId,	-- Not required after "Data.Set-7.8.1".
#endif
	Data.Resource.Resource			humanResource,
	Data.HumanResource.HumanResource	humanResource
 )
	=> Aggregate.GroupCatalogue.GroupCatalogue timeslotId locationId
	-> Data.Resource.ResourceMap humanResourceId humanResource
	-> Data.Map.Map humanResourceId (Data.Map.Map Data.Group.Id (Data.Set.Set Temporal.Day.Day))
findUnavailableDaysByGroupIdByHumanResourceId groupCatalogue	= Data.Map.filter (
	not . Data.Map.null	-- Purge humanResourceIds, who can attend all meetings of their mandatory-attendance groups.
 ) . Data.Map.map (
	\humanResource -> Data.Map.filter (
		not . Data.Set.null	-- Purge those groupIds, where the human-resource is fully available for all mandatory meetings.
	) . Data.Map.map (
		Data.Set.filter (
			not . (`Data.Resource.isAvailableOn` humanResource)
		) . Data.Set.map Temporal.Time.getDay . Data.Group.getMeetingTimes
	) . Data.Map.filter Data.Group.getMandatesAttendance . Data.Map.fromList . map (
		id &&& (groupCatalogue !)	-- Find the group-association.
	) . Data.Set.toList $ Data.HumanResource.getGroupMembership humanResource
 )

-- | Finds the members (/student-bodies/, /teacher/s) of each /group/.
findHumanResourceIdsByGroupId :: (Ord teacherId, RealFrac teachingRatio)
	=> ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> (Aggregate.GroupCatalogue.ResourceIdsByGroupId Aggregate.StudentBody.StudentBody, Aggregate.GroupCatalogue.ResourceIdsByGroupId teacherId)
findHumanResourceIdsByGroupId	= findHumanResourceIdsByGroupId' . getStudentBodyRegister &&& findHumanResourceIdsByGroupId' . getTeacherRegister where
	findHumanResourceIdsByGroupId' :: (Data.HumanResource.HumanResource humanResource, Ord humanResourceId) => Data.Resource.ResourceMap humanResourceId humanResource -> Aggregate.GroupCatalogue.ResourceIdsByGroupId humanResourceId
	findHumanResourceIdsByGroupId'	= Data.Map.foldrWithKey (
		\humanResourceId humanResource m	-> Data.Foldable.foldr (
			($ Data.Set.singleton humanResourceId) . Data.Map.insertWith Data.Set.union
		) m $ Data.HumanResource.getGroupMembership humanResource
	 ) Data.Map.empty

{- |
	* The number of /time-slot/s per week, which this /human-resource/ regularly requires, for the meetings of all /groups/ of which they're a member.

	* CAVEAT: days on which the /resource/ is unavailable, aren't counted on the assumption that this is because attendance for that group isn't mandatory.
-}
countNTimeslotsPerWeekForMeetings :: (
	Data.Resource.Resource			humanResource,
	Data.HumanResource.HumanResource	humanResource
 )
	=> Aggregate.GroupCatalogue.GroupCatalogue timeslotId locationId
	-> humanResource
	-> Size.NTimeslots
countNTimeslotsPerWeekForMeetings groupCatalogue humanResource	= Data.Foldable.foldr (
	(+) . Data.Set.size . Data.Set.filter (
		(`Data.Resource.isAvailableOn` humanResource) . Temporal.Time.getDay
	) . Data.Group.getMeetingTimes . (
		groupCatalogue !
	)
 ) 0 $ Data.HumanResource.getGroupMembership humanResource

instance (
	Enum		timeslotId,
	Ord		level,
	Ord		locationId,
	Ord		stream,
	Ord		synchronisationId,
	Ord		teacherId,
	Ord		timeslotId,
	RealFrac	teachingRatio,
	Show		level,
	Show		locationId,
	Show		stream,
	Show		synchronisationId,
	Show		teacherId,
	Show		teachingRatio,
	Show		timeslotId
 ) => ToolShed.SelfValidate.SelfValidator (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId) where
	getErrors problemParameters@MkProblemParameters {
		getProblemValidationSwitches	= problemValidationSwitches,
		getTimeslotIdBounds		= timeslotIdBounds,
		getLocationCatalogue		= locationCatalogue,
		getGroupCatalogue		= groupCatalogue,
		getTeacherRegister		= teacherRegister,
		getStudentBodyRegister		= studentBodyRegister
	} = ToolShed.SelfValidate.extractErrors [

-- Check 'getTimeslotIdBounds'.
		(
			ProblemConfiguration.ProblemValidationSwitches.getCheckTimeslotIdBounds problemValidationSwitches && nTimeslotsPerDay < 1,
			show timeslotIdBoundsTag ++ " are too narrow; " ++ show timeslotIdBounds
		),

-- Check that resource-maps have been defined.
		(
			ProblemConfiguration.ProblemValidationSwitches.getCheckNullLocationCatalogue problemValidationSwitches && Data.Map.null locationCatalogue,
			show ProblemConfiguration.ProblemValidationSwitches.checkNullLocationCatalogueTag ++ ": zero locations have been defined"
		), (
			ProblemConfiguration.ProblemValidationSwitches.getCheckNullTeacherRegister problemValidationSwitches && Data.Map.null teacherRegister,
			show ProblemConfiguration.ProblemValidationSwitches.checkNullTeacherRegisterTag ++ ": zero teachers have been defined"
		), (
			ProblemConfiguration.ProblemValidationSwitches.getCheckNullStudentBodyRegister problemValidationSwitches && Data.Map.null studentBodyRegister,
			show ProblemConfiguration.ProblemValidationSwitches.checkNullStudentBodyRegisterTag ++ ": zero student-bodies have been defined"
		), (
			ProblemConfiguration.ProblemValidationSwitches.getCheckNullGroupId problemValidationSwitches && Data.Map.member "" groupCatalogue,
			show ProblemConfiguration.ProblemValidationSwitches.checkNullGroupIdTag ++ ": null identifier in " ++ show Aggregate.GroupCatalogue.tag
		),

-- Check course-definitions.
		let
--			isValid :: timeslotId -> Bool
			isValid	= (`Factory.Data.Interval.elem'` timeslotIdBounds)

			coursesWithIllDefinedTimeslotRequests	= Data.Set.filter (
				\course -> case Data.Course.getTimeslotRequest course of
					Temporal.TimeslotRequest.Ideally idealTimeslotId	-> not $ isValid idealTimeslotId
					Temporal.TimeslotRequest.Specifically specifiedTimes	-> Data.Foldable.any (not . isValid . Temporal.Time.getTimeslotId) specifiedTimes
			 ) $ Aggregate.TeacherRegister.extractDistinctCourses teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckTimeslotRequests problemValidationSwitches && not (Data.Set.null coursesWithIllDefinedTimeslotRequests),
			show ProblemConfiguration.ProblemValidationSwitches.checkTimeslotRequestsTag ++ ": the timeslot-request for a course, specifies times outside permissible bounds " ++ show timeslotIdBounds ++ "; " ++ show (Data.Set.toList coursesWithIllDefinedTimeslotRequests)
		),
		let
			coursesWithExcessiveMinimumConsecutiveDays	= Data.Set.filter (
				(> nTimeslotsPerDay) . Data.Course.getMinimumConsecutiveLessons
			 ) $ Aggregate.TeacherRegister.extractDistinctCourses teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckMinimumConsecutiveLessons problemValidationSwitches && not (Data.Set.null coursesWithExcessiveMinimumConsecutiveDays),
			show ProblemConfiguration.ProblemValidationSwitches.checkMinimumConsecutiveLessonsTag ++ ": the minimum consecutive lessons for a course, can't exceed the number of time-slots per day " ++ show nTimeslotsPerDay ++ "; " ++ show (Data.Set.toList coursesWithExcessiveMinimumConsecutiveDays)
		),
		let
			excessiveLessonsPerWeekByTeacherId	= Data.Map.filter (
				not . null
			 ) $ Data.Map.map (
				\teacherProfile -> filter (
					> Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay teacherProfile
				) . map Data.Course.getRequiredLessonsPerWeek . Data.Set.toList $ Data.Teacher.getService teacherProfile
			 ) teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckRequiredLessonsPerWeek problemValidationSwitches && not (Data.Map.null excessiveLessonsPerWeekByTeacherId),
			show ProblemConfiguration.ProblemValidationSwitches.checkRequiredLessonsPerWeekTag ++ ": some teachers offer courses which individually require more lessons per week than they teach; " ++ show (Data.Map.toList excessiveLessonsPerWeekByTeacherId)	-- This could be checked in module "Data.Teacher", but the teacherId isn't available there.
		),
		let
			teachersOfferingMultipleCoursesPerSynchronisationId	= Data.Map.filter (not . null) $ Data.Map.map (
				map head . filter (
					(/= 1) . length	-- Select those teachers referencing a synchronisationId from different courses.
				) . ToolShed.Data.Foldable.gather . Data.Maybe.mapMaybe Data.Course.getMaybeSynchronisationId . Data.Set.toList . Data.Teacher.getService
			 ) teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForMultipleCoursesPerTeacherPerSynchronisationId problemValidationSwitches && not (Data.Map.null teachersOfferingMultipleCoursesPerSynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForMultipleCoursesPerTeacherPerSynchronisationIdTag ++ ": some teachers offer multiple courses with the same synchronisationId; " ++ show (Data.Map.toList teachersOfferingMultipleCoursesPerSynchronisationId)
		),
		let
			singletonTeachersBySynchronisationId	= Data.Map.map head . Data.Map.filter ((== 1) . length) $ Data.Map.map Data.Map.keys coursesByTeacherIdBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForSingletonSynchronisedCourses problemValidationSwitches && not (Data.Map.null singletonTeachersBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForSingletonSynchronisedCoursesTag ++ ": some synchronisationIds, are only referenced by the relevant courses of a single teacher; " ++ show (Data.Map.toList singletonTeachersBySynchronisationId)
		),
		let
			differentLessonsPerWeekByTeacherIdBySynchronisationId	= Data.Map.filter (
				(> 1) . Data.Set.size . Data.Set.fromList . Data.Map.elems	-- Select those synchronised courses which required more than one distinct number of lessons per week.
			 ) $ Data.Map.map (
				Data.Map.map Data.Course.getRequiredLessonsPerWeek
			 ) coursesByTeacherIdBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithDifferentLessonsPerWeek problemValidationSwitches && not (Data.Map.null differentLessonsPerWeekByTeacherIdBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForSynchronisedCoursesWithDifferentLessonsPerWeekTag ++ ": some synchronised courses, require different numbers of lessons per week; " ++ show (Data.Map.toList $ Data.Map.map Data.Map.toList differentLessonsPerWeekByTeacherIdBySynchronisationId)
		),
		let
			differentIdealTimeslotIdsBySynchronisationId	= Data.Map.filter (
				(> 1) . Data.Set.size	-- Select those synchronised courses which specify more than one distinct ideal timeslotId.
			 ) $ Data.Map.map (
				Data.Set.map Data.Maybe.fromJust . Data.Set.filter Data.Maybe.isJust . Data.Set.map (
					Temporal.TimeslotRequest.getMaybeIdealTimeslotId . Data.Course.getTimeslotRequest
				)
			 ) distinctCoursesBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithDifferentIdealTimeslots problemValidationSwitches && not (Data.Map.null differentIdealTimeslotIdsBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForSynchronisedCoursesWithDifferentIdealTimeslotsTag ++ ": the members of some sets of synchronised courses, have different ideal timeslot-identifiers; " ++ show (Data.Map.toList $ Data.Map.map Data.Set.toList differentIdealTimeslotIdsBySynchronisationId)
		),
		let
			excessSpecifiedTimesBySynchronisationId	= Data.Map.filter (
				uncurry (<)	-- Select those synchronised courses which specify too many booking-times.
			 ) $ Data.Map.map (
				(
					minimum {-requiredLessonsPerWeek-} *** Data.Set.size . Data.Set.unions {-number of distinct specified times-}
				) . unzip . Data.Set.toList . Data.Set.map (
					Data.Course.getRequiredLessonsPerWeek &&& Temporal.TimeslotRequest.getSpecifiedTimes . Data.Course.getTimeslotRequest
				) {-typically reduces the set to a singleton-}
			 ) distinctCoursesBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithExcessSpecifiedTimes problemValidationSwitches && not (Data.Map.null excessSpecifiedTimesBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForSynchronisedCoursesWithExcessSpecifiedTimesTag ++ ": some synchronised courses require fewer lessons per week, than they have specified booking-times; " ++ show (Data.Map.toList excessSpecifiedTimesBySynchronisationId)
		),
		let
			availableVsRequiredLessonsBySynchronisationId	= Data.Map.filter (
				uncurry (<)
			 ) . Data.Map.map (
				Control.Arrow.first $ (* nTimeslotsPerDay) . Temporal.Availability.countDaysPerWeekAvailable
			 ) . Data.Map.unionWith (
				\(availability, requiredLessons)	-> Temporal.Availability.findIntersection availability *** max requiredLessons {-they should all be the same-}
			 ) (
				Data.Map.mapWithKey (
					\synchronisationId -> (,) (
						Data.Resource.getAvailability $ Data.Map.filter (
							Data.Student.requiresAnySubjectBy (
								`Data.Set.member` Data.Set.map Data.Course.getSubject (
									distinctCoursesBySynchronisationId ! synchronisationId
								)
							)
						) studentBodyRegister
					) . Data.Set.findMax {-they should all be the same-} . Data.Set.map Data.Course.getRequiredLessonsPerWeek
				) distinctCoursesBySynchronisationId
			 ) $ Data.Map.map (
				Data.Resource.getAvailability . map (
					teacherRegister !
				) . Data.Map.keys {-teacherIds-} &&& Data.Foldable.maximum {-they should all be the same-} . Data.Map.map Data.Course.getRequiredLessonsPerWeek
			 ) coursesByTeacherIdBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithExcessLessonsPerWeek problemValidationSwitches && not (Data.Map.null availableVsRequiredLessonsBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForSynchronisedCoursesWithExcessLessonsPerWeekTag ++ ": the number of time-slots when the interested student-bodies & required teachers, are simultaneously available, is fewer than the required lessons of some synchronised courses; " ++ show (Data.Map.toList availableVsRequiredLessonsBySynchronisationId)
		),
		let
			unavailableSpecifiedDaysByTeacherIdBySynchronisationId	= Data.Map.filter (
				not . Data.Map.null	-- Select the synchronisationId of courses taught by one or more unavailable teachers.
			 ) $ Data.Map.map (
				Data.Map.filter (
					not . Data.Set.null	-- Select teachers who're unavailable on one or more days.
				) . Data.Map.mapWithKey (
					\teacherId -> Data.Set.filter (
						not . (
							`Data.Resource.isAvailableOn` (teacherRegister ! teacherId)
						)
					) . Data.Set.map Temporal.Time.getDay . Temporal.TimeslotRequest.getSpecifiedTimes . Data.Course.getTimeslotRequest
				)
			 ) coursesByTeacherIdBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithUnavailableSpecifiedDays problemValidationSwitches && not (Data.Map.null unavailableSpecifiedDaysByTeacherIdBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForSynchronisedCoursesWithUnavailableSpecifiedDaysTag ++ ": some synchronised courses, specify booking-times on days when not all the required teachers are available; " ++ show (
				Data.Map.toList $ Data.Map.map (
					Data.Map.toList . Data.Map.map Data.Set.toList
				) unavailableSpecifiedDaysByTeacherIdBySynchronisationId
			)
		),
		let
			unavailableStudentBodiesBySpecifiedDaysBySynchronisationId	= Data.Map.filter (
				not . Data.Map.null	-- Select the synchronisationId of courses required on one or more unavailable days.
			 ) $ Data.Map.mapWithKey (
				\synchronisationId -> Data.Map.filter (
					not . null	-- Select the days on which one or more student-bodies are unavailable.
				) . Data.Map.fromList . map (
					\day -> (
						day,
						Data.Map.keys $ Data.Map.filter (
							uncurry (&&) . (
								not . Data.Resource.isAvailableOn day &&& Data.Student.requiresAnySubjectBy (
									`Data.Set.member` Data.Set.map Data.Course.getSubject (
										distinctCoursesBySynchronisationId ! synchronisationId
									)
								)
							)
						) studentBodyRegister
					)
				) . Data.Set.toList . Data.Set.unions . map (
					Data.Set.map Temporal.Time.getDay . Temporal.TimeslotRequest.getSpecifiedTimes . Data.Course.getTimeslotRequest
				) . Data.Set.toList
			 ) distinctCoursesBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithUnavailableSpecifiedDays problemValidationSwitches && not (Data.Map.null unavailableStudentBodiesBySpecifiedDaysBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForSynchronisedCoursesWithUnavailableSpecifiedDaysTag ++ ": some synchronised courses, are requested by student-bodies who're unavailable on days when booking-times have been specified; " ++ show (
				Data.Map.toList $ Data.Map.map (
					Data.Map.toList . Data.Map.map (map Aggregate.StudentBody.getMnemonic)
				) unavailableStudentBodiesBySpecifiedDaysBySynchronisationId
			)
		),
		let
			excessTimeslotRequestsBySynchronisationId	= Data.Map.filter (
				uncurry (&&) . (not . null *** not . null)	-- Select synchronisationIds whose courses define both ideal timeslots & specify times.
			 ) $ Data.Map.map (
				(
					ToolShed.Data.List.nub' . Data.Maybe.catMaybes *** Data.Set.toList . Data.Set.unions
				) . unzip . Data.Set.toList . Data.Set.map (
					(
						Temporal.TimeslotRequest.getMaybeIdealTimeslotId &&& Temporal.TimeslotRequest.getSpecifiedTimes
					) . Data.Course.getTimeslotRequest
				)
			 ) distinctCoursesBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithExcessTimeslotRequests problemValidationSwitches && not (Data.Map.null excessTimeslotRequestsBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForSynchronisedCoursesWithExcessTimeslotRequestsTag ++ ": some synchronised courses, define both ideal timeslot-Ids & specify booking-times; " ++ show (Data.Map.toList excessTimeslotRequestsBySynchronisationId)
		),
		let
			subjectsByStudentBodyBySynchronisationId	= Data.Map.filter (
				not . Data.Map.null	-- Select synchronisationIds, where there's an issue with some student-bodies.
			 ) $ Data.Map.mapWithKey (
				\synchronisationId -> Data.Map.filter (
					(> 1) . Data.Set.size	-- Select student-bodies requiring more than one subject from this set of synchronised courses.
				) . Data.Map.map (
					Data.Set.filter (
						\subject -> not $ Data.Foldable.any (
							Data.Foldable.any (
								uncurry (&&) . (
									(/= Just synchronisationId) . Data.Course.getMaybeSynchronisationId &&& (== subject) . Data.Course.getSubject
								)
							) . Data.Teacher.getService
						) teacherRegister	-- Check for any teachers offering this subject outside of this set of synchronised courses.
					)
				) . Data.Map.unionsWith Data.Set.union . map (
					\synchronisedCourse -> Data.Map.filter (
						not . Data.Set.null	-- Select student-bodies requiring this course's subject; there shouldn't be more than one.
					) $ Data.Map.map (
						Data.Set.filter (== Data.Course.getSubject synchronisedCourse) . Data.Student.deriveAmalgamatedKnowledgeRequirement
					) studentBodyRegister
				) . Data.Set.toList
			 ) distinctCoursesBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForStudentsRequiringMultipleSynchronisedSubjects problemValidationSwitches && not (Data.Map.null subjectsByStudentBodyBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForStudentsRequiringMultipleSynchronisedSubjectsTag ++ ": some student-bodies require multiple subjects only offered within one set of synchronised courses; " ++ show (
				Data.Map.toList $ Data.Map.map (
					map (Aggregate.StudentBody.getMnemonic *** Data.Set.toList) . Data.Map.toList
				) subjectsByStudentBodyBySynchronisationId
			)
		),
		let
			duplicateSubjectsByTeacherIdBySynchronisationId	= Data.Map.filter (
				not . Data.Set.null	-- Select synchronisedCourseIds with one or more alternatives.
			 ) $ Data.Map.map (
				Data.Set.filter (
					not . Data.Map.null	-- Select those members of this synchronised course, for which there're one or more alternative teachers.
				) . Data.Set.map (
					\synchronisedCourse -> Data.Map.map Data.Set.findMin {-a teacher can't offer same subject more than once-} . Data.Map.filter (
						not . Data.Set.null	-- Select teachers offering courses in the same subject as the current synchronised one.
					) $ Data.Map.map (
						Data.Set.filter (
							== Data.Course.getSubject synchronisedCourse
						) . Data.Set.map Data.Course.getSubject . Data.Set.filter (
							/= synchronisedCourse	-- Except self. TODO should teachers be permitted to offer similar subjects with the same synchronisationId ?
						) . Data.Teacher.getService
					) teacherRegister
				) -- Map over one set of synchronised courses.
			 ) distinctCoursesBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForAlternativesToSynchronisedCourses problemValidationSwitches && not (Data.Map.null duplicateSubjectsByTeacherIdBySynchronisationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForAlternativesToSynchronisedCoursesTag ++ ": courses are offered with the same subject as synchronised ones, potentially inhibiting migration of students; " ++ show (
				Data.Map.toList $ Data.Map.map (Data.Foldable.concatMap Data.Map.toList) duplicateSubjectsByTeacherIdBySynchronisationId
			)
		),
		let
			synchronisationIdsWithoutSuitableLocations	= Data.Map.keys $ Data.Map.filter (
				null . ToolShed.Data.List.permutationsBy (
-- CAVEAT: we haven't confirmed that there's some common availability with the interested student-bodies & this specific permutation of teachers & suitable locations.
					\l r	-> let
						lookupResources	= (teacherRegister !) *** (locationCatalogue !)
					in snd {-locationId-} l /= snd {-locationId-} r {-allocate different locations to each teacher-} && Data.Resource.isAvailable (
						lookupResources l,
						lookupResources r
					) {-ensure common availability with each remaining list-}
				) {-permute teacherIds into locationIds-} . map (
					uncurry zip . Control.Arrow.first {-teacherId-} repeat	-- Construct '[(teacherId, locationId)]'.
				) . Data.Map.toList . Data.Map.mapWithKey (
					\teacherId course	-> Data.Map.keys $ Data.Map.filter (
						uncurry (&&) . (
							(
								Data.Course.getRequiredFacilityNames course `Data.Set.isSubsetOf`	-- Check that this location offers the required facilities.
							) . Data.Location.getFacilityNames &&& Data.Resource.isAvailable . (,,) (
								teacherRegister ! teacherId
							) (
								Data.Map.filter (
									Data.Foldable.elem (Data.Course.getSubject course) . Data.Student.deriveAmalgamatedKnowledgeRequirement
								) studentBodyRegister
							) -- Select locations which have some common availability with this teacher, & all interested students.
						)
					) locationCatalogue
				 )
			 ) coursesByTeacherIdBySynchronisationId
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForSynchronisedCoursesWithoutSuitableLocations problemValidationSwitches && not (null synchronisationIdsWithoutSuitableLocations),
			show ProblemConfiguration.ProblemValidationSwitches.checkForSynchronisedCoursesWithoutSuitableLocationsTag ++ ": no permutation of those locations offering the required facilities, is simultaneously available to the teachers of a synchronised course & to all interested students; " ++ show synchronisationIdsWithoutSuitableLocations
		),

		let
			duplicateStudentIds :: [(Data.Student.Id, [Aggregate.StudentBody.Mnemonic])]
			duplicateStudentIds	= map (
				(
					\studentId -> (
						studentId,
						map Aggregate.StudentBody.getMnemonic . Data.Set.toList . Data.Set.filter (
							(studentId `Data.Set.member`) . Aggregate.StudentBody.getStudentIds
						) $ Data.Map.keysSet studentBodyRegister
					) -- Pair.
				) . head
			 ) . filter (
				(/= 1) . length	-- Search for duplicates.
			 ) . ToolShed.Data.Foldable.gather . concatMap (
				Data.Set.toList . Aggregate.StudentBody.getStudentIds
			 ) $ Aggregate.StudentBodyRegister.getStudentBodies studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForDuplicateStudentIds problemValidationSwitches && not (null duplicateStudentIds),
			show ProblemConfiguration.ProblemValidationSwitches.checkForDuplicateStudentIdsTag ++ ": some student-identifiers exist in more than one student-body; " ++ show duplicateStudentIds
		),

-- Check the relative size of resources.
		let
			daysOnWhichStudentBodiesExceedTeachers :: Data.Map.Map Temporal.Day.Day String
			daysOnWhichStudentBodiesExceedTeachers	= Data.Map.map (
				\(nStudentBodies, nTeachers)	-> showString "number of student-bodies=" . shows nStudentBodies . showString " > number of teachers=" $ shows nTeachers ""
			 ) . Data.Map.filter (
				uncurry (>)
			 ) . Data.Map.fromList $ map (
				id &&& (
					Data.Map.size . (
						`Data.Resource.extractAvailableResources` studentBodyRegister
					) &&& Data.Map.size . (
						`Data.Resource.extractAvailableResources` teacherRegister
					)
				)
			 ) Temporal.Day.range
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodiesExceedTeachers problemValidationSwitches && not (Data.Map.null daysOnWhichStudentBodiesExceedTeachers),
			show ProblemConfiguration.ProblemValidationSwitches.checkIfStudentBodiesExceedTeachersTag ++ ": on some days, the number of student-bodies exceeds the number of available teachers; " ++ show (Data.Map.toList daysOnWhichStudentBodiesExceedTeachers)
		),
		let
			availableLocationCapacitiesByStudentBody	= Data.Map.filterWithKey (
				\studentBody -> Data.Foldable.all (Aggregate.StudentBody.getSize studentBody >)
			 ) $ Data.Map.map (
				\studentProfile -> Data.Map.map Data.Location.getCapacity $ Data.Map.filter (
					Data.Resource.isAvailable . (,) studentProfile
				) locationCatalogue
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodySizeExceedsCapacityOfAllLocations problemValidationSwitches && not (Data.Map.null availableLocationCapacitiesByStudentBody),
			show ProblemConfiguration.ProblemValidationSwitches.checkIfStudentBodySizeExceedsCapacityOfAllLocationsTag ++ ": some student-bodies can't be accommodated by the largest available location; " ++ show (
				map (
					(
						Aggregate.StudentBody.getMnemonic &&& Aggregate.StudentBody.getSize
					) *** take 1 {-there may be zero available locations-} . Data.List.sortBy (
						flip $ Data.Ord.comparing snd {-capacity-}	-- Largest first.
					) . Data.Map.toList
				) $ Data.Map.toList availableLocationCapacitiesByStudentBody
			)
		),
		let
			daysOnWhichStudentBodySizeExceedsCapacity :: Data.Map.Map Temporal.Day.Day String
			daysOnWhichStudentBodySizeExceedsCapacity	= Data.Map.map (
				(
					\(studentBodySize, capacity)	-> showString "student-body of size=" . shows studentBodySize . showChar ' ' $ if capacity == 0
						then "for which zero available locations exist"
						else showString "> capacity=" $ shows capacity ""
				) . head
			 ) . Data.Map.filter (
				not . null
			 ) . Data.Map.fromList $ map (
				\day -> (
					day,
					dropWhile (
						uncurry (<=)
					) $ zip (
						sortByDecreasingSize . map Aggregate.StudentBody.getSize . Aggregate.StudentBodyRegister.getStudentBodies $ Data.Resource.extractAvailableResources day studentBodyRegister
					) (
						(++ repeat 0) . sortByDecreasingSize . Data.Map.elems . Data.Map.map Data.Location.getCapacity $ Data.Resource.extractAvailableResources day locationCatalogue
					)
				) -- Pair.
			 ) Temporal.Day.range where
				sortByDecreasingSize :: [Size.NStudents] -> [Size.NStudents]
				sortByDecreasingSize	= Data.List.sortBy $ flip compare
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodySizeExceedsLocationCapacity problemValidationSwitches && not (Data.Map.null daysOnWhichStudentBodySizeExceedsCapacity),
			show ProblemConfiguration.ProblemValidationSwitches.checkIfStudentBodySizeExceedsLocationCapacityTag ++ ": on some days, a match between the sizes of student-bodies & the available locations, reveals inadequate capacity; " ++ show (Data.Map.toList daysOnWhichStudentBodySizeExceedsCapacity)
		),
		let
			timeslotsCapacity, timeslotsRequired :: Size.NTimeslots
			timeslotsCapacity	= (
				* nTimeslotsPerDay
			 ) . Data.Foldable.sum $ Data.Map.map (
				Temporal.Availability.countDaysPerWeekAvailable . Temporal.Availability.findIntersections . (
					: [
						Temporal.Availability.findUnions $ Data.Map.map Data.Resource.getAvailability studentBodyRegister,
						Temporal.Availability.findUnions $ Data.Map.map Data.Resource.getAvailability teacherRegister
					]
				) . Data.Resource.getAvailability	-- Select those days on which this location, @ least one student-body, & @ least one teacher, are simultaneously available.
			 ) locationCatalogue

			timeslotsRequired	= Data.Foldable.sum $ Data.Map.map (
				sum . map Data.Course.getRequiredLessonsPerWeek . Data.Set.toList . Data.Teacher.getService
			 ) teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckLocationsAvailabilityToSupportCourses problemValidationSwitches && timeslotsRequired > timeslotsCapacity,
			show ProblemConfiguration.ProblemValidationSwitches.checkLocationsAvailabilityToSupportCoursesTag ++ ": the " ++ show timeslotsRequired ++ " time-slots required to teach all the courses offered, exceeds the " ++ show timeslotsCapacity ++ " time-slots available in the configured locations"
		),
		let
			subjectsByStudentBody	= Data.Map.filter (
				not . Data.Set.null	-- Select those student-bodies, which require any subjects for which there are zero suitable locations.
			 ) $ Data.Map.mapWithKey (
				\studentBody studentProfile	-> Data.Set.filter (
					\subject -> not $ Data.Foldable.any (
						\teacherProfile -> Data.Foldable.any (
							uncurry (&&) . (
								Data.Course.isSuitable (Aggregate.StudentBody.getSize studentBody) subject &&& (
									\requiredFacilityNames -> Data.Foldable.any (
										\locationProfile -> all ($ locationProfile) [
											(>= Aggregate.StudentBody.getSize studentBody) . Data.Location.getCapacity,	-- Check that the location has adequate capacity.
											Data.Resource.isAvailable . (,,) studentProfile teacherProfile,			-- Check that all resources are simultaneously available.
											Data.Set.isSubsetOf requiredFacilityNames . Data.Location.getFacilityNames	-- Check that this location has adequate facilities.
										]
									) locationCatalogue
								) . Data.Course.getRequiredFacilityNames
							)
						) $ Data.Teacher.getService teacherProfile	-- Select the relevant portion of this teacher's service, which should be either zero or one course.
					) teacherRegister
				) $ Data.Student.deriveAmalgamatedKnowledgeRequirement studentProfile
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckSuitableLocationsForKnowledgeRequirements problemValidationSwitches && not (Data.Map.null subjectsByStudentBody),
			show ProblemConfiguration.ProblemValidationSwitches.checkSuitableLocationsForKnowledgeRequirementsTag ++ ": some student-bodies require subjects for which there's no suitable location; " ++ show (
				map (
					Aggregate.StudentBody.getMnemonic *** Data.Set.toList
				) $ Data.Map.toList subjectsByStudentBody
			)
		),

-- Check cross-references; must validate the existence of all requested subjects, before calling 'calculateStudentWorkloadBounds'.
		let
			nonExistentSubjectsByStudentBody	= Data.Map.filter (
				not . Data.Set.null
			 ) $ Data.Map.map (
				Data.Set.filter (
					`Data.Set.notMember` Aggregate.TeacherRegister.extractDistinctSubjects teacherRegister
				) . Data.Student.deriveAmalgamatedKnowledgeRequirement
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckSubjectExistence problemValidationSwitches && not (Data.Map.null nonExistentSubjectsByStudentBody),
			show ProblemConfiguration.ProblemValidationSwitches.checkSubjectExistenceTag ++ ": not all subjects required by student-bodies, are offered; " ++ show (
				map (
					Control.Arrow.first Aggregate.StudentBody.getMnemonic
				) . Data.Map.toList $ Data.Map.map Data.Set.toList nonExistentSubjectsByStudentBody
			)
		),
		let
			nonExistentMeetingLocationIdsByGroupId	= Data.Map.filter (`Data.Map.notMember` locationCatalogue) $ Data.Map.mapMaybe Data.Group.getMaybeLocationId groupCatalogue
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForNonExistentMeetingLocationIds problemValidationSwitches && not (Data.Map.null nonExistentMeetingLocationIdsByGroupId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForNonExistentMeetingLocationIdsTag ++ ": not all locations required for meetings, exist; " ++ show (Data.Map.toList nonExistentMeetingLocationIdsByGroupId)
		),
		let
			nonExistentOwnLocationIdsByTeacherId	= Data.Map.filter (`Data.Map.notMember` locationCatalogue) $ Data.Map.mapMaybe Data.Teacher.getMaybeOwnLocationId teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForNonExistentOwnLocationIds problemValidationSwitches && not (Data.Map.null nonExistentOwnLocationIdsByTeacherId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForNonExistentOwnLocationIdsTag ++ ": not all personal locations claimed by teachers, exist; " ++ show (Data.Map.toList nonExistentOwnLocationIdsByTeacherId)
		), -- CAVEAT: teachers may legitimately share a personal classroom, provided they either have mutually exclusive working-weeks, or interleave their classes.
		let
			unsupportedCoursesByTeacherId	= Data.Map.filter (not . Data.Set.null) $ Data.Map.map (
				\teacherProfile -> Data.Set.filter (
					\course -> not . Data.Foldable.any (
						(Data.Course.getRequiredFacilityNames course `Data.Set.isSubsetOf`) . Data.Location.getFacilityNames	-- Check that @ least one location offers the required facilities.
					) $ Data.Map.filter (
						Data.Resource.isAvailable . (,) teacherProfile	-- Check that this location & the teacher are simultaneously available.
					) locationCatalogue
				) $ Data.Teacher.getService teacherProfile
			 ) teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForNonExistentFacilities problemValidationSwitches && not (Data.Map.null unsupportedCoursesByTeacherId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForNonExistentFacilitiesTag ++ ": there are zero available locations, which offer all the facilities required, for @ least one of the courses offered; " ++ show (Data.Map.toList $ Data.Map.map Data.Set.toList unsupportedCoursesByTeacherId)
		),
		let
			nonExistentGroupIds :: Data.Group.Membership
			nonExistentGroupIds	= extractDistinctGroupMembership problemParameters \\ Data.Map.keysSet groupCatalogue
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForNonExistentGroupIds problemValidationSwitches && not (Data.Set.null nonExistentGroupIds),
			show ProblemConfiguration.ProblemValidationSwitches.checkForNonExistentGroupIdsTag ++ ": not all groups of which some student-bodies & teachers are members, exist; " ++ show (Data.Set.toList nonExistentGroupIds)
		),
		let
			groupIdsByLocationId	= Data.Map.filterWithKey (
				\groupId locationId	-> uncurry (+) (
					(
						Data.Maybe.maybe 0 Aggregate.StudentClass.getSize . Data.Map.lookup groupId *** Data.Maybe.maybe 0 Data.Set.size . Data.Map.lookup groupId
					) $ findHumanResourceIdsByGroupId problemParameters
				) > Data.Location.getCapacity (locationCatalogue ! locationId)
			 ) $ Data.Map.mapMaybe Data.Group.getMaybeLocationId groupCatalogue
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckCapacityOfLocationsForMeetings problemValidationSwitches && not (Data.Map.null groupIdsByLocationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkCapacityOfLocationsForMeetingsTag ++ ": some groups hold meetings in locations with inadequate capacity; " ++ show (Data.Map.toList groupIdsByLocationId)
		),
		let
			groupIdsByLocationIdByTime	= Data.Map.filter (
				Data.Foldable.any $ (> 1) . length
			 ) $ Data.Map.foldrWithKey (
				\groupId groupProfile m	-> Data.Maybe.maybe m (
					\locationId -> Data.Foldable.foldr (
						Data.Map.insertWith (
							Data.Map.unionWith (++)
						) `flip` Data.Map.singleton locationId [groupId]
					) m $ Data.Group.getMeetingTimes groupProfile
				) $ Data.Group.getMaybeLocationId groupProfile
			 ) Data.Map.empty groupCatalogue
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckDuplicateMeetingLocationIds problemValidationSwitches && not (Data.Map.null groupIdsByLocationIdByTime),
			show ProblemConfiguration.ProblemValidationSwitches.checkDuplicateMeetingLocationIdsTag ++ ": several groups require the same location simultaneously; " ++ show (Data.Map.toList $ Data.Map.map Data.Map.toList groupIdsByLocationIdByTime)
		),
		let
			invalidMeetingTimesByGroupId	= Data.Map.filter (
				Data.Foldable.any (
					\time -> any ($ Temporal.Time.getTimeslotId time) [
						(< Factory.Data.Interval.getMinBound timeslotIdBounds),
						(> Factory.Data.Interval.getMaxBound timeslotIdBounds)
					]
				)
			 ) $ Data.Map.map Data.Group.getMeetingTimes groupCatalogue
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForInvalidMeetingTimes problemValidationSwitches && not (Data.Map.null invalidMeetingTimesByGroupId),
			show ProblemConfiguration.ProblemValidationSwitches.checkForInvalidMeetingTimesTag ++ ": some groups specify meeting-times, using timeslot-identifiers which are outside permissible bounds " ++ show timeslotIdBounds ++ "; " ++ show (Data.Map.toList $ Data.Map.map Data.Set.toList invalidMeetingTimesByGroupId)
		),
		let
			daysAndLocationIdsByGroupId	= Data.Map.filter (
				not . Data.Set.null . fst {-days-}
			 ) . Data.Map.map (
				\(meetingDays, locationId)	-> (
					Data.Set.filter (
						not . (`Data.Resource.isAvailableOn` (locationCatalogue ! locationId))
					) meetingDays,
					locationId
				) -- Pair.
			 ) $ Data.Map.mapMaybe (
				uncurry fmap . (
					(,) . Data.Set.map Temporal.Time.getDay . Data.Group.getMeetingTimes &&& Data.Group.getMaybeLocationId
				)
			 ) groupCatalogue
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckMeetingLocationsAvailability problemValidationSwitches && not (Data.Map.null daysAndLocationIdsByGroupId),
			show ProblemConfiguration.ProblemValidationSwitches.checkMeetingLocationsAvailabilityTag ++ ": some groups require a location, which isn't available @ all of the specified meeting-times; " ++ show (
				Data.Map.toList $ Data.Map.map (Control.Arrow.first Data.Set.toList) daysAndLocationIdsByGroupId
			)
		),
		let
			groupsWithIndependentlyAvailableMembers	= filter (
				Temporal.Availability.isUnavailable . Data.Resource.getAvailability . (
					(`Data.HumanResource.extractGroupMembersOf` studentBodyRegister) &&& (`Data.HumanResource.extractGroupMembersOf` teacherRegister)
				)
			 ) . Data.Map.keys $ Data.Map.filter (
				uncurry (&&) . (
					(> 0) . Data.Group.countNTimeslotsPerWeek &&& Data.Group.getMandatesAttendance
				) -- If no meetings are required or members needn't attend, then they needn't ever be simultaneously available.
			 ) groupCatalogue
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckSimultaneousAvailabilityOfGroupMembers problemValidationSwitches && not (null groupsWithIndependentlyAvailableMembers),
			show ProblemConfiguration.ProblemValidationSwitches.checkSimultaneousAvailabilityOfGroupMembersTag ++ ": some groups which mandate attendance, can never meet because their members are never simultaneously available; " ++ show groupsWithIndependentlyAvailableMembers
		),
		let
			unavailableDaysByGroupId :: Data.Map.Map Data.Group.Id (Data.Set.Set Temporal.Day.Day)
			unavailableDaysByGroupId	= Data.Map.filter (
				not . Data.Set.null
			 ) . Data.Map.mapWithKey (
				\groupId -> let
					memberStudents	= Data.HumanResource.extractGroupMembersOf groupId studentBodyRegister
					memberTeachers	= Data.HumanResource.extractGroupMembersOf groupId teacherRegister
				in Data.Set.filter (
					\day -> (
						not (Data.Map.null memberStudents) || not (Data.Map.null memberTeachers)
					) && not (
						any (
							Temporal.Availability.isAvailableOn day
						) $ map Data.Resource.getAvailability (Data.Map.elems memberStudents) ++ map Data.Resource.getAvailability (Data.Map.elems memberTeachers)
					)
				)
			 ) $ Data.Map.map (
				Data.Set.map Temporal.Time.getDay . Data.Group.getMeetingTimes
			 ) groupCatalogue
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckAvailabilityOfAnyGroupMember problemValidationSwitches && not (Data.Map.null unavailableDaysByGroupId),
			show ProblemConfiguration.ProblemValidationSwitches.checkAvailabilityOfAnyGroupMemberTag ++ ": specific meetings of some groups can never be attended, because none of the members are available on that day; " ++ show (Data.Map.toList $ Data.Map.map Data.Set.toList unavailableDaysByGroupId)
		),
		let
			unavailableDaysByStudentBody	= findUnavailableDaysByGroupIdByHumanResourceId groupCatalogue studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsAvailabilityForMandatoryMeetings problemValidationSwitches && not (Data.Map.null unavailableDaysByStudentBody),
			show ProblemConfiguration.ProblemValidationSwitches.checkStudentsAvailabilityForMandatoryMeetingsTag ++ ": some student-bodies are unavailable for some of the meetings, of those groups mandating attendance, of which they're members; " ++ show (
				Data.Map.toList $ Data.Map.map (
					Data.Map.toList . Data.Map.map Data.Set.toList
				) unavailableDaysByStudentBody
			)
		),
		let
			unavailableDaysByTeacherId	= findUnavailableDaysByGroupIdByHumanResourceId groupCatalogue teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckTeachersAvailabilityForMandatoryMeetings problemValidationSwitches && not (Data.Map.null unavailableDaysByTeacherId),
			show ProblemConfiguration.ProblemValidationSwitches.checkTeachersAvailabilityForMandatoryMeetingsTag ++ ": some teachers are unavailable for some of the meetings, of those groups mandating attendance, of which they're members; " ++ show (
				Data.Map.toList $ Data.Map.map (
					Data.Map.toList . Data.Map.map Data.Set.toList
				) unavailableDaysByTeacherId
			)
		),
		let
			synchronousTimesByStudentBody	= Data.Map.filter (
				not . Data.Set.null	-- Leave only those student-bodies who're double-booked.
			 ) $ Data.Map.mapWithKey (
				\studentBody studentProfile	-> uncurry Data.Set.intersection $ (
					Data.Foldable.foldr Data.Set.union Data.Set.empty . Data.Set.map (
						\subject -> Data.Foldable.foldr1 Data.Set.intersection . Data.Map.map (
							Data.Foldable.foldr Data.Set.union Data.Set.empty . Data.Set.filter (
								Data.Foldable.all (
									(`Data.Resource.isAvailableOn` studentProfile) . Temporal.Time.getDay
								) -- Remove any course which specifies any time when the student's unavailable.
							) . Data.Set.map (
								Temporal.TimeslotRequest.getSpecifiedTimes . Data.Course.getTimeslotRequest
							) -- Extract the specified times from the relevant portion of this teacher's service; which could be null.
						 ) . Data.Map.filter (
							not . Data.Set.null	-- Remove those teachers who don't offer a course in the required subject.
						 ) . Data.Map.map (
							Data.Set.filter (
								Data.Course.isSuitable (Aggregate.StudentBody.getSize studentBody) subject
							) . Data.Teacher.getService	-- Select the relevant portion of this teacher's service, which should be either zero or one course.
						 ) $ Data.Map.filter (
							Data.Resource.isAvailable . (,) studentProfile	-- Confirm that this teacher & student-body are simultaneously available.
						 ) teacherRegister
					) . Data.Student.deriveAmalgamatedKnowledgeRequirement &&& Data.Foldable.foldr (
						Data.Set.union . Data.Group.getMeetingTimes . (
							groupCatalogue !	-- Acquire the group-profile.
						)
					) Data.Set.empty . {- Data.Set.filter Data.Group.getMandatesAttendance . -} Data.HumanResource.getGroupMembership
				) studentProfile
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckIndependenceOfStudentTimeslotsRequestsAndMeetings problemValidationSwitches && not (Data.Map.null synchronousTimesByStudentBody),
			show ProblemConfiguration.ProblemValidationSwitches.checkIndependenceOfStudentTimeslotsRequestsAndMeetingsTag ++ ": some student-bodies are members of groups, which meet @ times specified for all courses offering one of their knowledge-requirements; " ++ show (
				map (
					Control.Arrow.first Aggregate.StudentBody.getMnemonic
				) . Data.Map.toList $ Data.Map.map Data.Set.toList synchronousTimesByStudentBody
			)
		),
		let
			synchronousTimesByTeacherId	= Data.Map.filter (
				not . Data.Set.null	-- Leave only those teachers who're double-booked.
			 ) $ Data.Map.map (
				uncurry Data.Set.intersection . (
					Data.Teacher.findSpecifiedTimes &&& Data.Foldable.foldr (
						Data.Set.union . Data.Group.getMeetingTimes . (
							groupCatalogue !	-- Acquire the group-profile.
						)
					) Data.Set.empty . {- Data.Set.filter Data.Group.getMandatesAttendance . -} Data.HumanResource.getGroupMembership
				)
			 ) teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckIndependenceOfTeacherTimeslotsRequestsAndMeetings problemValidationSwitches && not (Data.Map.null synchronousTimesByTeacherId),
			show ProblemConfiguration.ProblemValidationSwitches.checkIndependenceOfTeacherTimeslotsRequestsAndMeetingsTag ++ ": some teachers are members of groups, which meet @ times specified for one of their courses; " ++ show (Data.Map.toList $ Data.Map.map Data.Set.toList synchronousTimesByTeacherId)
		),
		let
			synchronousSeparateMeetingsByTimeByTeacherId	= findSynchronousSeparateMandatoryMeetingsByTimeByHumanResourceId groupCatalogue teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckTeachersForSynchronousMeetings problemValidationSwitches && not (Data.Map.null synchronousSeparateMeetingsByTimeByTeacherId),
			show ProblemConfiguration.ProblemValidationSwitches.checkTeachersForSynchronousMeetingsTag ++ ": some teachers are members of groups with synchronous meeting-times, in separate locations; " ++ show (Data.Map.toList $ Data.Map.map Data.Map.toList synchronousSeparateMeetingsByTimeByTeacherId)
		),
		let
			synchronousSeparateMeetingsByTimeByStudentBody	= findSynchronousSeparateMandatoryMeetingsByTimeByHumanResourceId groupCatalogue studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsForSynchronousMeetings problemValidationSwitches && not (Data.Map.null synchronousSeparateMeetingsByTimeByStudentBody),
			show ProblemConfiguration.ProblemValidationSwitches.checkStudentsForSynchronousMeetingsTag ++ ": some student-bodies are members of groups with synchronous meeting-times, in separate locations; " ++ show (Data.Map.toList $ Data.Map.map Data.Map.toList synchronousSeparateMeetingsByTimeByStudentBody)
		),
		let
			teacherIds	= Data.Map.keys . Data.Map.filter (
				not . Data.Resource.isAvailable	-- Operate on Pair.
			 ) $ Data.Map.mapMaybe (
				\teacherProfile -> ((,) teacherProfile . (locationCatalogue !)) `fmap` Data.Teacher.getMaybeOwnLocationId teacherProfile
			 ) teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckOwnLocationsAvailability problemValidationSwitches && not (null teacherIds),
			show ProblemConfiguration.ProblemValidationSwitches.checkOwnLocationsAvailabilityTag ++ ": some teachers claim a personal location, which isn't available @ any time during their working-week; " ++ show teacherIds
		),
		let
			duplicateClaimsByLocationId	= Data.Map.map (
				Aggregate.TeacherRegister.getTeacherIds . fst {-teacherRegister-}
			 ) . Data.Map.filter (
				uncurry (&&) . (
					(> 1) . Data.Map.size . fst {-teacherRegister-} &&& Data.Resource.isAvailable {-days on which the location & all relevant teachers are available-}
				)
			 ) $ Data.Map.mapWithKey (
				(,) . (`Data.Map.filter` teacherRegister) . Data.Teacher.inhabits
			 ) locationCatalogue
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckDuplicateOwnLocationIds problemValidationSwitches && not (Data.Map.null duplicateClaimsByLocationId),
			show ProblemConfiguration.ProblemValidationSwitches.checkDuplicateOwnLocationIdsTag ++ ": ownership of a single location, is claimed by teachers whose working-weeks aren't mutually exclusive; " ++ show (Data.Map.toList duplicateClaimsByLocationId)
		),
		let
			teacherIdsByTime	= Data.Map.filter (
				(> Data.Map.size locationCatalogue) . length	-- CAVEAT: a lax constraint, since some locations may be unavailable or unsuitable.
			 ) . Data.Map.foldrWithKey (
				\teacherId -> flip $ Data.Foldable.foldr (
					Data.Map.insertWith (++) `flip` [teacherId]
				) -- Invert the map.
			 ) Data.Map.empty $ Data.Map.map Data.Teacher.findSpecifiedTimes teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckLocationsForSynchronousSpecifiedTimes problemValidationSwitches && not (Data.Map.null teacherIdsByTime),
			show ProblemConfiguration.ProblemValidationSwitches.checkLocationsForSynchronousSpecifiedTimesTag ++ ": more courses specify the same booking-time, than there are locations in which to hold them; " ++ show (Data.Map.toList teacherIdsByTime)
		),
		let
			unavailableSubjectsByStudentBody = Data.Map.filter (not . Data.Set.null) $ Data.Map.mapWithKey (
				\studentBody studentProfile	-> Data.Set.filter (
					\subject -> not . Data.Foldable.any (
						Data.Foldable.all (
							(`Data.Resource.isAvailableOn` studentProfile) . Temporal.Time.getDay
						) . Temporal.TimeslotRequest.getSpecifiedTimes . Data.Course.getTimeslotRequest
					) . Aggregate.TeacherRegister.findSuitableCourseByTeacherId (
						Aggregate.StudentBody.getSize studentBody	-- Ensure the student-body fits within 'Data.Course.getMaybeMaximumClassSize'.
					) subject $ Data.Map.filter (
						Data.Resource.isAvailable . (,) studentProfile	-- Find teachers available simultaneously with this student.
					) teacherRegister
				) $ Data.Student.deriveAmalgamatedKnowledgeRequirement studentProfile
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsAvailabilityForSpecifiedTimes problemValidationSwitches && not (Data.Map.null unavailableSubjectsByStudentBody),
			show ProblemConfiguration.ProblemValidationSwitches.checkStudentsAvailabilityForSpecifiedTimesTag ++ ": some student-bodies have requested subjects, for which either there're zero suitable courses, or all suitable courses specify booking-times when they're unavailable; " ++ show (Data.Map.toList $ Data.Map.map Data.Set.toList unavailableSubjectsByStudentBody)
		),
		let
			studentBodiesWithSynchronousTimeslotRequests :: [Aggregate.StudentBody.StudentBody]
			studentBodiesWithSynchronousTimeslotRequests = Data.Map.keys . Data.Map.filter null {-no acceptable permutation of courses-} $ Data.Map.mapWithKey (
				\studentBody studentProfile	-> filter {-[[[Time]]]-} (
					not . Data.List.Extra.anySame . concat {-merge the specified times, for a permutation of courses providing all required subjects-}
				) . ToolShed.Data.List.permutations {-where several teachers offer a matching course-} . map (
					\subject -> filter (
						all ((`Data.Resource.isAvailableOn` studentProfile) . Temporal.Time.getDay)	-- The student-body must be available @ all the times specified in any course.
					) . map (
						Data.Set.toList . Temporal.TimeslotRequest.getSpecifiedTimes . Data.Course.getTimeslotRequest	-- Extract the potentially null list of times specified by each course.
					) . Data.Map.elems . Aggregate.TeacherRegister.findSuitableCourseByTeacherId (Aggregate.StudentBody.getSize studentBody) subject $ Data.Map.filter (
						Data.Resource.isAvailable . (,) studentProfile
					) teacherRegister
				) . Data.Set.toList $ Data.Student.deriveAmalgamatedKnowledgeRequirement studentProfile
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckCoursesForSynchronousSpecifiedTimes problemValidationSwitches && not (null studentBodiesWithSynchronousTimeslotRequests),
			show ProblemConfiguration.ProblemValidationSwitches.checkCoursesForSynchronousSpecifiedTimesTag ++ ": some student-bodies require subjects, for which either there're zero suitable courses, or present a conflict between the specified booking-times of those courses which are suitable; " ++ show studentBodiesWithSynchronousTimeslotRequests
		),
		let
			subjectsWithInsufficientTeachers	= Data.Map.filter (
				uncurry (>)	-- Demand exceeds supply.
			 ) . Data.Map.mapWithKey (
				\subject nClassesRequired	-> (
					nClassesRequired,
					sum {-aggregate supply-} [
						nTimeslotPerWeekOfTeaching `div` Data.Course.getRequiredLessonsPerWeek course |
							(nTimeslotPerWeekOfTeaching, Just course)	<- map (
								Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay &&& Data.Teacher.lookupCourseIn subject
							) $ Data.Map.elems teacherRegister
					] -- List-comprehension.
				) -- Pair.
			 ) . Data.Map.fromListWith (+) . map (
				flip (,) 1	-- Count the number of separate classes required in each subject. CAVEAT: ignores the class-size, which may overflow all suitable locations.
			 ) . concatMap (
				Data.Set.toList . Data.Set.unions . map Data.Student.deriveAmalgamatedKnowledgeRequirement	-- Derive total knowledge-requirement of each student-class.
			 ) $ ToolShed.Data.Foldable.gatherBy Data.Student.getStream studentBodyRegister		-- Gather student-bodies which can be temporarily merged. CAVEAT: assumes 'ExecutionConfiguration.ExecutionOptions.getPermitTemporaryStudentBodyMerger'.
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckTeachingCapacityBySubject problemValidationSwitches && not (Data.Map.null subjectsWithInsufficientTeachers),
			show ProblemConfiguration.ProblemValidationSwitches.checkTeachingCapacityBySubjectTag ++ ": the total demand for separate courses in some subjects exceeds that offered by teachers; " ++ show (Data.Map.toList subjectsWithInsufficientTeachers)
		),

-- Check workload-bounds.
		let
			idleStudentBodies	= Data.Map.filter (
				uncurry (&&) . (
					Data.Set.null . Data.Student.deriveAmalgamatedKnowledgeRequirement &&& (> 0) . Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay
				)
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForIdleStudents problemValidationSwitches && not (Data.Map.null idleStudentBodies),
			show ProblemConfiguration.ProblemValidationSwitches.checkForIdleStudentsTag ++ ": some student-bodies require zero subjects, but have allocated one or more time-slots for teaching; " ++ show (Data.Map.toList idleStudentBodies)
		),
		let
			overloadedStudentBodies	= Data.Map.filter (
				uncurry (&&) . (
					not . Data.Set.null . Data.Student.deriveAmalgamatedKnowledgeRequirement &&& (== 0) . Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay
				)
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForOverloadedStudents problemValidationSwitches && not (Data.Map.null overloadedStudentBodies),
			show ProblemConfiguration.ProblemValidationSwitches.checkForOverloadedStudentsTag ++ ": some student-bodies have allocated zero time-slots for teaching, but have requested one or more subjects; " ++ show (Data.Map.toList overloadedStudentBodies)
		),
		let
			idleTeachers	= Data.Map.filter (
				uncurry (&&) . (
					not . Data.Teacher.offersService &&& (> 0) . Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay
				)
			 ) teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForIdleTeachers problemValidationSwitches && not (Data.Map.null idleTeachers),
			show ProblemConfiguration.ProblemValidationSwitches.checkForIdleTeachersTag ++ ": some teachers offer zero courses required by student-bodies, but have allocated one or more time-slots for teaching; " ++ show (Data.Map.toList idleTeachers)
		),
		let
			slackStudentsBodies :: Data.Map.Map Aggregate.StudentBody.StudentBody (Size.NTimeslots, Size.NTimeslots)
			slackStudentsBodies	= Data.Map.filter (uncurry (<)) $ Data.Map.map (
				(
					Temporal.Workload.getMaximum . uncurry (<+>)	-- Add the workloads associated with core & optional subject-requirements.
				) . calculateStudentWorkloadBounds teacherRegister &&& Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsLowerWorkloadBound problemValidationSwitches && not (Data.Map.null slackStudentsBodies),
			show ProblemConfiguration.ProblemValidationSwitches.checkStudentsLowerWorkloadBoundTag ++ ": the workload associated with all subject-requirements, for some student-bodies, is insufficient to meet the tuition-time in their working-week; " ++ show (map (Control.Arrow.first Aggregate.StudentBody.getMnemonic) $ Data.Map.toList slackStudentsBodies)
		),
		let
			overloadedStudentBodies :: Data.Map.Map Aggregate.StudentBody.StudentBody (Size.NTimeslots, Size.NTimeslots)
			overloadedStudentBodies	= Data.Map.filter (uncurry (>)) $ Data.Map.map (
				Temporal.Workload.getMinimum . Data.Requirements.getCore . calculateStudentWorkloadBounds teacherRegister &&& Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsUpperWorkloadBound problemValidationSwitches && not (Data.Map.null overloadedStudentBodies),
			show ProblemConfiguration.ProblemValidationSwitches.checkStudentsUpperWorkloadBoundTag ++ ": the workload associated with core subject-requirements, for some student-bodies, exceeds the tuition-time available in their working-week; " ++ show (map (Control.Arrow.first Aggregate.StudentBody.getMnemonic) $ Data.Map.toList overloadedStudentBodies)
		),
		let
			overloadedStudentBodies	= findHumanResourcesOverloadedWithMeetings nTimeslotsPerDay groupCatalogue studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsUpperWorkloadBound problemValidationSwitches && not (Data.Map.null overloadedStudentBodies),
			show ProblemConfiguration.ProblemValidationSwitches.checkStudentsUpperWorkloadBoundTag ++ ": the time associated with meetings, for some student-bodies, exceeds the non-teaching time available in their working-week; " ++ show (Data.Map.toList overloadedStudentBodies)
		),
		let
			overloadedTeachers	= findHumanResourcesOverloadedWithMeetings nTimeslotsPerDay groupCatalogue teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckTeachersUpperWorkloadBound problemValidationSwitches && not (Data.Map.null overloadedTeachers),
			show ProblemConfiguration.ProblemValidationSwitches.checkTeachersUpperWorkloadBoundTag ++ ": the time associated with meetings, for some teachers, exceeds the non-teaching time available in their working-week; " ++ show (Data.Map.toList overloadedTeachers)
		),

-- Check miscellaneous.
		let
			rogueStudentBodies	= Data.Map.filter (
				Data.Foldable.any (not . Data.Set.null)
			 ) $ Data.Map.map (
				Data.Map.filter (
					(> 1) . Data.Set.size
				) . Data.Map.fromListWith Data.Set.union . map (
					Data.Subject.getTopic &&& Data.Set.singleton . Data.Subject.getLevel
				) . Data.Set.toList . Data.Student.deriveAmalgamatedKnowledgeRequirement
			 ) studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckStudentsForMultipleLevelsOfSameTopic problemValidationSwitches && not (Data.Map.null rogueStudentBodies),
			show ProblemConfiguration.ProblemValidationSwitches.checkStudentsForMultipleLevelsOfSameTopicTag ++ ": some student-body's knowledge-requirements include more than one level of the same topic; " ++ show (
				Data.Map.toList $ Data.Map.map (Data.Map.toList . Data.Map.map Data.Set.toList) rogueStudentBodies
			)
		),
		let
			studentBodiesWithUnrealisableFreePeriodPreference	= extractResourceIdsWithUnrealisableFreePeriodPreference studentBodyRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForStudentsWithUnrealisableFreePeriodPreference problemValidationSwitches && not (null studentBodiesWithUnrealisableFreePeriodPreference),
			show ProblemConfiguration.ProblemValidationSwitches.checkForStudentsWithUnrealisableFreePeriodPreferenceTag ++ ": the preference of some student-bodies, for the position of free time-slots, within those days on which they're available, can never be realised because of the meeting-times of groups of which they're members; " ++ show (map Aggregate.StudentBody.getMnemonic studentBodiesWithUnrealisableFreePeriodPreference)
		),
		let
			teacherIdsWithUnrealisableFreePeriodPreference	= extractResourceIdsWithUnrealisableFreePeriodPreference teacherRegister
		in (
			ProblemConfiguration.ProblemValidationSwitches.getCheckForTeachersWithUnrealisableFreePeriodPreference problemValidationSwitches && not (null teacherIdsWithUnrealisableFreePeriodPreference),
			show ProblemConfiguration.ProblemValidationSwitches.checkForTeachersWithUnrealisableFreePeriodPreferenceTag ++ ": the preference of some teachers, for the position of free time-slots, within those days on which they're available, can never be realised because of the meeting-times of groups of which they're members; " ++ show teacherIdsWithUnrealisableFreePeriodPreference
		)
	 ] where
		nTimeslotsPerDay :: Size.NTimeslots
		nTimeslotsPerDay	= calculateNTimeslotsPerDay timeslotIdBounds

		extractResourceIdsWithUnrealisableFreePeriodPreference :: (
			Data.HumanResource.HumanResource	humanResource,
			Data.Resource.Resource			humanResource
		 ) => Data.Resource.ResourceMap humanResourceId humanResource -> [humanResourceId]
		extractResourceIdsWithUnrealisableFreePeriodPreference	= Data.Map.keys . Data.Map.filter (
			\profile -> let
				meetingTimesByDay	= Temporal.Time.categoriseByDay . Aggregate.GroupCatalogue.getMeetingTimes groupCatalogue $ Data.HumanResource.getGroupMembership profile
			in Data.Foldable.all (
				(
					\meetingTimeslotIdSet -> case Data.Maybe.fromJust $ Data.HumanResource.getMaybeFreePeriodPreference profile of
						Temporal.FreePeriodPreference.Pre	-> Data.Set.member (Factory.Data.Interval.getMinBound timeslotIdBounds) meetingTimeslotIdSet
						Temporal.FreePeriodPreference.Post	-> all (`Data.Set.member` meetingTimeslotIdSet) $ Factory.Data.Interval.toList timeslotIdBounds
						Temporal.FreePeriodPreference.Terminal	-> Data.Set.member (Factory.Data.Interval.getMaxBound timeslotIdBounds) meetingTimeslotIdSet
				) . Data.Maybe.fromMaybe Data.Set.empty . (`Data.Map.lookup` meetingTimesByDay)
			) . Temporal.Availability.deconstruct $ Data.Resource.getAvailability profile
		 ) . Data.Map.filter Data.HumanResource.hasFreePeriodPreference

		coursesByTeacherIdBySynchronisationId	= Aggregate.TeacherRegister.findCoursesByTeacherIdBySynchronisationId teacherRegister
		distinctCoursesBySynchronisationId	= Aggregate.TeacherRegister.findDistinctCoursesBySynchronisationId coursesByTeacherIdBySynchronisationId

{- |
	* Merges those /student-bodies/ whose /profile/s are identical, into a /student-body/.

	* Also writes the list of those students affected.
-}
reduceStudentBodyRegister :: (
	Ord	level,
	Ord	stream,
	Ord	teachingRatio
 )
	=> Aggregate.StudentClass.MnemonicSeparator
	-> ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Control.Monad.Writer.Writer [[Aggregate.StudentBody.StudentBody]] (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId)
reduceStudentBodyRegister mnemonicSeparator problemParameters	= do
	studentBodyRegister	<- Aggregate.StudentBodyRegister.reduce mnemonicSeparator $ getStudentBodyRegister problemParameters

	return {-to Writer-monad-} problemParameters { getStudentBodyRegister = studentBodyRegister }

{- |
	* Removes /course/s from the /teacherRegister/, which aren't required by any /student/.

	* Writes an association-list of /teacherId/ & any /knowledge/ they offer for which there's no demand;
	where there's a requirement for all a /teacher/'s knowledge, then no data is written for that teacher, rather than writing a null set.
-}
removeRedundantCourses :: (
#if !MIN_VERSION_containers(0,5,2)
	Ord	synchronisationId,
	Ord	timeslotId,
#endif
	Ord	level
 )
	=> ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Control.Monad.Writer.Writer [(teacherId, Data.Subject.Knowledge level)] (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId)
removeRedundantCourses problemParameters	= let
	(teacherRegister, redundantServiceByTeacherId)	= (
		Data.Map.map fst &&& Data.Map.filter (not . Data.Set.null) . Data.Map.map snd
	 ) . Data.Map.map (
		\profile -> let
			(requiredService, redundantService)	= Data.Set.partition (
				(`Data.Set.member` Aggregate.StudentBodyRegister.extractDistinctSubjects (getStudentBodyRegister problemParameters)) . Data.Course.getSubject
			 ) $ Data.Teacher.getService profile
		in (
			profile {Data.Teacher.getService = requiredService},
			redundantService
		) -- Pair.
	 ) $ getTeacherRegister problemParameters
 in do
	Control.Monad.Writer.tell . Data.Map.toList $ Data.Map.map (Data.Set.map Data.Course.getSubject) redundantServiceByTeacherId

	return {-to Writer-monad-} problemParameters {
		getTeacherRegister	= teacherRegister
	}

-- | Remove any /groups/ from the /groupCatalogue/ which have zero /meeting-times/.
removePointlessGroups
	:: ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Control.Monad.Writer.Writer [Data.Group.Id] (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId)
removePointlessGroups problemParameters	= let
	(pointlessGroupMembership, reducedGroupCatalogue)	= Control.Arrow.first Data.Map.keysSet . Data.Map.partition (
		Data.Set.null . Data.Group.getMeetingTimes
	 ) $ getGroupCatalogue problemParameters
 in do
	Control.Monad.Writer.tell $ Data.Set.toList pointlessGroupMembership

	return {-to Writer-monad-} problemParameters {
		getGroupCatalogue	= reducedGroupCatalogue,
		getStudentBodyRegister	= Data.Map.map (Data.Student.unsubscribe pointlessGroupMembership) $ getStudentBodyRegister problemParameters,
		getTeacherRegister	= Data.Map.map (Data.Teacher.unsubscribe pointlessGroupMembership) $ getTeacherRegister problemParameters
	}

-- | Remove any /groups/ from the /groupCatalogue/ to which neither /student-bodies/ nor /teachers/ have subscribed.
removeUnsubscribedGroups
	:: RealFrac teachingRatio
	=> ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Control.Monad.Writer.Writer [Data.Group.Id] (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId)
removeUnsubscribedGroups problemParameters	= let
	(reducedGroupCatalogue, unsubscribedGroupCatalogue)	= Data.Map.partitionWithKey (
		\groupId _	-> Data.Set.member groupId . uncurry Data.Set.union $ (
			Data.HumanResource.extractCombinedGroupMembership . getStudentBodyRegister &&& Data.HumanResource.extractCombinedGroupMembership . getTeacherRegister
		) problemParameters
	 ) $ getGroupCatalogue problemParameters
 in do
	Control.Monad.Writer.tell $ Data.Map.keys unsubscribedGroupCatalogue

	return {-to Writer-monad-} problemParameters {
		getGroupCatalogue	= reducedGroupCatalogue
	}

-- | Proxies request to 'Aggregate.TeacherRegister.mergeConstraintsOnSynchronisedCourses'.
mergeConstraintsOnSynchronisedCourses :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	teacherId,
	Ord	timeslotId,
	Show	synchronisationId,
	Show	timeslotId
 ) => ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
mergeConstraintsOnSynchronisedCourses problemParameters	= problemParameters {
	getTeacherRegister	= Aggregate.TeacherRegister.mergeConstraintsOnSynchronisedCourses $ getTeacherRegister problemParameters
}

{- |
	* When temporary student-body mergers are permitted according to the execution-options, some problem-validation switches are inappropriate & should be disabled to prevent false positives.

	* Also writes the names of any disabled switches.
-}
disableAnyValidationInappropriateForTemporaryStudentBodyMerger
	:: ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId
	-> Control.Monad.Writer.Writer [String] (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId)
disableAnyValidationInappropriateForTemporaryStudentBodyMerger problemParameters	= foldr (
	\(_, disabledSwitchTag)	-> (Control.Monad.Writer.tell [disabledSwitchTag] >>)	-- Report the disabled problem-validation switch's name, & forward the writer.
 ) (
	return {-to Writer-monad-} problemParameters {
		getProblemValidationSwitches	= problemValidationSwitches {
			ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodiesExceedTeachers		= False,
			ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodySizeExceedsLocationCapacity	= False
		} -- Disable potentially conflicting problem-validation switches.
	} -- Initial value.
 ) $ filter (
	($ problemValidationSwitches) . fst {-accessor-}	-- Select enabled problem-validation switches.
 ) [
	(ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodiesExceedTeachers,			ProblemConfiguration.ProblemValidationSwitches.checkIfStudentBodiesExceedTeachersTag),
	(ProblemConfiguration.ProblemValidationSwitches.getCheckIfStudentBodySizeExceedsLocationCapacity,	ProblemConfiguration.ProblemValidationSwitches.checkIfStudentBodySizeExceedsLocationCapacityTag)
 ] where
	problemValidationSwitches	= getProblemValidationSwitches problemParameters

-- | True if any /student-body/ or /teacher/ has a preference for the location of any free timeslots in their working day.
hasAnyFreePeriodPreference :: RealFrac teachingRatio => ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> Bool
hasAnyFreePeriodPreference	= uncurry (||) . (Aggregate.StudentBodyRegister.hasAnyFreePeriodPreference . getStudentBodyRegister &&& Aggregate.TeacherRegister.hasAnyFreePeriodPreference . getTeacherRegister)

-- | Whether /course/s offer different /minimumConsecutiveLessons/.
hasVariousMinimumConsecutiveLessons ::
#if !MIN_VERSION_containers(0,5,2)
	(
		Ord	level,
		Ord	synchronisationId,
		Ord	timeslotId
	) =>
#endif
	ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId -> Bool
hasVariousMinimumConsecutiveLessons = (> 1) . Data.Set.size . Data.Foldable.foldr (
	\teacherProfile -> Data.Set.union (Data.Set.map Data.Course.getMinimumConsecutiveLessons $ Data.Teacher.getService teacherProfile)
 ) Data.Set.empty . getTeacherRegister

instance (
	Enum		timeslotId,
	Ord		level,
	Ord		synchronisationId,
	Ord		teacherId,
	Ord		timeslotId,
	RealFrac	teachingRatio,
	Show		level,
	Show		synchronisationId,
	Show		teacherId,
	Show		timeslotId
 ) => Configuration.Configuration (ProblemParameters campus level locationId stream synchronisationId teacherId teachingRatio timeslotId) where
	issueWarnings problemParameters	= [
		msg | (True, msg) <- [
			let
				excessTotalWorkloadByStudentBody :: Data.Map.Map Aggregate.StudentBody.StudentBody Size.NTimeslots
				excessTotalWorkloadByStudentBody	= findExcessTotalWorkloadByStudentBody problemParameters
			in (
				not $ Data.Map.null excessTotalWorkloadByStudentBody,
				"the time required for the total (core & optional) knowledge-requirements of some student-bodies, exceeds that allocated to teaching; " ++ show (
					Control.Arrow.first Aggregate.StudentBody.getMnemonic `map` Data.Map.toList excessTotalWorkloadByStudentBody
				)
			),
			let
				subjectsOfferedInMultipleCoursesRequiringDifferentLessonsPerWeek	= Aggregate.TeacherRegister.findSubjectsOfferedInMultipleCoursesRequiringDifferentLessonsPerWeek $ getTeacherRegister problemParameters
			in (
				not $ Data.Map.null subjectsOfferedInMultipleCoursesRequiringDifferentLessonsPerWeek,
				"some subjects have been offered by more than one teacher, but in courses requiring a different number of lessons per week; " ++ show (
					Data.Map.toList $ Data.Map.map Data.Map.toList subjectsOfferedInMultipleCoursesRequiringDifferentLessonsPerWeek
				)
			), (
				ProblemConfiguration.ValidationSwitch.areAllOff $ getProblemValidationSwitches problemParameters,
				"all problem-validation switches are off; the problem-parameters may be invalid"
			), (
				ProblemConfiguration.ValidationSwitch.areAllOff $ getTimetableValidationSwitches problemParameters,
				"all timetable-validation switches are off; any imported timetable may be invalid"
			),
			let
				subjectsByTeacherId	= Data.Map.map (
					map Data.Course.getSubject . Data.Set.toList
				 ) . Data.Map.filter (
					not . Data.Set.null
				 ) . Data.Map.map (
					Data.Set.filter (
						Data.Maybe.maybe False (
							null . show	-- BODGE: the type is unknown.
						) . Data.Course.getMaybeSynchronisationId
					) . Data.Teacher.getService
				 ) $ getTeacherRegister problemParameters
			in (
				not $ Data.Map.null subjectsByTeacherId,
				"some teachers have defined courses with a null 'synchronisationId'; " ++ show (Data.Map.toList subjectsByTeacherId)
			),
			let
				synchronousMeetingsByTimeByStudentBodyMnemonic	= findSynchronousMeetingsByTimeByStudentBodyMnemonic problemParameters
			in (
				not $ Data.Map.null synchronousMeetingsByTimeByStudentBodyMnemonic,
				"the following student-bodies have synchonous group-meetings; " ++ show (
					map (Control.Arrow.first Aggregate.StudentBody.getMnemonic) . Data.Map.toList $ Data.Map.map Data.Map.toList synchronousMeetingsByTimeByStudentBodyMnemonic
				)
			), -- Pair.
			let
				synchronousMeetingsByTimeByTeacherId	= findSynchronousMeetingsByTimeByTeacherId problemParameters
			in (
				not $ Data.Map.null synchronousMeetingsByTimeByTeacherId,
				"the following teachers have synchonous group-meetings; " ++ show (Data.Map.toList $ Data.Map.map Data.Map.toList synchronousMeetingsByTimeByTeacherId)
			), -- Pair.
			case calculateNTimeslotsPerDay $ getTimeslotIdBounds problemParameters of
				1 -> (
					hasAnyFreePeriodPreference problemParameters,
					"a " ++ show Temporal.FreePeriodPreference.tag ++ " is meaningless with only one time-slot per day"
				 ) -- Pair.
				2 -> (
					let
						hasTerminalFreePeriodPreference :: Data.HumanResource.HumanResource resource => Data.Resource.ResourceMap resourceId resource -> Bool
						hasTerminalFreePeriodPreference	= Data.Foldable.any $ (== Just Temporal.FreePeriodPreference.Terminal) . Data.HumanResource.getMaybeFreePeriodPreference
					in uncurry (||) $ (
						hasTerminalFreePeriodPreference . getStudentBodyRegister &&& hasTerminalFreePeriodPreference . getTeacherRegister
					) problemParameters,
					"'" ++ Temporal.FreePeriodPreference.tag ++ "=" ++ show Temporal.FreePeriodPreference.Terminal ++ "' is redundant when there are only two time-slots per day"
				 ) -- Pair.
				_ -> (False, undefined)
		]
	 ]

