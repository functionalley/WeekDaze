{-# LANGUAGE CPP, FlexibleContexts #-}
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

 [@DESCRIPTION@]	Describes all the /teacher/s available to be booked in the timetable, in terms of their name & their attributes.
-}

module WeekDaze.Aggregate.TeacherRegister(
-- * Types
-- ** Type-synonyms
	TeacherRegister,
	CoursesByTeacherId,
	CoursesByTeacherIdBySynchronisationId,
	NTimeslotsByTeacherIdBySubject,
-- * Constants
	tag,
-- * Functions
	calculateWorkloadBoundsBySubject,
	countAvailableTeacherDays,
	extractDistinctCourses,
	extractDistinctOwnLocationIds,
	extractDistinctSubjects,
--	extractDistinctRequiredFacilityNames,
	findSuitableCourseByTeacherId,
	findCoursesByTeacherIdBySynchronisationId,
	findDistinctCoursesBySynchronisationId,
	findSpecifiedTimes,
	findSubjectsOfferedInMultipleCoursesRequiringDifferentLessonsPerWeek,
	mergeConstraintsOnSynchronisedCourses,
	countLessonsPerWeekByFacilityName,
-- ** Accessors
	getTeacherIds,
	getTeacherProfiles,
#ifdef USE_HDBC
-- ** Constructor
	fromDatabase,
#endif
-- ** Predicates
	hasAnyFreePeriodPreference,
	hasAnySynchronisedCourses,
	hasAnyIdealTimeslotRequests,
	hasAnyCourseMaximumClassSizes,
	hasAnySpecificTimeRequests,
	hasAnyTimeslotRequests,
	hasAnySpecialists,
	isInhabited
) where

import qualified	Control.Arrow
import			Control.Arrow((&&&), (***))
import qualified	Data.Foldable
import qualified	Data.Map
import			Data.Map((!))
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	WeekDaze.Data.Course			as Data.Course
import qualified	WeekDaze.Data.HumanResource		as Data.HumanResource
import qualified	WeekDaze.Data.Location			as Data.Location
import qualified	WeekDaze.Data.Resource			as Data.Resource
import qualified	WeekDaze.Data.Subject			as Data.Subject
import qualified	WeekDaze.Data.Teacher			as Data.Teacher
import qualified	WeekDaze.Size				as Size
import qualified	WeekDaze.Temporal.Time			as Temporal.Time
import qualified	WeekDaze.Temporal.TimeslotRequest	as Temporal.TimeslotRequest
import qualified	WeekDaze.Temporal.Workload		as Temporal.Workload

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.IntMap
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector		as Database.Selector
import qualified	WeekDaze.Data.Group			as Data.Group
import qualified	WeekDaze.Temporal.Availability		as Temporal.Availability
import qualified	WeekDaze.Temporal.Day			as Temporal.Day
import qualified	WeekDaze.Temporal.FreePeriodPreference	as Temporal.FreePeriodPreference

{- |
	* Construct from the specified database-connection.

	* CAVEAT: though the database may not permit a null value for many fields (applying its own default value when the value is unspecified),
	default values are applied here should the SQL-query return one.
-}
fromDatabase :: (
	Database.HDBC.IConnection	connection,
	Data.Convertible.Convertible	Database.HDBC.SqlValue level,			-- Flexible context.
	Data.Convertible.Convertible	Database.HDBC.SqlValue locationId,		-- Flexible context.
	Data.Convertible.Convertible	Database.HDBC.SqlValue synchronisationId,	-- Flexible context.
	Data.Convertible.Convertible	Database.HDBC.SqlValue teacherId,		-- Flexible context.
	Data.Convertible.Convertible	Database.HDBC.SqlValue teachingRatio,		-- Flexible context.
	Data.Convertible.Convertible	Database.HDBC.SqlValue timeslotId,		-- Flexible context.
	Data.Typeable.Typeable		teachingRatio,
	Ord				level,
	Ord				synchronisationId,
	Ord				teacherId,
	Ord				timeslotId,
	RealFrac			teachingRatio,
	Show				level,
	Show				synchronisationId,
	Show				timeslotId
 )
	=> connection			-- ^ An abstract database-connection.
	-> Database.HDBC.SqlValue	-- ^ The project-id.
	-> IO (TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio)
fromDatabase connection	projectIdSql	= let
	idealTimeslotRequestColumnName, teacherRegisterIdColumnName :: Database.Selector.ColumnName
	idealTimeslotRequestColumnName	= "idealTimeslotRequest"
	teacherRegisterIdColumnName	= showString tag "Id";

	requiredFacilityTableName, serviceTableName, specialtyTopicTableName, specificTimeRequestsTableName, teacherGroupMembershipTableName, teacherRegisterTableName :: Database.Selector.TableName
	[requiredFacilityTableName, serviceTableName, specialtyTopicTableName, specificTimeRequestsTableName, teacherGroupMembershipTableName, teacherRegisterTableName]	= map (showString Database.Selector.tablePrefix) ["requiredFacility", Data.Teacher.serviceTag, Data.Teacher.specialtyTopicTag, "specificTimeRequest", "teacherGroupMembership", tag]
 in do
	facilityNameByFacilityTypeId	<- Data.Location.findFacilityNameByFacilityTypeId connection projectIdSql

#ifdef USE_HDBC_ODBC
	[
		selectRequiredFacilitiesForTeacherRegisterId,
		selectSpecificTimeRequestsForTeacherRegisterId,
		selectServiceForTeacherRegisterId,
		selectGroupIdsForTeacherRegisterId,
		selectSpecialtyTopicForTeacherRegisterId
	 ] <- mapM (
		\(columnNames, tableName)	-> Database.Selector.prepare connection columnNames [tableName] [teacherRegisterIdColumnName]
	 ) [
		(
			[
				Data.Subject.topicTag,
				Data.Subject.levelTag,
				Data.Location.facilityTypeIdTag
			],
			requiredFacilityTableName
		), (
			[
				Data.Subject.topicTag,
				Data.Subject.levelTag,
				Temporal.Day.tag,
				Database.Selector.timeslotIdColumnName
			],
			specificTimeRequestsTableName
		), (
			[
				Data.Subject.topicTag,
				Data.Subject.levelTag,
				Data.Course.requiredLessonsPerWeekTag,
				Data.Course.minimumConsecutiveLessonsTag,
				Data.Course.maximumClassSizeTag,
				Database.Selector.synchronisationIdColumnName,
				idealTimeslotRequestColumnName
			],
			serviceTableName
		), (
			[Data.Group.groupIdTag],
			teacherGroupMembershipTableName
		), (
			[Data.Subject.topicTag],
			specialtyTopicTableName
		)
	 ] -- Prepare statements for execution with each teacherRegisterId.
#endif /* USE_HDBC_ODBC */
	Database.Selector.select connection [
		teacherRegisterIdColumnName,
		Database.Selector.teacherIdColumnName,
		Temporal.Availability.tag,
		Database.Selector.locationIdColumnName,
		Data.Teacher.maximumTeachingRatioTag,
		Temporal.FreePeriodPreference.tag
	 ] [teacherRegisterTableName] [(Database.Selector.projectIdColumnName, projectIdSql)] >>= fmap Data.Map.fromList . mapM (
		\teacherRow -> case teacherRow of
			[
				teacherRegisterIdSql,
				teacherIdSql,
				availabilitySql,
				locationIdSql,
				maximumTeachingRatioSql,
				freePeriodPreferenceSql
			 ] -> do
#ifndef USE_HDBC_ODBC
				let primaryKey	= [(teacherRegisterIdColumnName, teacherRegisterIdSql)]
#endif
				requiredFacilityNamesBySubject	<- (
					Data.Map.fromListWith Data.Set.union . map (
						\requiredFacilityRow -> case requiredFacilityRow of
							[topicSql, levelSql, facilityTypeIdSql]	-> let
								facilityTypeId	= Data.Maybe.fromMaybe (
									error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tnull " $ shows Data.Location.facilityTypeIdTag "."
								 ) . either (
									error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tfailed to parse the value for " . shows Data.Location.facilityTypeIdTag . showString " read from the database; " . show
								 ) id $ Database.HDBC.safeFromSql facilityTypeIdSql
							 in (
								(,) (Data.Subject.mkSubjectFromSql topicSql levelSql) . Data.Set.singleton
							 ) . Data.Maybe.fromMaybe (
								error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tunknown " . showString Data.Location.facilityTypeIdTag . showChar '=' $ shows facilityTypeId "."
							 ) $ Data.IntMap.lookup facilityTypeId facilityNameByFacilityTypeId
							_					-> error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tunexpected number of columns=" . shows (length requiredFacilityRow) . showString " in row of table " $ shows requiredFacilityTableName "."
					)
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectRequiredFacilitiesForTeacherRegisterId [teacherRegisterIdSql] >> Database.HDBC.fetchAllRows' selectRequiredFacilitiesForTeacherRegisterId
				 )
#else
				 ) `fmap` Database.Selector.select connection [
					Data.Subject.topicTag,
					Data.Subject.levelTag,
					Data.Location.facilityTypeIdTag
				 ] [requiredFacilityTableName] primaryKey
#endif
				specificTimeRequestsBySubject	<- (
					Data.Map.fromListWith Data.Set.union . map (
						\specificTimeRow -> case specificTimeRow of
							[topic, level, day, timeslotId]	-> (
								Data.Subject.mkSubjectFromSql topic level,
								Data.Set.singleton $ Temporal.Time.mkTimeFromSql day timeslotId
							 ) -- Pair
							_				-> error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tunexpected number of columns=" . shows (length specificTimeRow) . showString " in row of table " $ shows specificTimeRequestsTableName "."
					)
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectSpecificTimeRequestsForTeacherRegisterId [teacherRegisterIdSql] >> Database.HDBC.fetchAllRows' selectSpecificTimeRequestsForTeacherRegisterId
				 )
#else
				 ) `fmap` Database.Selector.select connection [
					Data.Subject.topicTag,
					Data.Subject.levelTag,
					Temporal.Day.tag,
					Database.Selector.timeslotIdColumnName
				 ] [specificTimeRequestsTableName] primaryKey
#endif
				service	<- (
					Data.Set.fromList . map (
						\courseRow -> case courseRow of
							[topicSql, levelSql, requiredLessonsPerWeekSql, minimumConsecutiveLessonsSql, maximumClassSizeSql, synchronisationIdSql, idealTimeslotRequestSql]	-> let
								subject	= Data.Subject.mkSubjectFromSql topicSql levelSql
							 in Data.Course.mkCourse subject (
								Data.Maybe.fromMaybe (
									error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tnull " $ shows Data.Course.requiredLessonsPerWeekTag "."
								) . either (
									error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tfailed to parse the value for " . shows Data.Course.requiredLessonsPerWeekTag . showString " read from the database; " . show
								) id $ Database.HDBC.safeFromSql requiredLessonsPerWeekSql
							 ) (
								Data.Maybe.fromMaybe Data.Set.empty $ Data.Map.lookup subject requiredFacilityNamesBySubject
							 ) (
								Data.Maybe.maybe (
									Temporal.TimeslotRequest.Specifically . Data.Maybe.fromMaybe Data.Set.empty $ Data.Map.lookup subject specificTimeRequestsBySubject
								) Temporal.TimeslotRequest.Ideally $ Database.HDBC.fromSql idealTimeslotRequestSql
							 ) (
								Data.Maybe.fromMaybe Data.Course.defaultMinimumConsecutiveLessons . either (
									error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tfailed to parse the value for " . shows Data.Course.minimumConsecutiveLessonsTag . showString " read from the database; " . show
								) id $ Database.HDBC.safeFromSql minimumConsecutiveLessonsSql
							 ) (
								either (
									error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tfailed to parse the value for " . shows Data.Course.maximumClassSizeTag . showString " read from the database; " . show
								) id $ Database.HDBC.safeFromSql maximumClassSizeSql	-- Returns Nothing for SqlNull.
							 ) $ Database.HDBC.fromSql synchronisationIdSql	-- Returns Nothing for SqlNull.
							_	-> error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tunexpected number of columns=" . shows (length courseRow) . showString " in row of table " $ shows serviceTableName "."
					)
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectServiceForTeacherRegisterId [teacherRegisterIdSql] >> Database.HDBC.fetchAllRows' selectServiceForTeacherRegisterId
				 )
#else
				 ) `fmap` Database.Selector.select connection [
					Data.Subject.topicTag,
					Data.Subject.levelTag,
					Data.Course.requiredLessonsPerWeekTag,
					Data.Course.minimumConsecutiveLessonsTag,
					Data.Course.maximumClassSizeTag,
					Database.Selector.synchronisationIdColumnName,
					idealTimeslotRequestColumnName
				 ] [serviceTableName] primaryKey
#endif
				groupMembership	<- (
					Data.Set.fromList . map (
						Data.Maybe.fromMaybe (
							error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tnull " $ shows Data.Group.groupIdTag "."
						) . Database.HDBC.fromSql . head {-select the only column-}
					)
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectGroupIdsForTeacherRegisterId [teacherRegisterIdSql] >> Database.HDBC.fetchAllRows' selectGroupIdsForTeacherRegisterId
				 )
#else
				 ) `fmap` Database.Selector.select connection [Data.Group.groupIdTag] [teacherGroupMembershipTableName] primaryKey
#endif
				maybeSpecialtyTopic	<- (
					Data.Maybe.listToMaybe . map (
						Database.HDBC.fromSql . head {-select the only column-}
					)
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectSpecialtyTopicForTeacherRegisterId [teacherRegisterIdSql] >> Database.HDBC.fetchAllRows' selectSpecialtyTopicForTeacherRegisterId
				 )
#else
				 ) `fmap` Database.Selector.select connection [Data.Subject.topicTag] [specialtyTopicTableName] primaryKey
#endif
				return {-to IO-monad-} (
					Data.Maybe.fromMaybe (
						error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tnull " $ shows Database.Selector.teacherIdColumnName "."
					) $ Database.HDBC.fromSql teacherIdSql,
					Data.Teacher.mkProfile service (
						Database.HDBC.fromSql locationIdSql	-- Returns 'Nothing' for SqlNull.
					) (
						Data.Maybe.fromMaybe (
							error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tnull " $ shows Temporal.Availability.tag "."
						) $ Database.HDBC.fromSql availabilitySql
					) (
						Database.Selector.fromSqlFractional (
							error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tnull " $ shows Data.Teacher.maximumTeachingRatioTag "."
						) maximumTeachingRatioSql
					) groupMembership maybeSpecialtyTopic $ Database.HDBC.fromSql freePeriodPreferenceSql	-- Returns Nothing for SqlNull.
				 ) -- Pair.
			_ -> error . showString "WeekDaze.Aggregate.TeacherRegister.fromDatabase:\tunexpected number of columns=" . shows (length teacherRow) . showString " in row of table " $ shows teacherRegisterTableName "."
	 )
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag	= "teacherRegister"

-- | The complete set of 'Data.Teacher.Profile', indexed by 'teacherId'.
type TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio	= Data.Resource.ResourceMap teacherId (Data.Teacher.Profile synchronisationId level timeslotId locationId teachingRatio)

-- | Accessor.
getTeacherIds :: TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> [teacherId]
getTeacherIds	= Data.Map.keys

-- | Accessor.
getTeacherProfiles :: TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> [Data.Teacher.Profile synchronisationId level timeslotId locationId teachingRatio]
getTeacherProfiles	= Data.Map.elems

-- | Extracts the set of all 'Data.Course.Course's, from the /teacher-register/.
extractDistinctCourses :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 ) => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Data.Teacher.Service synchronisationId level timeslotId
extractDistinctCourses	= Data.Map.foldr (Data.Set.union . Data.Teacher.getService) Data.Set.empty

-- | Extracts the set of distinct 'locationId's, from the /teacher-register/.
extractDistinctOwnLocationIds :: Ord locationId => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Data.Location.Locus locationId
extractDistinctOwnLocationIds	= Data.Map.foldr (\profile locus -> Data.Maybe.maybe locus (`Data.Set.insert` locus) $ Data.Teacher.getMaybeOwnLocationId profile) Data.Set.empty

-- | Extracts the set of all 'Data.Subject.Subject's, from the /teacher-register/.
extractDistinctSubjects :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 ) => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Data.Subject.Knowledge level
extractDistinctSubjects	= Data.Set.map Data.Course.getSubject . extractDistinctCourses

-- | Extracts the set of distinct required 'Data.Location.Facility's, from the /teacher-register/.
extractDistinctRequiredFacilityNames :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 ) => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Data.Location.FacilityNames
extractDistinctRequiredFacilityNames	= Data.Set.foldr (Data.Set.union . Data.Course.getRequiredFacilityNames) Data.Set.empty . extractDistinctCourses

{- |
	* Determine the total number of /lesson/s required for each /subject/, according to the 'Data.Course.Course's offered.

	* Since each /course/ may be offered by more than one 'teacherId', the actual workload can only be constrained within bounds.
-}
calculateWorkloadBoundsBySubject :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 ) => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Data.Map.Map (Data.Subject.Subject level) Temporal.Workload.Bounds
calculateWorkloadBoundsBySubject	= Data.Map.map (minimum &&& maximum) . Data.Map.fromListWith (++) . map (Data.Course.getSubject &&& return {-to List-monad-} . Data.Course.getRequiredLessonsPerWeek) . Data.Set.toList . extractDistinctCourses

-- | Find those /teacher/s offering a /course/ in the specified /subject/.
findSuitableCourseByTeacherId
	:: Eq level
	=> Size.NStudents	-- ^ The number of /student/s, who need to be placed.
	-> Data.Subject.Subject level
	-> TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio
	-> Data.Map.Map teacherId (Data.Course.Course synchronisationId level timeslotId)
findSuitableCourseByTeacherId nStudents subject	= Data.Map.mapMaybe (Data.Teacher.lookupSuitableCourse nStudents subject)

-- | The sum of the number of /day/s worked by each /teacher/.
countAvailableTeacherDays :: TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Size.NDays
countAvailableTeacherDays	= Data.Map.foldr ((+) . Data.Resource.countDaysPerWeekAvailable) 0

{- |
	* The total number of /lesson/s per week, required by all those /course/s requiring each /facility/.

	* CAVEAT: all /course/s are counted, not just those for which there's some demand from /student/s.
-}
countLessonsPerWeekByFacilityName :: TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Data.Map.Map Data.Location.FacilityName Size.NTimeslots
countLessonsPerWeekByFacilityName	= Data.Map.foldr (
	\profile m	-> Data.Set.foldr (
		\course m'	-> Data.Set.foldr (
			Data.Map.insertWith (+) `flip` Data.Course.getRequiredLessonsPerWeek course
		) m' $ Data.Course.getRequiredFacilityNames course
	) m $ Data.Teacher.getService profile
 ) Data.Map.empty

-- | True if a /teacher/ typically inhabits the specified 'locationId'.
isInhabited :: Eq locationId => locationId -> TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Bool
isInhabited locationId	= Data.Foldable.any (Data.Teacher.inhabits locationId)

-- | A map indexed by /teacherId/, of /course/s.
type CoursesByTeacherId synchronisationId teacherId level timeslotId	= Data.Map.Map teacherId (Data.Course.Course synchronisationId level timeslotId)

-- | A map indexed by /synchronisationId/, of 'CoursesByTeacherId'.
type CoursesByTeacherIdBySynchronisationId synchronisationId teacherId level timeslotId	= Data.Map.Map synchronisationId (CoursesByTeacherId synchronisationId teacherId level timeslotId)

{- |
	* Returns /course/s which reference a /synchronisationId/, indexed by that /synchronisationId/, sub-indexed by the /teacher-Id/ who offers them.

	* CAVEAT: If a /teacher/ erroneously offers two /course/s with the same /synchronisationId/, then the second will overwrite the first.

	* CAVEAT: /course/s are returned irrespective of whether any /student/ requires them.
-}
findCoursesByTeacherIdBySynchronisationId :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	teacherId,
	Ord	timeslotId
 ) => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> CoursesByTeacherIdBySynchronisationId synchronisationId teacherId level timeslotId
findCoursesByTeacherIdBySynchronisationId	= Data.Map.foldrWithKey (
	\teacherId -> flip $ Data.Set.foldr (
		uncurry (Data.Map.insertWith Data.Map.union) . Control.Arrow.second (Data.Map.singleton teacherId)
	)
 ) Data.Map.empty . Data.Map.map (
	Data.Set.map (
		Data.Maybe.fromJust . Data.Course.getMaybeSynchronisationId &&& id
	) . Data.Set.filter Data.Course.isSynchronised . Data.Teacher.getService
 )

-- | Returns the set of /course/s corresponding to each /synchronisationId/, irrespective of whether they're required by any /student/.
findDistinctCoursesBySynchronisationId :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 ) => CoursesByTeacherIdBySynchronisationId synchronisationId teacherId level timeslotId -> Data.Map.Map synchronisationId (Data.Set.Set (Data.Course.Course synchronisationId level timeslotId))
findDistinctCoursesBySynchronisationId	= Data.Map.map $ Data.Set.fromList . Data.Map.elems

-- | True if any /teacher/ has specified a /free-period preference/.
hasAnyFreePeriodPreference :: RealFrac teachingRatio => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Bool
hasAnyFreePeriodPreference	= Data.Foldable.any Data.HumanResource.hasFreePeriodPreference

-- | True if any /teacher/ has offered a /synchronised course/ in their /service/.
hasAnySynchronisedCourses :: TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Bool
hasAnySynchronisedCourses	= Data.Foldable.any Data.Teacher.offersAnySynchronisedCourse

-- | True if any /teacher/ has requested an /ideal timeslot/ in their /service/.
hasAnyIdealTimeslotRequests :: TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Bool
hasAnyIdealTimeslotRequests	= Data.Foldable.any Data.Teacher.hasAnyIdealTimeslotRequest

-- | True if any /teacher/ has specified a /maximum class-size/ in their /service/.
hasAnyCourseMaximumClassSizes :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	timeslotId
 ) => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Bool
hasAnyCourseMaximumClassSizes	= Data.Foldable.any (Data.Maybe.isJust . Data.Course.getMaybeMaximumClassSize) . extractDistinctCourses

-- | True if any /teacher/ has requested a /specific time/ in their /service/.
hasAnySpecificTimeRequests :: TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Bool
hasAnySpecificTimeRequests	= Data.Foldable.any Data.Teacher.hasAnySpecificTimeRequest

-- | True if any /teacher/ has requested a /specific time/ in their /service/.
hasAnyTimeslotRequests :: TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Bool
hasAnyTimeslotRequests	= Data.Foldable.any (uncurry (||) . (Data.Teacher.hasAnyIdealTimeslotRequest &&& Data.Teacher.hasAnySpecificTimeRequest))

-- | True if any /teacher/ has requested an /ideal timeslot/ in their /service/.
hasAnySpecialists :: TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Bool
hasAnySpecialists	= Data.Foldable.any Data.Teacher.hasSpecialtyTopic

-- | Find the set of all specified /time/s, for any /course/, offered by any /teacher/.
findSpecifiedTimes :: Ord timeslotId => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> Temporal.Time.TimeSet timeslotId
findSpecifiedTimes	= Data.Map.foldr (Data.Set.union . Data.Teacher.findSpecifiedTimes) Data.Set.empty

-- | The type returned by 'subjectsOfferedInMultipleCoursesRequiringDifferentLessonsPerWeek'.
type NTimeslotsByTeacherIdBySubject level teacherId	= Data.Map.Map (Data.Subject.Subject level) (Data.Map.Map teacherId Size.NTimeslots)

-- | Finds /subject/s offered by more than one /teacher/, but as /course/s requiring different /lesson/s per week.
findSubjectsOfferedInMultipleCoursesRequiringDifferentLessonsPerWeek
	:: (Ord level, Ord teacherId)
	=> TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio
	-> NTimeslotsByTeacherIdBySubject level teacherId
findSubjectsOfferedInMultipleCoursesRequiringDifferentLessonsPerWeek	= Data.Map.filter (
	(> 1) . Data.Set.size . Data.Set.fromList . Data.Map.elems {-lessonsPerWeek-}
 ) . Data.Map.foldrWithKey (
	\teacherId teacherProfile m	-> Data.Set.foldr (
		\course -> uncurry (
			Data.Map.insertWith Data.Map.union
		) (
			Data.Course.getSubject &&& Data.Map.singleton teacherId . Data.Course.getRequiredLessonsPerWeek $ course
		)
	) m $ Data.Teacher.getService teacherProfile
 ) Data.Map.empty

-- | Tightens the constraints on each member of a set of synchronised /course/s, to a level compatible with the requirements of other members.
mergeConstraintsOnSynchronisedCourses :: (
	Ord	level,
	Ord	synchronisationId,
	Ord	teacherId,
	Ord	timeslotId,
	Show	synchronisationId,
	Show	timeslotId
 ) => TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio -> TeacherRegister teacherId synchronisationId level timeslotId locationId teachingRatio
mergeConstraintsOnSynchronisedCourses teacherRegister	= Data.Map.map (
	\profile -> profile {
		Data.Teacher.getService	= Data.Set.map (
			\course -> Data.Maybe.maybe course (
				\synchronisationId -> let
					distinctCourses	= findDistinctCoursesBySynchronisationId (findCoursesByTeacherIdBySynchronisationId teacherRegister) ! synchronisationId
				in course {
					Data.Course.getMinimumConsecutiveLessons	= Data.Set.findMax $ Data.Set.map Data.Course.getMinimumConsecutiveLessons distinctCourses,
					Data.Course.getTimeslotRequest			= let
						timeslotRequests			= Data.Set.map Data.Course.getTimeslotRequest distinctCourses
						(idealTimeslotIds, specifiedTimes)	= (
							Data.Set.map (Data.Maybe.fromJust . Temporal.TimeslotRequest.getMaybeIdealTimeslotId) *** Data.Set.unions . map Temporal.TimeslotRequest.getSpecifiedTimes . Data.Set.toList
						 ) $ Data.Set.partition Temporal.TimeslotRequest.isIdeally timeslotRequests

						hasZeroIdealTimeslotIds, hasZeroSpecifiedTimes :: Bool
						hasZeroIdealTimeslotIds	= Data.Set.null idealTimeslotIds
						hasZeroSpecifiedTimes	= Data.Set.null specifiedTimes
					in if hasZeroIdealTimeslotIds && hasZeroSpecifiedTimes
						then Data.Course.getTimeslotRequest course	-- Unchanged.
						else if Data.Set.size idealTimeslotIds == 1 && hasZeroSpecifiedTimes
							then Temporal.TimeslotRequest.Ideally $ Data.Set.findMin idealTimeslotIds
							else if hasZeroIdealTimeslotIds && not hasZeroSpecifiedTimes
								then Temporal.TimeslotRequest.Specifically specifiedTimes
								else error . showString "WeekDaze.Aggregate.TeacherRegister.mergeConstraintsOnSynchronisedCourses:\tincompatible TimeslotRequests; " $ shows (synchronisationId, Data.Set.toList timeslotRequests) "."
				}
			 ) $ Data.Course.getMaybeSynchronisationId course
		) $ Data.Teacher.getService profile
	}
 ) teacherRegister

