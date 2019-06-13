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

 [@DESCRIPTION@]	Defines the name & attributes, of all /group/s.
-}

module WeekDaze.Aggregate.GroupCatalogue(
-- * Types
-- ** Type-synonyms
	GroupCatalogue,
	ResourceIdsByGroupId,
-- * Constants
	tag,
-- * Functions
	extractDistinctMeetingLocationIds,
	getMeetingTimes
#ifdef USE_HDBC
-- ** Translation
	,fromDatabase
#endif
) where

import qualified	Data.Map
import			Data.Map((!))
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	WeekDaze.Data.Group		as Data.Group
import qualified	WeekDaze.Data.Location		as Data.Location
import qualified	WeekDaze.Data.Resource		as Data.Resource
import qualified	WeekDaze.Temporal.Time		as Temporal.Time

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	WeekDaze.Database.Selector	as Database.Selector
import qualified	WeekDaze.Temporal.Day		as Temporal.Day

-- | Construct from the specified database-connection.
fromDatabase :: (
	Database.HDBC.IConnection	connection,
	Data.Convertible.Convertible	Database.HDBC.SqlValue locationId,	-- Flexible context.
	Data.Convertible.Convertible	Database.HDBC.SqlValue timeslotId,	-- Flexible context.
	Ord				timeslotId
 )
	=> connection			-- ^ An abstract database-connection.
	-> Database.HDBC.SqlValue	-- ^ The project-id.
	-> IO (GroupCatalogue timeslotId locationId)
fromDatabase connection	projectIdSql	= let
	groupCatalogueIdColumnName :: Database.Selector.ColumnName
	groupCatalogueIdColumnName	= showString tag "Id";

	groupCatalogueTableName, meetingTimesTableName :: Database.Selector.TableName
	groupCatalogueTableName	= showString Database.Selector.tablePrefix tag
	meetingTimesTableName	= showString Database.Selector.tablePrefix "meetingTime"
 in do
#ifdef USE_HDBC_ODBC
	selectMeetingTimesForGroupCatalogueId	<- Database.Selector.prepare connection [Temporal.Day.tag, Database.Selector.timeslotIdColumnName] [meetingTimesTableName] [groupCatalogueIdColumnName]
#endif
	Database.Selector.select connection [
		groupCatalogueIdColumnName,
		Data.Group.groupIdTag,
		Database.Selector.locationIdColumnName,
		Data.Group.mandatesAttendanceTag
	 ] [groupCatalogueTableName] [(Database.Selector.projectIdColumnName, projectIdSql)] >>= fmap Data.Map.fromList . mapM (
		\groupRow -> case groupRow of
			[groupCatalogueIdSql, groupIdSql, locationIdSql, mandatesAttendanceSql]	-> do
				meetingTimes	<- (
					Data.Set.fromList . map (
						\meetingTimesRow -> case meetingTimesRow of
							[day, timeslotId]	-> Temporal.Time.mkTimeFromSql day timeslotId
							_			-> error . showString "WeekDaze.Aggregate.GroupCatalogue.fromDatabase:\tunexpected number of columns=" . shows (length meetingTimesRow) . showString " in row of table " . shows meetingTimesTableName . showString ", where " . showString Data.Group.groupIdTag . showChar '=' $ shows groupIdSql "."
					)
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectMeetingTimesForGroupCatalogueId [groupCatalogueIdSql] >> Database.HDBC.fetchAllRows' selectMeetingTimesForGroupCatalogueId
				 )
#else
				 ) `fmap` Database.Selector.select connection [
					Temporal.Day.tag,
					Database.Selector.timeslotIdColumnName
				 ] [meetingTimesTableName] [(groupCatalogueIdColumnName, groupCatalogueIdSql)]
#endif
				return {-to IO-monad-} (
					Data.Maybe.fromMaybe (
						error . showString "WeekDaze.Aggregate.GroupCatalogue.fromDatabase:\tnull " $ shows Data.Group.groupIdTag "."
					) $ Database.HDBC.fromSql groupIdSql,
					Data.Group.mkProfile meetingTimes (
						Database.HDBC.fromSql locationIdSql	-- Returns Nothing for SqlNull.
					) $ Data.Maybe.fromMaybe Data.Group.defaultMandatesAttendance . either (
						error . showString "WeekDaze.Aggregate.GroupCatalogue.fromDatabase:\tfailed to parse the value for " . shows Data.Group.mandatesAttendanceTag . showString " read from the database; " . show
					) id $ Database.HDBC.safeFromSql mandatesAttendanceSql
				 ) -- Pair.
			_						-> error . showString "WeekDaze.Aggregate.GroupCatalogue.fromDatabase:\tunexpected number of columns=" . shows (length groupRow) . showString " in row of table " $ shows groupCatalogueTableName "."
	 )
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag	= "groupCatalogue"

-- | The complete set of 'Data.Group.Profile', indexed by 'Data.Group.Id'.
type GroupCatalogue timeslotId locationId	= Data.Resource.ResourceMap Data.Group.Id (Data.Group.Profile timeslotId locationId)

-- | A map indexed by a /group/-id, of /resource-id/s.
type ResourceIdsByGroupId resourceId	= Data.Map.Map Data.Group.Id (Data.Set.Set resourceId)

-- | Extracts the set of distinct 'locationId's, from the /group-catalogue/.
extractDistinctMeetingLocationIds :: Ord locationId => GroupCatalogue timeslotId locationId -> Data.Location.Locus locationId
extractDistinctMeetingLocationIds	= Data.Map.foldr (\profile locus -> Data.Maybe.maybe locus (`Data.Set.insert` locus) $ Data.Group.getMaybeLocationId profile) Data.Set.empty

-- | Gather the set of /meeting-time/s corresponding to the specified set of /group/s.
getMeetingTimes :: Ord timeslotId => GroupCatalogue timeslotId locationId -> Data.Group.Membership -> Temporal.Time.TimeSet timeslotId
getMeetingTimes groupCatalogue	= Data.Set.foldr (Data.Set.union . Data.Group.getMeetingTimes . (groupCatalogue !)) Data.Set.empty

