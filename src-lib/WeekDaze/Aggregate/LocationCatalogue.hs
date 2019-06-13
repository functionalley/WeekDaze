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

 [@DESCRIPTION@]	Defines the names & attributes of all /location/s available to be booked in the /timetable/.

-}

module WeekDaze.Aggregate.LocationCatalogue(
-- * Types
-- ** Type-synonyms
	LocationCatalogue,
-- * Constants
	tag,
-- * Functions
	countDaysByFacilityName,
--	countDistinctCampuses,
	extractDistinctFacilityNames,
	findSuitableLocations,
-- ** Accessors
	getLocationIds,
#ifdef USE_HDBC
-- ** Constructor
	fromDatabase,
#endif
-- ** Predicates
	hasAnyFacilities,
	isSingleCampus
) where

import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Set
import qualified	WeekDaze.Data.Location		as Data.Location
import qualified	WeekDaze.Data.Resource		as Data.Resource
import qualified	WeekDaze.Size			as Size

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Default
import qualified	Data.IntMap
import qualified	Data.Maybe
import qualified	WeekDaze.Database.Selector	as Database.Selector
import qualified	WeekDaze.Temporal.Availability	as Temporal.Availability

{- |
	* Construct from the specified database-connection.

	* CAVEAT: though the database may not permit a null value for many fields (applying its own default value when the value is unspecified),
	default values are applied here should the SQL-query return one.
-}
fromDatabase :: (
	Database.HDBC.IConnection		connection,
	Data.Convertible.Convertible		Database.HDBC.SqlValue campus,		-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue locationId,	-- Flexible context.
	Data.Default.Default			campus,
	Ord					locationId,
	Show					campus
 )
	=> connection			-- ^ An abstract database-connection.
	-> Database.HDBC.SqlValue	-- ^ The project-id.
	-> IO (LocationCatalogue locationId campus)
fromDatabase connection	projectIdSql	= let
	locationCatalogueIdColumnName :: Database.Selector.ColumnName
	locationCatalogueIdColumnName	= showString tag "Id";

	facilityTableName, locationCatalogueTableName :: Database.Selector.TableName
	facilityTableName		= showString Database.Selector.tablePrefix "facility"
	locationCatalogueTableName	= showString Database.Selector.tablePrefix tag
 in do
	facilityNameByFacilityTypeId			<- Data.Location.findFacilityNameByFacilityTypeId connection projectIdSql

#ifdef USE_HDBC_ODBC
	selectFacilityTypeIdsForLocationCatalogueId	<- Database.Selector.prepare connection [Data.Location.facilityTypeIdTag] [facilityTableName] [locationCatalogueIdColumnName]	-- Select the facilityTypeIds at a locationId to be defined. CAVEAT: prepared statements don't seem to work reliably with HDBC-mysql.
#endif
	Database.Selector.select connection [
		locationCatalogueIdColumnName,
		Database.Selector.locationIdColumnName,
		Temporal.Availability.tag,
		Data.Location.capacityTag,
		Database.Selector.campusColumnName
	 ] [locationCatalogueTableName] [(Database.Selector.projectIdColumnName, projectIdSql)] >>= fmap Data.Map.fromList . mapM (
		\locationRow -> case locationRow of
			[locationCatalogueIdSql, locationIdSql, availabilitySql, capacitySql, campusSql]	-> do
				facilityNames	<- map (
					\locationCatalogueRow -> case locationCatalogueRow of
						[facilityTypeIdSql]	-> let
							facilityTypeId	= Data.Maybe.fromMaybe (
								error . showString "WeekDaze.Aggregate.LocationCatalogue.fromDatabase:\tnull " $ shows Data.Location.facilityTypeIdTag "."
							 ) . either (
								error . showString "WeekDaze.Aggregate.LocationCatalogue.fromDatabase:\tfailed to parse the value for " . shows Data.Location.facilityTypeIdTag . showString " read from the database; " . show
							 ) id $ Database.HDBC.safeFromSql facilityTypeIdSql
						 in Data.Maybe.fromMaybe (
							error . showString "WeekDaze.Aggregate.LocationCatalogue.fromDatabase:\tunknown " . showString Data.Location.facilityTypeIdTag . showChar '=' $ shows facilityTypeId "."
						 ) $ Data.IntMap.lookup facilityTypeId facilityNameByFacilityTypeId
						_			-> error . showString "WeekDaze.Aggregate.LocationCatalogue.fromDatabase:\tunexpected number of columns=" . shows (length locationCatalogueRow) . showString" in row of table " $ shows facilityTableName "."
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectFacilityTypeIdsForLocationCatalogueId [locationCatalogueIdSql] >> Database.HDBC.fetchAllRows' selectFacilityTypeIdsForLocationCatalogueId
				 )
#else
				 ) `fmap` Database.Selector.select connection [Data.Location.facilityTypeIdTag] [facilityTableName] [(locationCatalogueIdColumnName, locationCatalogueIdSql)]	-- Select the facilityTypeIds at this locationId.
#endif
				return {-to IO-monad-} (
					Data.Maybe.fromMaybe (
						error . showString "WeekDaze.Aggregate.LocationCatalogue.fromDatabase:\tnull " $ shows Database.Selector.locationIdColumnName "."
					) $ Database.HDBC.fromSql locationIdSql,
					Data.Location.mkProfile (
						Data.Maybe.fromMaybe (
							error . showString "WeekDaze.Aggregate.LocationCatalogue.fromDatabase:\tnull " $ shows Data.Location.capacityTag "."
						) . either (
							error . showString "WeekDaze.Aggregate.LocationCatalogue.fromDatabase:\tfailed to parse the value for " . shows Data.Location.capacityTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql capacitySql
					) (
						Data.Set.fromList facilityNames
					) (
						Data.Maybe.fromMaybe Data.Default.def $ Database.HDBC.fromSql availabilitySql
					) . Data.Maybe.fromMaybe Data.Default.def $ Database.HDBC.fromSql campusSql
				 ) -- Pair.
			_											-> error . showString "WeekDaze.Aggregate.LocationCatalogue.fromDatabase:\tunexpected number of columns=" . shows (length locationRow) . showString " in row of table " $ shows locationCatalogueTableName "."
	 )
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag	= "locationCatalogue"

-- | The complete set of /location/s, indexed by their 'locationId'.
type LocationCatalogue locationId campus	= Data.Resource.ResourceMap locationId (Data.Location.Profile campus)

-- | Accessor.
getLocationIds :: LocationCatalogue locationId campus -> [locationId]
getLocationIds	= Data.Map.keys

-- | Count the total available /day/s, of those /location/s offering each type of /facility/.
countDaysByFacilityName :: LocationCatalogue locationId campus -> Data.Map.Map Data.Location.FacilityName Size.NDays
countDaysByFacilityName	= Data.Map.foldr (
	\profile m	-> Data.Set.foldr (
		\facilityName -> Data.Map.insertWith (+) facilityName (Data.Resource.countDaysPerWeekAvailable profile)
	) m $ Data.Location.getFacilityNames profile
 ) Data.Map.empty

-- | Count the number of distinct /campus/es which have been configured.
countDistinctCampuses :: Ord campus => LocationCatalogue locationId campus -> Int
countDistinctCampuses	= Data.Set.size . Data.Foldable.foldr (Data.Set.insert . Data.Location.getCampus) Data.Set.empty

-- | Whether all /location/s exist on the same /campus/.
isSingleCampus :: Ord campus => LocationCatalogue locationId campus -> Bool
isSingleCampus	= (== 1) . countDistinctCampuses

-- | Extracts the set of distinct /facilities/, from the catalogue.
extractDistinctFacilityNames :: LocationCatalogue locationId campus -> Data.Location.FacilityNames
extractDistinctFacilityNames	= Data.Map.foldr (Data.Set.union . Data.Location.getFacilityNames) Data.Set.empty

-- | Find those /locations/ which meet or exceed, the specified criteria.
findSuitableLocations
	:: Size.NStudents
	-> Data.Location.FacilityNames
	-> LocationCatalogue locationId campus
	-> LocationCatalogue locationId campus
findSuitableLocations requiredCapacity requiredFacilityNames	= Data.Map.filter (Data.Location.isSuitable requiredCapacity requiredFacilityNames)

-- | True if any /location/ offers any /facilities/.
hasAnyFacilities :: LocationCatalogue locationId campus -> Bool
hasAnyFacilities	= Data.Foldable.any Data.Location.hasFacilities

