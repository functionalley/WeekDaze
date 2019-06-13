{-# LANGUAGE CPP #-}
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

	* Defines the attributes of a /location/, in terms of its; /capacity/, /facilities/, whether it's /off-site/, & its /availability/.

	* It refers to a more general concept than a merely a /class-room/, including for example, an assembly-hall or a games-field.
-}

module WeekDaze.Data.Location(
-- * Types
-- ** Type-synonyms
	FacilityName,
	FacilityNames,
	WastedResources,
	Locus,
-- ** Data-types
	Profile(
--		MkProfile,
		getCapacity,
		getFacilityNames,
		getAvailability,
		getCampus
	),
-- * Constants
--	tag,
	capacityTag,
--	facilitiesTag,
	facilityTypeIdTag,
	facilityNameTag,
	facilityValueTag,
--	facilityTypeTableName,
	defaultFacilityNames,
-- * Functions
	calculateWaste,
	calculateRelativeWaste,
	calculateRelativeUse,
#ifdef USE_HDBC
	findFacilityNameByFacilityTypeId,
#endif
-- ** Accessors
--	getWastedCapacity,
	getWastedFacilityNames,
-- ** Constructor
	mkProfile,
-- ** Predicates
	hasFacilities,
	isSuitable
) where

import			Control.Arrow((&&&), (***))
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.Set
import			Data.Set((\\))
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	Text.XML.HXT.DOM.Util
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Data.Resource		as Data.Resource
import qualified	WeekDaze.Size			as Size
import qualified	WeekDaze.Temporal.Availability	as Temporal.Availability

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.IntMap
import qualified	WeekDaze.Database.Selector	as Database.Selector

-- | Used to qualify XML.
facilityTypeTableName :: Database.Selector.TableName
facilityTypeTableName	= Database.Selector.tablePrefix ++ "facilityType"

-- | Queries the database to build a map of /facilityName/s by /facilityTypeId/.
findFacilityNameByFacilityTypeId
	:: Database.HDBC.IConnection	connection
	=> connection
	-> Database.HDBC.SqlValue	-- ^ ProjectId
	-> IO (Data.IntMap.IntMap FacilityName)
findFacilityNameByFacilityTypeId connection projectIdSql	= (
	Data.IntMap.fromList . map (
		\row -> case row of
			[facilityTypeIdSql, facilityNameSql]	-> (
				either (
					error . showString "WeekDaze.Data.Location.findFacilityNameByFacilityTypeId:\tfailed to parse the value for " . shows facilityTypeIdTag . showString " read from the database; " . show
				) id $ Database.HDBC.safeFromSql facilityTypeIdSql,
				Database.HDBC.fromSql facilityNameSql
			 ) -- Pair.
			_					-> error $ "WeekDaze.Data.Location.findFacilityNameByFacilityTypeId:\tunexpected number of columns=" ++ show (length row) ++ " in row of table " ++ show facilityTypeTableName ++ "."
	)
 ) `fmap` Database.Selector.select connection [facilityTypeIdTag, facilityNameTag] [facilityTypeTableName] [(Database.Selector.projectIdColumnName, projectIdSql)]
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag			= "locationProfile"

-- | Used to qualify SQL & XML.
capacityTag :: String
capacityTag		= "capacity"

-- | Used to qualify XML.
facilitiesTag :: String
facilitiesTag		= "facilities"

-- | Used to qualify SQL & XML.
facilityTypeIdTag :: String
facilityTypeIdTag	= "facilityTypeId"

-- | Used to qualify SQL & XML.
facilityNameTag :: String
facilityNameTag		= "facilityName"

-- | Used to qualify XML.
facilityValueTag :: String
facilityValueTag	= "value"

{- |
	* A /subject/-specific /facility/, e.g. "desks", "musical instruments", or "goal-posts".

	* The specific string used to designate these must match those defined for /course/s.
-}
type FacilityName	= String

-- | The set of facility-names advertised by a /location/.
type FacilityNames	= Data.Set.Set FacilityName

-- | The default value for /facilityNames/.
defaultFacilityNames :: FacilityNames
defaultFacilityNames	= Data.Set.empty

-- | Aggregates the attributes of a /location/, which a generalisation of a room, since it also includes games-fields.
data Profile campus	= MkProfile {
	getCapacity		:: Size.NStudents,			-- ^ Typically the number of seats. N.B.: though the games-field has a rather ill-defined /capacity/, the changing-rooms don't.
	getFacilityNames	:: FacilityNames,			-- ^ The /location/ may advertise /facilities/ to support specific activities.
	getAvailability		:: Temporal.Availability.Availability,	-- ^ The /location/ may have a periodic /availability/.
	getCampus		:: campus				-- ^ Proximate /locations/ are grouped into /campus/es, between which intra-day migration is trivial.
} deriving (Eq, Show)

instance Show campus => ToolShed.SelfValidate.SelfValidator (Profile campus) where
	getErrors profile@MkProfile {
		getAvailability	= availability
	}
		| not $ ToolShed.SelfValidate.isValid availability	= ToolShed.SelfValidate.getErrors availability
		| otherwise						= ToolShed.SelfValidate.extractErrors [
			(getCapacity profile <= 0,	show capacityTag ++ " must exceed zero; " ++ show profile)
		]

instance Data.Resource.Resource (Profile campus) where
	getAvailability	= getAvailability

instance (
	Data.Default.Default			campus,
	Eq					campus,
	HXT.XmlPickler				campus,
	Show					campus
 ) => HXT.XmlPickler (Profile campus) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		Text.XML.HXT.DOM.Util.uncurry4 mkProfile,	-- Construct from a quadruple.
		\MkProfile {
			getCapacity		= capacity,
			getFacilityNames	= facilityNames,
			getAvailability		= availability,
			getCampus		= campus
		} -> (
			capacity,
			facilityNames,
			availability,
			campus
		) -- Deconstruct to a quadruple.
	 ) $ HXT.xp4Tuple (
		HXT.xpAttr capacityTag HXT.xpInt
	 ) (
		HXT.xpDefault defaultFacilityNames . HXT.xpElem facilitiesTag . HXT.xpWrap (
			Data.Set.fromList,	-- Construct from a List.
			Data.Set.toList		-- Deconstruct to a List.
		) . HXT.xpList1 {-the default is null-} . HXT.xpElem facilityNameTag $ HXT.xpTextAttr facilityValueTag {-can't be null-}
	 ) (
		HXT.xpDefault Data.Default.def HXT.xpickle	-- Availability.
	 ) (
		HXT.xpDefault Data.Default.def HXT.xpickle	-- Campus.
	 )

instance Control.DeepSeq.NFData	campus => Control.DeepSeq.NFData (Profile campus) where
	rnf (MkProfile x0 x1 x2 x3)	= Control.DeepSeq.rnf (x0, x1, x2, x3)

-- | Smart constructor.
mkProfile
	:: Show campus
	=> Size.NStudents
	-> FacilityNames
	-> Temporal.Availability.Availability
	-> campus
	-> Profile campus
mkProfile capacity facilityNames availability campus
	| ToolShed.SelfValidate.isValid profile	= profile
	| otherwise				= error $ "WeekDaze.Data.Location.mkProfile:\t" ++ ToolShed.SelfValidate.getFirstError profile ++ "."
	where
		profile	= MkProfile capacity facilityNames availability campus

{- |
	* True if the 'Profile' meets or exceeds the specified criteria.

	* Takes no account of either /availability/ or /campus/.
-}
isSuitable
	:: Size.NStudents
	-> FacilityNames
	-> Profile campus
	-> Bool
isSuitable requiredCapacity requiredFacilityNames profile	= requiredCapacity <= getCapacity profile && requiredFacilityNames `Data.Set.isSubsetOf` getFacilityNames profile

-- | True if the /location/ has some facilities.
hasFacilities :: Profile campus -> Bool
hasFacilities	= not . Data.Set.null . getFacilityNames

-- | A measure of the /capacity/ & specific /facilities/, which are available at a /location/, but are unused in some context.
type WastedResources	= (Size.NStudents, FacilityNames)

-- | Accessor.
getWastedCapacity :: WastedResources -> Size.NStudents
getWastedCapacity	= fst

-- | Accessor.
getWastedFacilityNames :: WastedResources -> FacilityNames
getWastedFacilityNames	= snd

{- |
	* Measures the excess /capacity/ & unused /facilities/, of the specified /location/.

	* All /facilities/ are considered equal.

	* CAVEAT: each /location/ must have previously passed 'isSuitable'.
-}
calculateWaste
	:: Size.NStudents	-- ^ The required /capacity/.
	-> FacilityNames	-- ^ The required /facilities/.
	-> Profile campus	-- ^ The /location/ whose /resources/ are to be compared with requirements.
	-> WastedResources
calculateWaste requiredCapacity requiredFacilities	= subtract requiredCapacity . getCapacity &&& (\\ requiredFacilities) . getFacilityNames

-- | Convert 'WastedResources' to a pair of ratios.
calculateRelativeWaste :: Fractional relativeWaste => Profile campus -> WastedResources -> (relativeWaste, relativeWaste)
calculateRelativeWaste profile wastedResources	= (
	fromIntegral (getWastedCapacity wastedResources) / fromIntegral (getCapacity profile),
	let
		nFacilities	= Data.Set.size $ getFacilityNames profile
	in if nFacilities == 0
		then 0	-- If the location hasn't any facilities, none can be wasted.
		else fromIntegral (Data.Set.size $ getWastedFacilityNames wastedResources) / fromIntegral nFacilities
 ) -- Pair.

-- | The relative extent to which /capacity/ & /facilities/ are used.
calculateRelativeUse :: Fractional relativeUse => Profile campus -> WastedResources -> (relativeUse, relativeUse)
calculateRelativeUse profile	= ((1 -) *** (1 -)) . calculateRelativeWaste profile

-- | A collection of distinct /location-ids/.
type Locus locationId	= Data.Set.Set locationId
