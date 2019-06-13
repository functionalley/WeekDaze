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

 [@DESCRIPTION@]	Describes the attributes of a /teacher/.

 [@TODO@]	Some teachers work half days, or even just specific /time-slot/s; so 'getWorkingWeek' is an inadequate description.
-}

module WeekDaze.Data.Teacher(
-- * Types
-- ** Type-synonyms
	Service,
-- ** Data-types
	Profile(
--		MkProfile,
		getService,
		getMaybeOwnLocationId,
--		getWorkingWeek,
--		getMaximumTeachingRatio,
		getMaybeSpecialtyTopic,
--		getGroupMembership,
		getMaybeFreePeriodPreference
	),
-- * Constants
--	tag,
	maximumTeachingRatioTag,
	serviceTag,
	specialtyTopicTag,
--	defaultService,
-- * Functions
	lookupCourseIn,
	lookupSuitableCourse,
	findSpecifiedTimes,
	unsubscribe,
-- ** Constructor
	mkProfile,
-- ** Predicates
	offersSuitableCourse,
	hasAnyIdealTimeslotRequest,
	hasAnySpecificTimeRequest,
	hasSpecialtyTopic,
	inhabits,
	isSpecialistIn,
	offersAnySynchronisedCourse,
	offersService
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Data.Foldable
import qualified	Data.List.Extra
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Data.Course			as Data.Course
import qualified	WeekDaze.Data.Group			as Data.Group
import qualified	WeekDaze.Data.HumanResource		as Data.HumanResource
import qualified	WeekDaze.Data.Resource			as Data.Resource
import qualified	WeekDaze.Data.Subject			as Data.Subject
import qualified	WeekDaze.Size				as Size
import qualified	WeekDaze.Temporal.Availability		as Temporal.Availability
import qualified	WeekDaze.Temporal.FreePeriodPreference	as Temporal.FreePeriodPreference
import qualified	WeekDaze.Temporal.Time			as Temporal.Time
import qualified	WeekDaze.Temporal.TimeslotRequest	as Temporal.TimeslotRequest

-- | Used to qualify XML.
tag :: String
tag			= "teacherProfile"

-- | Used to qualify SQL & XML.
maximumTeachingRatioTag :: String
maximumTeachingRatioTag	= "maximumTeachingRatio"

-- | Used to qualify XML.
serviceTag :: String
serviceTag		= "service"

-- | Used to qualify XML.
specialtyTopicTag :: String
specialtyTopicTag	= "specialtyTopic"

{- |
	* A /teacher/ offers a set of /course/s.

	* Whilst they may perform a reserve-role in some additional /subject/, this isn't going to affect the /timetable/.
-}
type Service synchronisationId level timeslotId	= Data.Set.Set (Data.Course.Course synchronisationId level timeslotId)

-- | The default value for 'getService'.
defaultService :: Service synchronisationId level timeslotId
defaultService	= Data.Set.empty

-- | The service offered by a /teacher/.
data Profile synchronisationId level timeslotId locationId teachingRatio	= MkProfile {
	getService			:: Service synchronisationId level timeslotId,			-- ^ The set of /course/s offered.

{- |
	* A /teacher/ may have their own personal /location/ (i.e. a /form-room/).

	* Absence of this field implies that they're nomadic, & may use any free 'locationId', appropriate for the /course/; however inconveniently located.

	* A personal /location/ can't be assumed to have all the /facilities/ required for the /service/ offered.

	* CAVEAT: it's conceivable that some /teacher/s share a /location/, but each one still only uses that /location/.
-}
	getMaybeOwnLocationId		:: Maybe locationId,						-- ^ A /teacher/ may have a fixed abode.
	getWorkingWeek			:: Temporal.Availability.Availability,				-- ^ A /teacher/ may be part-time.
{- |
	* The maximum ratio, in the closed unit-interval [0,1], of a /teacher/'s working week (rather than the whole week), which is devoted to teaching.
	The remainder may be administration or attendance at /meetings/.
	This concept may also be required, to allow ad-hoc re-scheduling, to compensate for absentees.

	* This concept differs from a /part-time teacher/, who teaches for a static /contractually specified/ portion of the week.

	* When the type-parameter is a /Rational/ & the denominator is the number of /lesson/s per week, specification is in the basic time-units of the timetable.

	* The value can't be exceeded, but may not be reached, thus allowing at least the required time for the administration.
-}
	getMaximumTeachingRatio		:: teachingRatio,
	getGroupMembership		:: Data.Group.Membership,					-- ^ A /teacher/ may meet regularly with various /group/s.
	getMaybeSpecialtyTopic		:: Maybe Data.Subject.Topic,					-- ^ A /teacher/ may specialise in a specific /topic/, rather than being a generalist.
	getMaybeFreePeriodPreference	:: Maybe Temporal.FreePeriodPreference.FreePeriodPreference	-- ^ A /teacher/ may have a preference for the position within each /day/, of unallocated /time-slot/s.
} deriving (Eq, Ord)

instance (Show synchronisationId, Show level, Show timeslotId, Show locationId, Show teachingRatio) => Show (Profile synchronisationId level timeslotId locationId teachingRatio) where
	showsPrec _ MkProfile {
		getService			= service,
		getMaybeOwnLocationId		= maybeOwnLocationId,
		getWorkingWeek			= workingWeek,
		getMaximumTeachingRatio		= maximumTeachingRatio,
		getGroupMembership		= groupMembership,
		getMaybeSpecialtyTopic		= maybeSpecialtyTopic,
		getMaybeFreePeriodPreference	= maybeFreePeriodPreference
	} = showString tag . showString "={" . showString serviceTag . showChar '=' . shows (
		Data.Set.toList service
	 ) . showString ",locationId=" . showString (
		Data.Maybe.maybe "<none>" show maybeOwnLocationId
	 ) . showChar ',' . showString Temporal.Availability.tag . showChar '=' . shows workingWeek . showChar ',' . showString maximumTeachingRatioTag . showChar '=' . shows maximumTeachingRatio . showChar ',' . showString Data.HumanResource.groupMembershipTag . showChar '=' . shows (
		Data.Set.toList groupMembership
	 ) . showChar ',' . showString specialtyTopicTag . showChar '=' . showString (
		Data.Maybe.maybe "<none>" show maybeSpecialtyTopic
	 ) . showChar ',' . showString Temporal.FreePeriodPreference.tag . showChar '=' . showString (
		Data.Maybe.maybe "<none>" show maybeFreePeriodPreference
	 ) . showChar '}'

instance (
	Ord	level,
#if !MIN_VERSION_containers(0,5,2)
	Ord	synchronisationId,
#endif
	Ord	timeslotId,
	Real	teachingRatio,
	Show	level,
	Show	synchronisationId,
	Show	timeslotId
 ) => ToolShed.SelfValidate.SelfValidator (Profile synchronisationId level timeslotId locationId teachingRatio) where
	getErrors profile@MkProfile {
		getService		= service,
		getWorkingWeek		= workingWeek,
		getMaximumTeachingRatio	= maximumTeachingRatio,
		getMaybeSpecialtyTopic	= maybeSpecialtyTopic
	}
		| not $ ToolShed.SelfValidate.isValid service		= ToolShed.SelfValidate.getErrors service
		| not $ ToolShed.SelfValidate.isValid workingWeek	= ToolShed.SelfValidate.getErrors workingWeek
		| otherwise						= ToolShed.SelfValidate.extractErrors [
			(
				Data.List.Extra.anySame . map Data.Course.getSubject $ Data.Set.toList service,
				"multiple courses in the same " ++ show Data.Subject.tag ++ " have been offered in this teacher's " ++ show serviceTag ++ "; " ++ show (Data.Set.toList service)
			), (
				any ($ maximumTeachingRatio) [(< 0), (> 1)],
				maximumTeachingRatioTag ++ "=" ++ show (realToFrac maximumTeachingRatio :: Double {-hide the actual type-}) ++ " must be within the closed unit-interval '[0,1]'"	-- A staff-member can be purely administrative, since they could be a member of a group.
			), (
				maybeSpecialtyTopic == Just "",
				"explicitly null " ++ show specialtyTopicTag
			), (
				Data.Maybe.maybe False (`Data.Set.notMember` Data.Set.map (Data.Subject.getTopic . Data.Course.getSubject) service) maybeSpecialtyTopic,
				"a teacher's " ++ show specialtyTopicTag ++ " " ++ show (Data.Maybe.fromJust maybeSpecialtyTopic) ++ " must be offered as a " ++ show serviceTag ++ "; " ++ show (Data.Set.toList service)
			), (
				Data.List.Extra.anySame $ Data.Foldable.concatMap (Data.Set.toList . Temporal.TimeslotRequest.getSpecifiedTimes . Data.Course.getTimeslotRequest) service,
				"there's an overlap in the " ++ show Temporal.TimeslotRequest.specificallyTag ++ " requested booking-times, of the courses offered in the " ++ show serviceTag ++ "; " ++ show (Data.Set.toList service)
			), (
				uncurry (&&) $ (offersService &&& (== 0) . getMaximumTeachingRatio) profile,
				"a portion of the working week must be allocated to teaching " ++ show (Data.Set.toList service)
			), (
				uncurry (&&) $ (not . offersService &&& (> 0) . getMaximumTeachingRatio) profile,
				maximumTeachingRatioTag ++ "=" ++ show (realToFrac maximumTeachingRatio :: Double {-hide the actual type-}) ++ " of " ++ Temporal.Availability.tag ++ "=" ++ show workingWeek ++ " can't be filled without offering a service"
			)
		]

instance Data.Resource.Resource (Profile synchronisationId level timeslotId locationId teachingRatio) where
	getAvailability	= getWorkingWeek

instance RealFrac teachingRatio => Data.HumanResource.HumanResource (Profile synchronisationId level timeslotId locationId teachingRatio) where
	getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay		= round . uncurry (*) . (getMaximumTeachingRatio &&& fromIntegral . Data.HumanResource.calculateNTimeslotsPerWeekAvailable nTimeslotsPerDay)
	getNTimeslotsPerWeekOfNonTeaching nTimeslotsPerDay	= uncurry (-) . (
		Data.HumanResource.calculateNTimeslotsPerWeekAvailable nTimeslotsPerDay &&& Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay
	 ) -- Derivation from 'getNTimeslotsPerWeekOfTeaching' ensures they are compatible in the face of rounding.
	getGroupMembership					= getGroupMembership
	getMaybeFreePeriodPreference				= getMaybeFreePeriodPreference

instance (
	HXT.XmlPickler	level,
	HXT.XmlPickler	locationId,
	HXT.XmlPickler	synchronisationId,
	HXT.XmlPickler	teachingRatio,
	HXT.XmlPickler	timeslotId,
	Ord		level,
	Ord		synchronisationId,
	Ord		timeslotId,
	Real		teachingRatio,
	Show		level,
	Show		synchronisationId,
	Show		timeslotId
 ) => HXT.XmlPickler (Profile synchronisationId level timeslotId locationId teachingRatio) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, g)	-> mkProfile a b c d e f g,	-- Construct from a tuple.
		\MkProfile {
			getService			= service,
			getMaybeOwnLocationId		= maybeOwnLocationId,
			getWorkingWeek			= workingWeek,
			getMaximumTeachingRatio		= maximumTeachingRatio,
			getGroupMembership		= groupMembership,
			getMaybeSpecialtyTopic		= maybeSpecialtyTopic,
			getMaybeFreePeriodPreference	= maybeFreePeriodPreference
		} -> (
			service,
			maybeOwnLocationId,
			workingWeek,
			maximumTeachingRatio,
			groupMembership,
			maybeSpecialtyTopic,
			maybeFreePeriodPreference
		) -- Deconstruct to a tuple.
	 ) $ HXT.xp7Tuple (
		HXT.xpDefault defaultService . HXT.xpElem serviceTag . HXT.xpWrap (
			Data.Set.fromList,	-- Construct from a List.
			Data.Set.toList		-- Deconstruct to a List.
		) $ HXT.xpList1 {-the default is null-} HXT.xpickle
	 ) (
		HXT.xpOption HXT.xpickle	-- maybeOwnLocationId.
	 ) HXT.xpickle {-workingWeek-} (
		HXT.xpAttr maximumTeachingRatioTag HXT.xpickle
	 ) (
		HXT.xpDefault Data.HumanResource.defaultGroupMembership . HXT.xpElem Data.HumanResource.groupMembershipTag . HXT.xpWrap (
			Data.Set.fromList,	-- Construct from a List.
			Data.Set.toList		-- Deconstruct to a List.
		) . HXT.xpList1 {-the default is null-} . HXT.xpElem Data.Group.memberTag $ HXT.xpTextAttr Data.Group.groupIdTag
	 ) (
		HXT.xpOption $ HXT.xpTextAttr specialtyTopicTag {-can't be null-}
	 ) (
		HXT.xpOption HXT.xpickle	-- maybeFreePeriodPreference.
	 )

instance (
	Control.DeepSeq.NFData	level,
	Control.DeepSeq.NFData	locationId,
	Control.DeepSeq.NFData	synchronisationId,
	Control.DeepSeq.NFData	teachingRatio,
	Control.DeepSeq.NFData	timeslotId
 ) => Control.DeepSeq.NFData (Profile synchronisationId level timeslotId locationId teachingRatio) where
	rnf (MkProfile x0 x1 x2 x3 x4 x5 x6)	= Control.DeepSeq.rnf (x0, x1, x2, x3, x4, x5, x6)

-- | Smart constructor.
mkProfile :: (
	Ord	level,
	Ord	timeslotId,
	Real	teachingRatio,
	Show	level,
	Show	synchronisationId,
	Show	timeslotId
 )
	=> Service synchronisationId level timeslotId			-- ^ The set of /course/s offered.
	-> Maybe locationId						-- ^ Personal classroom.
	-> Temporal.Availability.Availability				-- ^ The /day/s on which the /teacher/ is contracted to work.
	-> teachingRatio						-- ^ The maximum ratio of a working-week actually devoted to teaching.
	-> Data.Group.Membership					-- ^ The /group/s of which this /teacher/ is a member.
	-> Maybe Data.Subject.Topic					-- ^ Specialty topic.
	-> Maybe Temporal.FreePeriodPreference.FreePeriodPreference	-- ^ Any preference for the position within each /day/, of free /time-slot/s.
	-> Profile synchronisationId level timeslotId locationId teachingRatio
mkProfile service maybeOwnLocationId workingWeek maximumTeachingRatio groupMembership maybeSpecialtyTopic maybeFreePeriodPreference
	| ToolShed.SelfValidate.isValid profile	= profile
	| otherwise				= error $ "WeekDaze.Data.Teacher.mkProfile:\t" ++ ToolShed.SelfValidate.getFirstError profile ++ "."
	where
		profile	= MkProfile {
			getService			= service,
			getMaybeOwnLocationId		= maybeOwnLocationId,
			getWorkingWeek			= workingWeek,
			getMaximumTeachingRatio		= maximumTeachingRatio,
			getGroupMembership		= groupMembership,
			getMaybeSpecialtyTopic		= maybeSpecialtyTopic,
			getMaybeFreePeriodPreference	= maybeFreePeriodPreference
		}

-- | Extract any /course/ from the specified /profile/, which matches the specified /subject/.
lookupCourseIn
	:: Eq level
	=> Data.Subject.Subject level
	-> Profile synchronisationId level timeslotId locationId teachingRatio
	-> Maybe (Data.Course.Course synchronisationId level timeslotId)
lookupCourseIn subject	= Data.Foldable.find ((== subject) . Data.Course.getSubject) . getService

-- | Extract any /course/ from the specified /profile/, which matches the specified /subject/ & caters for the required number of /student/s.
lookupSuitableCourse
	:: Eq level
	=> Size.NStudents
	-> Data.Subject.Subject level
	-> Profile synchronisationId level timeslotId locationId teachingRatio
	-> Maybe (Data.Course.Course synchronisationId level timeslotId)
lookupSuitableCourse nStudents subject	= Data.Foldable.find (Data.Course.isSuitable nStudents subject) . getService

-- | Find the set of all specified /time/s, for any of the /course/s offered.
findSpecifiedTimes :: Ord timeslotId => Profile synchronisationId level timeslotId locationId teachingRatio -> Temporal.Time.TimeSet timeslotId
findSpecifiedTimes	= Data.Set.foldr (Data.Set.union . Temporal.TimeslotRequest.getSpecifiedTimes . Data.Course.getTimeslotRequest) Data.Set.empty . getService

-- | True if the /teacher/ offers a /course/ in the required /subject/ & can cater for the required number of /student/s.
offersSuitableCourse
	:: Eq level
	=> Size.NStudents
	-> Data.Subject.Subject level
	-> Profile synchronisationId level timeslotId locationId teachingRatio
	-> Bool
offersSuitableCourse nStudents subject	= Data.Foldable.any (Data.Course.isSuitable nStudents subject) . getService

-- | True if the specified /teacher/ makes any /ideal timeslot-request/ amongst the /course/s offered in their /service/.
hasAnyIdealTimeslotRequest :: Profile synchronisationId level timeslotId locationId teachingRatio -> Bool
hasAnyIdealTimeslotRequest	= Data.Foldable.any (Temporal.TimeslotRequest.isIdeally . Data.Course.getTimeslotRequest) . getService

-- | True if the specified /teacher/ makes any /specific time-request/ amongst the /course/s offered in their /service/.
hasAnySpecificTimeRequest :: Profile synchronisationId level timeslotId locationId teachingRatio -> Bool
hasAnySpecificTimeRequest	= Data.Foldable.any Data.Course.specifiesTimes . getService

-- | True if the specified /teacher/ claims any /topic/ as their specialty.
hasSpecialtyTopic :: Profile synchronisationId level timeslotId locationId teachingRatio -> Bool
hasSpecialtyTopic	= Data.Maybe.isJust . getMaybeSpecialtyTopic

-- | True if the specified /teacher/ claims the specified /location/ as their own.
inhabits :: Eq locationId => locationId -> Profile synchronisationId level timeslotId locationId teachingRatio -> Bool
inhabits locationId	= (== Just locationId) . getMaybeOwnLocationId

-- | True if the /teacher/ specializes in the specified /topic/.
isSpecialistIn :: Data.Subject.Topic -> Profile synchronisationId level timeslotId locationId teachingRatio -> Bool
isSpecialistIn topic	= (== Just topic) . getMaybeSpecialtyTopic

-- | True if the specified /teacher/ offers any synchronised /course/.
offersAnySynchronisedCourse :: Profile synchronisationId level timeslotId locationId teachingRatio -> Bool
offersAnySynchronisedCourse	= Data.Foldable.any Data.Course.isSynchronised . getService

-- | True if the /teacher/ offers one or more /course/s in their /service/.
offersService :: Profile synchronisationId level timeslotId locationId teachingRatio -> Bool
offersService	= not . Data.Set.null . getService

-- | Unsubscribe from the specified set of /groups/.
unsubscribe
	:: Data.Group.Membership
	-> Profile synchronisationId level timeslotId locationId teachingRatio
	-> Profile synchronisationId level timeslotId locationId teachingRatio
unsubscribe groupMembership profile	= profile {
	getGroupMembership	= Data.Set.filter (`Data.Set.notMember` groupMembership) $ getGroupMembership profile
}
