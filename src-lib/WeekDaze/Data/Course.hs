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

 [@DESCRIPTION@]	Describes the attributes of a /course/; <https://en.wikipedia.org/wiki/Course_(education)>.
-}

module WeekDaze.Data.Course(
-- * Types
-- ** Data-types
	Course(
--		MkCourse,
		getSubject,
		getRequiredLessonsPerWeek,
		getRequiredFacilityNames,
		getTimeslotRequest,
		getMinimumConsecutiveLessons,
		getMaybeMaximumClassSize,
		getMaybeSynchronisationId
	),
-- * Constants
--	tag,
	maximumClassSizeTag,
	minimumConsecutiveLessonsTag,
	requiredFacilitiesTag,
	requiredLessonsPerWeekTag,
	defaultMinimumConsecutiveLessons,
-- * Functions
	countStudentPlaces,
	calculateIdealConsecutiveLessons,
-- ** Constructor
	mkCourse,
-- ** Predicates
	isSuitable,
	specifiesTimes,
	isASpecifiedTime,
	isASpecifiedDay,
	isFluid,
	isRigid,
	hasRigidlySpecifiedDays,
	requiresConsecutiveLessons,
--	countImplicitlySpecifiedLessons,
	requestsSeparatedTimelotsWithinAnyDay,
	isSynchronised
) where

import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Data.Location			as Data.Location
import qualified	WeekDaze.Data.Subject			as Data.Subject
import qualified	WeekDaze.Size				as Size
import qualified	WeekDaze.Temporal.Day			as Temporal.Day
import qualified	WeekDaze.Temporal.Time			as Temporal.Time
import qualified	WeekDaze.Temporal.TimeslotRequest	as Temporal.TimeslotRequest

-- | Used to qualify Output & XML.
tag :: String
tag				= "course"

-- | Used to qualify Output, SQL & XML.
minimumConsecutiveLessonsTag :: String
minimumConsecutiveLessonsTag	= "minimumConsecutiveLessons"

-- | Used to qualify Output, SQL & XML.
maximumClassSizeTag :: String
maximumClassSizeTag		= "maximumClassSize"

-- | Used to qualify Output & XML.
requiredFacilitiesTag :: String
requiredFacilitiesTag		= "requiredFacilities"

-- | Used to qualify Output, SQL & XML.
requiredLessonsPerWeekTag :: String
requiredLessonsPerWeekTag	= "requiredLessonsPerWeek"

-- | The default value for 'getMinimumConsecutiveLessons'.
defaultMinimumConsecutiveLessons :: Size.NTimeslots
defaultMinimumConsecutiveLessons	= 1

-- | The attributes of a /course/.
data Course synchronisationId level timeslotId	= MkCourse {
	getSubject			:: Data.Subject.Subject level,
	getRequiredLessonsPerWeek	:: Size.NTimeslots,					-- ^ The /subject/ is assumed to take a minimum time-duration to teach, & for ease of scheduling, we assume that this is divided into a constant number of /lesson/s per week.
	getRequiredFacilityNames	:: Data.Location.FacilityNames,				-- ^ The specific strings used to designate these, must match those in 'Data.Location.Profile'.
	getTimeslotRequest		:: Temporal.TimeslotRequest.TimeslotRequest timeslotId,	-- ^ Either the /ideal time-slot/ or the specified /times/, at which to book /lesson/s in this /course/.
	getMinimumConsecutiveLessons	:: Size.NTimeslots,					-- ^ Some /topic/s may require too great a preparation-time for an isolated /lesson/ to be practical; e.g. "games", or possibly "cookery". The specified value represents an ideal number of periods, which can be exceeded if necessary.
	getMaybeMaximumClassSize	:: Maybe Size.NStudents,				-- ^ The optional upper bound on the number of /student/s for whom the /teacher/ can cater; which is independent of 'Data.Location.getCapacity'.
	getMaybeSynchronisationId	:: Maybe synchronisationId				-- ^ The optional identifier, shared by /course/s whose /booking/s must be synchronised.
} deriving (Eq, Ord)

instance (Show synchronisationId, Show level, Show timeslotId) => Show (Course synchronisationId level timeslotId) where
	showsPrec _ MkCourse {
		getSubject			= subject,
		getRequiredLessonsPerWeek	= requiredLessonsPerWeek,
		getRequiredFacilityNames	= requiredFacilityNames,
		getTimeslotRequest		= timeslotRequest,
		getMinimumConsecutiveLessons	= minimumConsecutiveLessons,
		getMaybeMaximumClassSize	= maybeMaximumClassSize,
		getMaybeSynchronisationId	= maybeSynchronisationId
	} = foldr (.) (
		showChar '}'	-- Initial value.
	 ) . (
		showString (tag ++ "={") :	-- Delimiter.
	 ) . Data.List.intersperse (
		showString ", "	-- Separator.
	 ) $ [
		showString Data.Subject.tag . showChar '=' . shows subject,
		showString requiredLessonsPerWeekTag . showChar '=' . shows requiredLessonsPerWeek,
		showString requiredFacilitiesTag . showChar '=' . shows (Data.Set.toList requiredFacilityNames),
		shows timeslotRequest,
		showString minimumConsecutiveLessonsTag . showChar '=' . shows minimumConsecutiveLessons
	 ] ++ Data.Maybe.catMaybes [
		fmap (\s -> showString maximumClassSizeTag . showChar '=' . shows s) maybeMaximumClassSize,
		fmap shows maybeSynchronisationId
	 ]

instance (
#if !MIN_VERSION_containers(0,5,2)
	Ord	timeslotId,
#endif
	Show	level,
	Show	synchronisationId,
	Show	timeslotId
 ) => ToolShed.SelfValidate.SelfValidator (Course synchronisationId level timeslotId) where
	getErrors course@MkCourse {
		getRequiredLessonsPerWeek	= requiredLessonsPerWeek,
		getTimeslotRequest		= timeslotRequest,
		getMinimumConsecutiveLessons	= minimumConsecutiveLessons,
		getMaybeMaximumClassSize	= maybeMaximumClassSize
	} = ToolShed.SelfValidate.extractErrors [
		(
			requiredLessonsPerWeek <= 0,
			show requiredLessonsPerWeekTag ++ " must exceed zero; " ++ show course
		), (
			minimumConsecutiveLessons <= 0,
			show minimumConsecutiveLessonsTag ++ " must exceed zero; " ++ show course
		), (
			requiredLessonsPerWeek < max minimumConsecutiveLessons (Temporal.TimeslotRequest.countSpecifiedTimes timeslotRequest),
			show requiredLessonsPerWeekTag ++ " must be greater than or equal to both, '" ++ minimumConsecutiveLessonsTag ++ "', & the number of specified times; " ++ show course
		), (
			requiredLessonsPerWeek < Temporal.TimeslotRequest.countSpecifiedDays timeslotRequest * minimumConsecutiveLessons,
			show requiredLessonsPerWeekTag ++ " must be greater than or equal to '" ++ minimumConsecutiveLessonsTag ++ "' multiplied by the number of distinct days on which times are specified; " ++ show course
		), (
			Data.Maybe.maybe False (<= 0) maybeMaximumClassSize,
			show maximumClassSizeTag ++ " must exceed zero; " ++ show course
		)
	 ]

instance (
	HXT.XmlPickler	level,
	HXT.XmlPickler	synchronisationId,
	HXT.XmlPickler	timeslotId,
	Ord		timeslotId,
	Show		level,
	Show		synchronisationId,
	Show		timeslotId
 ) => HXT.XmlPickler (Course synchronisationId level timeslotId) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f, g)	-> mkCourse a b c d e f g,	-- Construct from a tuple.
		\MkCourse {
			getSubject			= subject,
			getRequiredLessonsPerWeek	= requiredLessonsPerWeek,
			getRequiredFacilityNames	= requiredFacilityNames,
			getTimeslotRequest		= timeslotRequest,
			getMinimumConsecutiveLessons	= minimumConsecutiveLessons,
			getMaybeMaximumClassSize	= maybeMaximumClassSize,
			getMaybeSynchronisationId	= maybeSynchronisationId
		} -> (
			subject,
			requiredLessonsPerWeek,
			requiredFacilityNames,
			timeslotRequest,
			minimumConsecutiveLessons,
			maybeMaximumClassSize,
			maybeSynchronisationId
		) -- Deconstruct to a tuple.
	 ) $ HXT.xp7Tuple HXT.xpickle {-subject-} (
		HXT.xpAttr requiredLessonsPerWeekTag HXT.xpInt
	 ) (
		HXT.xpDefault Data.Location.defaultFacilityNames . HXT.xpElem requiredFacilitiesTag . HXT.xpWrap (
			Data.Set.fromList,	-- Construct from a List.
			Data.Set.toList		-- Deconstruct to a List.
		) . HXT.xpList1 {-the default is null-} . HXT.xpElem Data.Location.facilityNameTag $ HXT.xpTextAttr Data.Location.facilityValueTag {-can't be null-}
	 ) HXT.xpickle {-timeslotRequest-} (
		defaultMinimumConsecutiveLessons `HXT.xpDefault` HXT.xpAttr minimumConsecutiveLessonsTag HXT.xpInt
	 ) (
		HXT.xpAttrImplied maximumClassSizeTag HXT.xpInt
	 ) (
		HXT.xpOption HXT.xpickle	-- maybeSynchronisationId.
	 )

instance (
	Control.DeepSeq.NFData	level,
	Control.DeepSeq.NFData	synchronisationId,
	Control.DeepSeq.NFData	timeslotId
 ) => Control.DeepSeq.NFData (Course synchronisationId level timeslotId) where
	rnf (MkCourse x0 x1 x2 x3 x4 x5 x6)	= Control.DeepSeq.rnf (x0, x1, x2, x3, x4, x5, x6)

-- | Smart constructor.
mkCourse :: (
	Show	level,
	Show	synchronisationId,
	Show	timeslotId
 )
	=> Data.Subject.Subject level				-- ^ The /topic/ & /level/ at which it is to be taught.
	-> Size.NTimeslots					-- ^ The required number of /time-slot/s per week.
	-> Data.Location.FacilityNames				-- ^ The set of things required for the /course/.
	-> Temporal.TimeslotRequest.TimeslotRequest timeslotId	-- ^ The /ideal time-slot/ or specified /times/, at which to book /lesson/s in this /course/.
	-> Size.NTimeslots					-- ^ The minimum number of consecutive /time-slot/s required for any booking in this /subject/.
	-> Maybe Size.NStudents					-- ^ The maximum class-size.
	-> Maybe synchronisationId				-- ^ The optional identifier of a set of /course/s, whose /lesson/s must be synchronised.
	-> Course synchronisationId level timeslotId
mkCourse subject requiredLessonsPerWeek facilityNames timeslotRequest minimumConsecutiveLessons maybeMaximumClassSize maybeSynchronisationId
	| ToolShed.SelfValidate.isValid course	= course
	| otherwise				= error $ "WeekDaze.Data.Course.mkCourse:\t" ++ ToolShed.SelfValidate.getFirstError course ++ "."
	where
		course	= MkCourse {
			getSubject			= subject,
			getRequiredLessonsPerWeek	= requiredLessonsPerWeek,
			getRequiredFacilityNames	= facilityNames,
			getTimeslotRequest		= timeslotRequest,
			getMinimumConsecutiveLessons	= minimumConsecutiveLessons,
			getMaybeMaximumClassSize	= maybeMaximumClassSize,
			getMaybeSynchronisationId	= maybeSynchronisationId
		}

-- | Get the number of /student/-places, when accounting for the capacity-limits imposed by both the /course/ & the /location/; but not for any /student/s already booked.
countStudentPlaces :: Course synchronisationId level timeslotId -> Data.Location.Profile campus -> Size.NStudents
countStudentPlaces course	= (\locationCapacity -> Data.Maybe.maybe locationCapacity (min locationCapacity) $ getMaybeMaximumClassSize course) . Data.Location.getCapacity

{- |
	* The /required lessons per week/ would typically be an integral multiple of the /minimum consecutive lessons/,
	but otherwise some sessions must be longer to absorb the fractional remainder.

	* Ideally the required number of /lesson/s would be divided equally amongst the maximum possible number of sessions,
	to make the session-durations as equal as possible.

	* This function returns that ideal mean fractional duration, of the integral session-durations.
	Eg:

>	requiredLessonsPerWeek	minimumConsecutiveLessons	isolated sessions	ideal mean session-duration
>	======================	=========================	=================	===========================
>	7			2				3			7/3 == 2.33
>	7			3				2			7/2 == 3.5
>	7			4				1			7
>	11			3				3			11/3 == 3.67

	* CAVEAT: when the ideal is non-integral, it can never be achieved in practice;
	deviations from this ideal of less than half a /time-slot/, are therefore insignificant.
-}
calculateIdealConsecutiveLessons :: Fractional f => Course synchronisationId level timeslotId -> f
calculateIdealConsecutiveLessons course
	| minimumConsecutiveLessons == 1	= 1
	| maximumSeparateSessions == 0		= error "WeekDaze.Data.Course.calculateIdealConsecutiveLessons:\tattempt to divide by zero (maximum separate sessions)."
	| otherwise				= fromIntegral requiredLessonsPerWeek / fromIntegral maximumSeparateSessions	-- Divide the required number of lessons evenly over the maximum possible number of sessions.
	where
		(requiredLessonsPerWeek, minimumConsecutiveLessons)	= getRequiredLessonsPerWeek &&& getMinimumConsecutiveLessons $ course
		maximumSeparateSessions					= requiredLessonsPerWeek `div` minimumConsecutiveLessons

-- | 'True' if the /course/ is in the required /subject/ & its /teacher/ can cater for the required number of /student/s.
isSuitable :: Eq level
	=> Size.NStudents
	-> Data.Subject.Subject level
	-> Course synchronisationId level timeslotId
	-> Bool
isSuitable requiredClassSize requiredSubject course = getSubject course == requiredSubject && Data.Maybe.maybe True (>= requiredClassSize) (getMaybeMaximumClassSize course)

-- | True if /time/s have been specified.
specifiesTimes :: Course synchronisationId level timeslotId -> Bool
specifiesTimes	= not . Temporal.TimeslotRequest.isNull . getTimeslotRequest

-- | True if the /course/ requests /booking/ of a /lesson/ at the specified /time/.
isASpecifiedTime :: Ord timeslotId => Temporal.Time.Time timeslotId -> Course synchronisationId level timeslotId -> Bool
isASpecifiedTime time	= Temporal.TimeslotRequest.isASpecifiedTime time . getTimeslotRequest

-- | True if the one of the specified /time/s for the /course/, falls on the specified /day/.
isASpecifiedDay :: Temporal.Day.Day -> Course synchronisationId level timeslotId -> Bool
isASpecifiedDay day	= Temporal.TimeslotRequest.isASpecifiedDay day . getTimeslotRequest

-- | True if zero /lesson/s of the /course/ are to be booked at /specified time/s.
isFluid :: Course synchronisationId level timeslotId -> Bool
isFluid	= Temporal.TimeslotRequest.isNull . getTimeslotRequest

-- | True if all /lesson/s of the /course/ are required to be booked at /specified time/s.
isRigid :: Course synchronisationId level timeslotId -> Bool
isRigid	= uncurry (==) . (getRequiredLessonsPerWeek &&& Temporal.TimeslotRequest.countSpecifiedTimes . getTimeslotRequest)

{- |
	* True if the /day/s on which /lesson/s must be booked, is known.

	* CAVEAT: this result doesn't account for the case where multiple times have been specified for one /day/,
	but can't be booked as one span of /minimumConsecutiveLessons/ because of the extent of the gaps between.
-}
hasRigidlySpecifiedDays ::
#if !MIN_VERSION_containers(0,5,2)
	Ord	timeslotId =>
#endif
	Course synchronisationId level timeslotId -> Bool
hasRigidlySpecifiedDays course	= isRigid course || getRequiredLessonsPerWeek course == Temporal.TimeslotRequest.countSpecifiedDays (getTimeslotRequest course) * getMinimumConsecutiveLessons course

-- | True if all /booking/s require more than one consecutive identical /lesson/.
requiresConsecutiveLessons :: Course synchronisationId level timeslotId -> Bool
requiresConsecutiveLessons	= (> 1) . getMinimumConsecutiveLessons

-- | Count the total number of /lesson/s implied by specifically requested times, assuming that a split session isn't acceptable.
countImplicitlySpecifiedLessons :: (Enum timeslotId, Ord timeslotId) => Course synchronisationId level timeslotId -> Size.NTimeslots
countImplicitlySpecifiedLessons course	= Data.Foldable.foldr (
	(+) . max (
		getMinimumConsecutiveLessons course	-- Any span between specified times, which is shorter than this must be elongated.
	) . succ {-fence-post-} . uncurry Temporal.Time.calculateAbsoluteDistance . (
		Data.Set.findMax &&& Data.Set.findMin	-- Delimit the span between specified times.
	) -- Assuming that a split session is not permissible, count the lessons implied by the specifically requested times on this day.
 ) 0 . Temporal.Time.categoriseByDay . Temporal.TimeslotRequest.getSpecifiedTimes $ getTimeslotRequest course

-- | True if the /course/ specifies times which can't be satisfied without splitting the session in at least one /day/.
requestsSeparatedTimelotsWithinAnyDay :: (Enum timeslotId, Ord timeslotId) => Course synchronisationId level timeslotId -> Bool
requestsSeparatedTimelotsWithinAnyDay	= uncurry (>) . (
	countImplicitlySpecifiedLessons &&& getRequiredLessonsPerWeek	-- If so, then the assumption in 'countImplicitlySpecifiedLessons' that a split-session isn't acceptable, is no longer tenable.
 )

{- |
	* True if the /course/ references a /synchronisationId/.

	* CAVEAT: doesn't account for the possibility that this is the only /course/ to reference this specified /synchronisationId/.
-}
isSynchronised :: Course synchronisationId level timeslotId -> Bool
isSynchronised	= Data.Maybe.isJust . getMaybeSynchronisationId

