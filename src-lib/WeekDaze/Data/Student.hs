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

 [@DESCRIPTION@]	Describes the attributes of a /student/.
-}

module WeekDaze.Data.Student(
-- * Types
-- ** Type-synonyms
	Id,
	KnowledgeRequirements,
-- ** Data-types
	Profile(
--		MkProfile,
		getStream,
		getKnowledgeRequirements,
--		getWorkingWeek,
		getTeachingRatio,
--		getGroupMembership,
		getMaybeFreePeriodPreference
	),
-- * Constants
--	tag,
	knowledgeRequirementsTag,
--	streamTag,
	teachingRatioTag,
	defaultTeachingRatio,
-- * Functions
	amalgamateKnowledgeRequirements,
	deriveAmalgamatedKnowledgeRequirement,
	unsubscribe,
-- ** Constructor
	mkProfile,
-- ** Predicates
	hasAnyCoreKnowledgeRequirements,
	hasAnyOptionalKnowledgeRequirements,
	requiresAnySubjectBy
) where

import			Control.Arrow((&&&), (***))
import qualified	Control.DeepSeq
import qualified	Data.Default
import qualified	Data.Foldable
import qualified	Data.Maybe
import qualified	Data.Set
import qualified	Text.XML.HXT.Arrow.Pickle			as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Data.Group				as Data.Group
import qualified	WeekDaze.Data.HumanResource			as Data.HumanResource
import qualified	WeekDaze.Data.Requirements			as Data.Requirements
import qualified	WeekDaze.Data.Resource				as Data.Resource
import qualified	WeekDaze.Data.Subject				as Data.Subject
import qualified	WeekDaze.Temporal.Availability			as Temporal.Availability
import qualified	WeekDaze.Temporal.FreePeriodPreference	as Temporal.FreePeriodPreference

-- | Used to qualify XML.
tag :: String
tag				= "studentProfile"

-- | Used to qualify SQL & XML.
streamTag :: String
streamTag			= "stream"

-- | Used to qualify XML.
knowledgeRequirementsTag :: String
knowledgeRequirementsTag	= "knowledgeRequirements"

-- | Used to qualify SQL & XML.
teachingRatioTag :: String
teachingRatioTag		= "teachingRatio"

-- | The default value for 'getTeachingRatio'.
defaultTeachingRatio :: Num teachingRatio => teachingRatio
defaultTeachingRatio	= 1

{- |
	* A unique identifier, which may be merely their name, or more likely a registration-number of some unspecified format.

	* Though arbitrary, a /string/ adequately covers all possibilities.
-}
type Id	= String

-- | A specification of /subject/-requirements, partitioned into those considered /core/ & the /optional/ remainder.
type KnowledgeRequirements level	= Data.Requirements.Requirements (Data.Subject.Knowledge level)

-- | A combination of the /core/ & /optional/ /knowledge-requirements/.
amalgamateKnowledgeRequirements :: Ord level => KnowledgeRequirements level -> Data.Subject.Knowledge level
amalgamateKnowledgeRequirements	= uncurry Data.Set.union

-- | The attributes of a /student/.
data Profile level stream teachingRatio	= MkProfile {
	getStream			:: stream,							-- ^ The common academic year & potentially also the topic-independent ability-stream of the member-/student/s.
	getKnowledgeRequirements	:: KnowledgeRequirements level,					-- ^ The core & optional /subject/s required for the education of the member-/student/s.
	getWorkingWeek			:: Temporal.Availability.Availability,				-- ^ /Student/s are typically unavailable at the weekends.
	getTeachingRatio		:: teachingRatio,						-- ^ The ratio in the semi-closed unit-interval (0,1], of the time when the member-/student/s are available, which is allocated to tuition; the remainder by inference being allocated to free study (as often required by final year /student/s), or /meeting/s.
	getGroupMembership		:: Data.Group.Membership,					-- ^ The member-/student/s may meet regularly with various /group/s.
	getMaybeFreePeriodPreference	:: Maybe Temporal.FreePeriodPreference.FreePeriodPreference	-- ^ Any preference for the position within each /day/, of unallocated /time-slot/s.
} deriving (Eq, Ord)

instance (Show level, Show stream, Show teachingRatio) => Show (Profile level stream teachingRatio) where
	showsPrec _ MkProfile {
		getStream			= stream,
		getKnowledgeRequirements	= knowledgeRequirements,
		getWorkingWeek			= workingWeek,
		getTeachingRatio		= teachingRatio,
		getGroupMembership		= groupMembership,
		getMaybeFreePeriodPreference	= maybeFreePeriodPreference
	} = showString tag . showString "={" . showString streamTag . showChar '=' . shows stream . showChar ',' . showString knowledgeRequirementsTag . showChar '=' . shows (
		Data.Set.toList *** Data.Set.toList $ knowledgeRequirements
	 ) . showChar ',' . showString Temporal.Availability.tag . showChar '=' . shows workingWeek . showChar ',' . showString teachingRatioTag . showChar '=' . shows teachingRatio . showChar ',' . showString Data.HumanResource.groupMembershipTag . showChar '=' . shows (
		Data.Set.toList groupMembership
	 ) . showChar ',' . showString Temporal.FreePeriodPreference.tag . showChar '=' . showString (
		Data.Maybe.maybe "<none>" show maybeFreePeriodPreference
	 ) . showChar '}'

instance (
	Ord	level,
	Real	teachingRatio,
	Show	level
 ) => ToolShed.SelfValidate.SelfValidator (Profile level stream teachingRatio) where
	getErrors MkProfile {
		getKnowledgeRequirements	= knowledgeRequirements,
		getWorkingWeek			= workingWeek,
		getTeachingRatio		= teachingRatio
	}
		| not $ ToolShed.SelfValidate.isValid workingWeek	= ToolShed.SelfValidate.getErrors workingWeek
		| otherwise						= ToolShed.SelfValidate.extractErrors [
			let
				duplicateKnowledgeRequirements	= uncurry Data.Set.intersection knowledgeRequirements
			in (
				not $ Data.Set.null duplicateKnowledgeRequirements,
				"overlapping " ++ show knowledgeRequirementsTag ++ "; " ++ show duplicateKnowledgeRequirements
			), (
				any ($ teachingRatio) [(<= 0), (> 1)],
				show teachingRatioTag ++ " '" ++ show (realToFrac teachingRatio :: Double {-hide the actual type-}) ++ "' must be within the semi-closed unit-interval '(0,1]'"
			)
		]

instance Data.Resource.Resource (Profile level stream teachingRatio) where
	getAvailability	= getWorkingWeek

instance RealFrac teachingRatio => Data.HumanResource.HumanResource (Profile level stream teachingRatio) where
	getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay		= round . uncurry (*) . (getTeachingRatio &&& fromIntegral . Data.HumanResource.calculateNTimeslotsPerWeekAvailable nTimeslotsPerDay)
	getNTimeslotsPerWeekOfNonTeaching nTimeslotsPerDay	= uncurry (-) . (
		Data.HumanResource.calculateNTimeslotsPerWeekAvailable nTimeslotsPerDay &&& Data.HumanResource.getNTimeslotsPerWeekOfTeaching nTimeslotsPerDay
	 ) -- Derivation from 'getNTimeslotsPerWeekOfTeaching' ensures they are compatible in the face of rounding.
	getGroupMembership					= getGroupMembership
	getMaybeFreePeriodPreference				= getMaybeFreePeriodPreference

instance (
	Data.Default.Default	stream,
	Eq			stream,
	HXT.XmlPickler		level,
	HXT.XmlPickler		stream,
	HXT.XmlPickler		teachingRatio,
	Ord			level,
	Real			teachingRatio,
	Show			level
 ) => HXT.XmlPickler (Profile level stream teachingRatio) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		\(a, b, c, d, e, f)	-> mkProfile a b c d e f,	-- Construct from a tuple.
		\MkProfile {
			getStream			= stream,
			getKnowledgeRequirements	= knowledgeRequirements,
			getWorkingWeek			= workingWeek,
			getTeachingRatio		= teachingRatio,
			getGroupMembership		= groupMembership,
			getMaybeFreePeriodPreference	= maybeFreePeriodPreference
		} -> (
			stream,
			knowledgeRequirements,
			workingWeek,
			teachingRatio,
			groupMembership,
			maybeFreePeriodPreference
		) -- Deconstruct to a tuple.
	 ) $ HXT.xp6Tuple (
		HXT.xpDefault Data.Default.def HXT.xpickle	-- Level.
	 ) (
		HXT.xpElem knowledgeRequirementsTag $ HXT.xpElem Data.Requirements.coreTag (
			HXT.xpWrap (
				Data.Set.fromList,	-- Construct from a List.
				Data.Set.toList		-- Deconstruct to a List.
			) $ HXT.xpList1 {-can't be null-} HXT.xpickle
		) `HXT.xpPair` HXT.xpDefault Data.Set.empty (
			HXT.xpElem Data.Requirements.optionalTag . HXT.xpWrap (
				Data.Set.fromList,	-- Construct from a List.
				Data.Set.toList		-- Deconstruct to a List.
			) $ HXT.xpList1 {-the default is null-} HXT.xpickle
		)
	 ) HXT.xpickle {-workingWeek-} (
		HXT.xpDefault defaultTeachingRatio $ HXT.xpAttr teachingRatioTag HXT.xpickle
	 ) (
		HXT.xpDefault Data.HumanResource.defaultGroupMembership . HXT.xpElem Data.HumanResource.groupMembershipTag . HXT.xpWrap (
			Data.Set.fromList,	-- Construct from a List.
			Data.Set.toList		-- Deconstruct to a List.
		) . HXT.xpList1 {-the default is null-} . HXT.xpElem Data.Group.memberTag $ HXT.xpTextAttr Data.Group.groupIdTag {-can't be null-}
	 ) (
		HXT.xpOption HXT.xpickle	-- maybeFreePeriodPreference.
	 )

instance (
	Control.DeepSeq.NFData	level,
	Control.DeepSeq.NFData	stream,
	Control.DeepSeq.NFData	teachingRatio
 ) => Control.DeepSeq.NFData (Profile level stream teachingRatio) where
	rnf (MkProfile x0 x1 x2 x3 x4 x5)	= Control.DeepSeq.rnf (x0, x1, x2, x3, x4, x5)

-- | Smart constructor.
mkProfile :: (
	Ord	level,
	Real	teachingRatio,
	Show	level
 )
	=> stream							-- ^ The stream for this /student/.
	-> KnowledgeRequirements level					-- ^ The /subject/s to study.
	-> Temporal.Availability.Availability				-- ^ The /day/s on which this /student/ can attend school.
	-> teachingRatio						-- ^ The portion of the /working-week/ allocated to tuition.
	-> Data.Group.Membership					-- ^ The /groups/ of which this /student/ is a member.
	-> Maybe Temporal.FreePeriodPreference.FreePeriodPreference	-- ^ Any preference for the position within each day, of free /time-slot/s.
	-> Profile level stream teachingRatio
mkProfile stream knowledgeRequirements workingWeek teachingRatio groupMembership maybeFreePeriodPreference
	| ToolShed.SelfValidate.isValid profile	= profile
	| otherwise				= error $ "WeekDaze.Data.Student.mkProfile:\t" ++ ToolShed.SelfValidate.getFirstError profile ++ "."
	where
		profile	= MkProfile {
			getStream			= stream,
			getKnowledgeRequirements	= knowledgeRequirements,
			getWorkingWeek			= workingWeek,
			getTeachingRatio		= teachingRatio,
			getGroupMembership		= groupMembership,
			getMaybeFreePeriodPreference	= maybeFreePeriodPreference
		}

-- | Get the core & optional /subject-requirements/.
deriveAmalgamatedKnowledgeRequirement :: Ord level => Profile level stream teachingRatio -> Data.Subject.Knowledge level
deriveAmalgamatedKnowledgeRequirement	= amalgamateKnowledgeRequirements . getKnowledgeRequirements

-- | True if the /student/ has any core /subject/-requirements.
hasAnyCoreKnowledgeRequirements :: Profile level stream teachingRatio -> Bool
hasAnyCoreKnowledgeRequirements	= not . Data.Set.null . Data.Requirements.getCore . getKnowledgeRequirements

-- | True if the /student/ has any optional /subject/-requirements.
hasAnyOptionalKnowledgeRequirements :: Profile level stream teachingRatio -> Bool
hasAnyOptionalKnowledgeRequirements	= not . Data.Set.null . Data.Requirements.getOptional . getKnowledgeRequirements

-- | True if the /student/ is interested in a /subject/, according to the specified predicate.
requiresAnySubjectBy
	:: Ord level
	=> (Data.Subject.Subject level -> Bool)	-- ^ Predicate.
	-> Profile level stream teachingRatio
	-> Bool
requiresAnySubjectBy predicate	= Data.Foldable.any predicate . deriveAmalgamatedKnowledgeRequirement

-- | Unsubscribe from the specified set of /groups/.
unsubscribe
	:: Data.Group.Membership
	-> Profile level stream teachingRatio
	-> Profile level stream teachingRatio
unsubscribe groupMembership profile	= profile {
	getGroupMembership	= Data.Set.filter (`Data.Set.notMember` groupMembership) $ getGroupMembership profile
}
