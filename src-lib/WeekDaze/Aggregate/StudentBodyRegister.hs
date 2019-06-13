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

 [@DESCRIPTION@]	Provides a catalogue of /student-profile/s, indexed by the /student-body/ by whom it's shared.
-}

module WeekDaze.Aggregate.StudentBodyRegister(
-- * Types
-- ** Type-synonyms
	StudentBodyRegister,
	KnowledgeByStudentBody,
-- * Constants
	tag,
-- * Functions
	countAvailableStudentDays,
	countSubjectsRequired,
	extractDistinctSubjects,
	reduce,
	reduce',
-- ** Accessors
	getStudentBodies,
#ifdef USE_HDBC
-- ** Constructor
	fromDatabase,
#endif
-- ** Predicates
	hasAnyFreePeriodPreference,
	hasAnyCoreKnowledgeRequirements,
	hasAnyOptionalKnowledgeRequirements
) where

import			Control.Arrow((&&&))
import qualified	Control.Monad.Writer
import qualified	Data.Foldable
import qualified	Data.Map
import qualified	Data.Set
import qualified	WeekDaze.Aggregate.StudentBody		as Aggregate.StudentBody
import qualified	WeekDaze.Aggregate.StudentClass		as Aggregate.StudentClass
import qualified	WeekDaze.Data.HumanResource		as Data.HumanResource
import qualified	WeekDaze.Data.Resource			as Data.Resource
import qualified	WeekDaze.Data.Student			as Data.Student
import qualified	WeekDaze.Data.Subject			as Data.Subject
import qualified	WeekDaze.Size				as Size

#ifdef USE_HDBC
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.Default
import qualified	Data.IntMap
import qualified	Data.Maybe
import qualified	Data.Typeable
import qualified	WeekDaze.Database.Selector		as Database.Selector
import qualified	WeekDaze.Data.Group			as Data.Group
import qualified	WeekDaze.Data.Requirements		as Data.Requirements
import qualified	WeekDaze.Temporal.Availability		as Temporal.Availability
import qualified	WeekDaze.Temporal.FreePeriodPreference	as Temporal.FreePeriodPreference

-- | Construct from the specified database-connection.
fromDatabase :: (
	Database.HDBC.IConnection		connection,
	Data.Convertible.Convertible		Database.HDBC.SqlValue level,		-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue stream,		-- Flexible context.
	Data.Convertible.Convertible		Database.HDBC.SqlValue teachingRatio,	-- Flexible context.
	Data.Default.Default			stream,
	Data.Typeable.Typeable			teachingRatio,
	Ord					level,
	RealFrac				teachingRatio,
	Show					level
 )
	=> connection			-- ^ An abstract database-connection.
	-> Database.HDBC.SqlValue	-- ^ The project-id.
	-> IO (StudentBodyRegister level stream teachingRatio)
fromDatabase connection	projectIdSql	= let
	priorityColumnName, streamIdColumnName, streamNameColumnName, studentBodyRegisterIdColumnName :: Database.Selector.ColumnName
	priorityColumnName		= "priority"
	streamIdColumnName		= "streamId"
	streamNameColumnName		= "streamName"	-- CAVEAT: known merely as 'stream' in Data.Student & the DTD.
	studentBodyRegisterIdColumnName	= showString tag "Id";

	knowledgeRequirementsTableName, streamTableName, studentBodyRegisterTableName, studentBodyMembershipTableName, studentGroupMembershipTableName :: Database.Selector.TableName
	[knowledgeRequirementsTableName, streamTableName, studentBodyRegisterTableName, studentBodyMembershipTableName, studentGroupMembershipTableName]	= map (showString Database.Selector.tablePrefix) ["knowledgeRequirement", "stream", tag, "studentBodyMembership", "studentGroupMembership"]
 in do
	streamNameByStreamId	<- (
		Data.IntMap.fromList . map (
			\row -> case row of
				[streamIdSql, streamNameSql]	-> (
					either (
						error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tfailed to parse the value for " . shows streamIdColumnName . showString " read from the database; " . show
					 ) id $ Database.HDBC.safeFromSql streamIdSql,
					Database.HDBC.fromSql streamNameSql
				 ) -- Pair.
				_				-> error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tunexpected number of columns=" . shows (length row) . showString " in row of table " $ shows streamTableName "."
		)
	 ) `fmap` Database.Selector.select connection [streamIdColumnName, streamNameColumnName] [streamTableName] [(Database.Selector.projectIdColumnName, projectIdSql)]

#ifdef USE_HDBC_ODBC
	[
		selectStudentIdsForStudentBodyRegisterId,
		selectKnowledgeRequirementsForStudentBodyRegisterId,
		selectGroupIdsForStudentBodyRegisterId
	 ] <- mapM (
		\(columnNames, tableName)	-> Database.Selector.prepare connection columnNames [tableName] [studentBodyRegisterIdColumnName]
	 ) [
		(
			[Aggregate.StudentBody.studentIdTag],
			studentBodyMembershipTableName
		), (
			[
				priorityColumnName,
				Data.Subject.topicTag,
				Data.Subject.levelTag
			],
			knowledgeRequirementsTableName
		), (
			[Data.Group.groupIdTag],
			studentGroupMembershipTableName
		)
	 ] -- Prepare statements for execution with each studentBodyRegisterId.
#endif /* USE_HDBC_ODBC */
	Database.Selector.select connection [
		studentBodyRegisterIdColumnName,
		Aggregate.StudentBody.mnemonicTag,
		Temporal.Availability.tag,
		streamIdColumnName,
		Data.Student.teachingRatioTag,
		Temporal.FreePeriodPreference.tag
	 ] [studentBodyRegisterTableName] [(Database.Selector.projectIdColumnName, projectIdSql)] >>= fmap Data.Map.fromList . mapM (
		\studentRow -> case studentRow of
			[studentBodyRegisterIdSql, mnemonicSql, availabilitySql, streamIdSql, teachingRatioSql, freePeriodPreferenceSql]	-> do
#ifndef USE_HDBC_ODBC
				let primaryKey	= [(studentBodyRegisterIdColumnName, studentBodyRegisterIdSql)]
#endif
				studentIds	<- map (
					Database.HDBC.fromSql . head {-select the only column-}
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectStudentIdsForStudentBodyRegisterId [studentBodyRegisterIdSql] >> Database.HDBC.fetchAllRows' selectStudentIdsForStudentBodyRegisterId
				 )
#else
				 ) `fmap` Database.Selector.select connection [Aggregate.StudentBody.studentIdTag] [studentBodyMembershipTableName] primaryKey
#endif
				knowledgeRequirements	<- (
					(
						Data.Maybe.fromMaybe Data.Set.empty . Data.Map.lookup Data.Requirements.coreTag &&& Data.Maybe.fromMaybe Data.Set.empty . Data.Map.lookup Data.Requirements.optionalTag
					) . Data.Map.fromListWith Data.Set.union . map (
						\knowledgeRequirementRow -> case knowledgeRequirementRow of
							[prioritySql, topicSql, levelSql]	-> (
								Data.Maybe.fromMaybe (
									error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tnull " $ shows priorityColumnName "."
								) $ Database.HDBC.fromSql prioritySql,
								Data.Set.singleton $ Data.Subject.mkSubjectFromSql topicSql levelSql
							 ) -- Pair
							_					-> error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tunexpected number of columns=" . shows (length knowledgeRequirementRow) . showString " in row of table " $ shows knowledgeRequirementsTableName "."
					)
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectKnowledgeRequirementsForStudentBodyRegisterId [studentBodyRegisterIdSql] >> Database.HDBC.fetchAllRows' selectKnowledgeRequirementsForStudentBodyRegisterId
				 )
#else
				 ) `fmap` Database.Selector.select connection [
					priorityColumnName,
					Data.Subject.topicTag,
					Data.Subject.levelTag
				 ] [knowledgeRequirementsTableName] primaryKey
#endif

				groupMembership		<- (
					Data.Set.fromList . map (
						either (
							error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tfailed to parse the value for " . shows Data.Group.groupIdTag . showString " read from the database; " . show
						) id . Database.HDBC.safeFromSql . head {-select the only column-}
					)
#ifdef USE_HDBC_ODBC
				 ) `fmap` (
					Database.HDBC.execute selectGroupIdsForStudentBodyRegisterId [studentBodyRegisterIdSql] >> Database.HDBC.fetchAllRows' selectGroupIdsForStudentBodyRegisterId
				 )
#else
				 ) `fmap` Database.Selector.select connection [Data.Group.groupIdTag] [studentGroupMembershipTableName] primaryKey
#endif
				return {-to IO-monad-} (
					Aggregate.StudentBody.mkStudentBody (
						either (
							error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tfailed to parse the value for " . shows Aggregate.StudentBody.mnemonicTag . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql mnemonicSql
					) . Data.Set.fromList $ map (
						Data.Maybe.fromMaybe (
							error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tnull " $ shows Aggregate.StudentBody.studentIdTag "."
						)
					) studentIds,
					Data.Student.mkProfile (
						Data.Maybe.maybe Data.Default.def (
							Data.Maybe.fromMaybe (
								error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tfailed to find a " . shows streamNameColumnName . showString " corresponding to the value for " $ shows streamIdColumnName " read from the database."
							) . (
								`Data.IntMap.lookup` streamNameByStreamId
							)
						) . either (
							error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tfailed to parse the value for " . shows streamIdColumnName . showString " read from the database; " . show
						) id $ Database.HDBC.safeFromSql streamIdSql
					) knowledgeRequirements (
						Data.Maybe.fromMaybe (
							error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tnull " $ shows Temporal.Availability.tag "."
						) $ Database.HDBC.fromSql availabilitySql
					) (
						Database.Selector.fromSqlFractional Data.Student.defaultTeachingRatio teachingRatioSql
					) groupMembership $ Database.HDBC.fromSql freePeriodPreferenceSql	-- Returns Nothing for SqlNull.
				 ) -- Pair.
			_ -> error . showString "WeekDaze.Aggregate.StudentBodyRegister.fromDatabase:\tunexpected number of columns=" . shows (length studentRow) . showString" in row of table " $ shows studentBodyRegisterTableName "."
	 )
#endif /* USE_HDBC */

-- | Used to qualify XML.
tag :: String
tag		= "studentBodyRegister"

-- | A map indexed by /student-body/, of /profile/s shared amongst /student/s in the /body/.
type StudentBodyRegister level stream teachingRatio	= Data.Resource.ResourceMap Aggregate.StudentBody.StudentBody (Data.Student.Profile level stream teachingRatio)

-- | Accessor.
getStudentBodies :: StudentBodyRegister level stream teachingRatio -> [Aggregate.StudentBody.StudentBody]
getStudentBodies	= Data.Map.keys

-- | Extracts the set of distinct /subjects/ required, gathered from all /students/.
extractDistinctSubjects :: Ord level => StudentBodyRegister level stream teachingRatio -> Data.Subject.Knowledge level
extractDistinctSubjects	= Data.Map.foldr (Data.Set.union . Data.Student.deriveAmalgamatedKnowledgeRequirement) Data.Set.empty

{- |
	* Merges those /student-bodies/ whose /profile/s are identical, into a single larger /student-body/.

	* Writes the lists of merged /student-bodies/.

	* Checks that any new /mnemonic/s, composed from those of /student-bodies/ with identical profiles, differ from existing ones.
	This shouldn't matter, because the key of 'StudentBodyRegister' is the whole /student-body/ not just the /mnemonic/,
	regrettably, for efficiency, /StudentBody/ implements 'Eq' & 'Ord' using only the /mnemonic/.
-}
reduce :: (
	Ord	level,
	Ord	stream,
	Ord	teachingRatio
 )
	=> Aggregate.StudentClass.MnemonicSeparator
	-> StudentBodyRegister level stream teachingRatio
	-> Control.Monad.Writer.Writer [[Aggregate.StudentBody.StudentBody]] (StudentBodyRegister level stream teachingRatio)
reduce mnemonicSeparator studentBodyRegister
	| null ambiguousMnemonics	= Control.Monad.Writer.writer (studentBodyRegister', mergeableStudentBodies)
	| otherwise			= error . showString "WeekDaze.Aggregate.StudentBodyRegister.reduce:\tsome of the mnemonics composed by merging those of student-bodies with identical profiles, are ambiguous; " $ shows ambiguousMnemonics "."
	where
		(studentBodyRegister', (ambiguousMnemonics, mergeableStudentBodies))	= (
			Data.Map.fromList . map (
				\(sharedStudentProfile, studentClass)	-> (Aggregate.StudentClass.merge mnemonicSeparator studentClass, sharedStudentProfile)	-- Reverse the mapping back to the original.
			) . Data.Map.toList &&& (
				Data.Set.toList . Data.Set.intersection (
					Data.Set.map Aggregate.StudentBody.getMnemonic $ Data.Map.keysSet studentBodyRegister
				) . Data.Set.fromList . map (
					Aggregate.StudentBody.getMnemonic . Aggregate.StudentClass.merge mnemonicSeparator
				) &&& map Data.Set.toList
			) . filter (
				(/= 1) . Data.Set.size	-- Select student-bodies that can be merged.
			) . Data.Map.elems
		 ) . Data.Map.fromListWith Data.Set.union . map (
			\(studentBody, studentProfile)	-> (studentProfile, Data.Set.singleton studentBody)	-- Reverse the mapping.
		 ) $ Data.Map.toList studentBodyRegister

-- | Merges those /student-bodies/ whose /profile/s are identical, into a single larger /student-body/.
reduce' :: (
	Ord	level,
	Ord	stream,
	Ord	teachingRatio
 )
	=> Aggregate.StudentClass.MnemonicSeparator
	-> StudentBodyRegister level stream teachingRatio
	-> StudentBodyRegister level stream teachingRatio
reduce' s	= fst {-StudentBodyRegister-} . Control.Monad.Writer.runWriter . reduce s

-- | The sum of the number of /day/s worked by each /student/.
countAvailableStudentDays :: StudentBodyRegister level stream teachingRatio -> Size.NDays
countAvailableStudentDays	= Data.Map.foldrWithKey (\studentBody -> (+) . (* Aggregate.StudentBody.getSize studentBody) . Data.Resource.countDaysPerWeekAvailable) 0

-- | /Knowlege-requirement/, indexed by /student-body/.
type KnowledgeByStudentBody level	= Data.Map.Map Aggregate.StudentBody.StudentBody (Data.Subject.Knowledge level)

-- | Count the total number of /subject/s required by all /student/s.
countSubjectsRequired :: KnowledgeByStudentBody level -> Int
countSubjectsRequired	= Data.Map.foldrWithKey (\studentBody -> (+) . (* Aggregate.StudentBody.getSize studentBody) . Data.Set.size) 0

-- | True if any /student-body/ has specified a free-period preference.
hasAnyFreePeriodPreference :: RealFrac teachingRatio => StudentBodyRegister level stream teachingRatio -> Bool
hasAnyFreePeriodPreference	= Data.Foldable.any Data.HumanResource.hasFreePeriodPreference

-- | True if any /student-body/ has specified a core /subject/ in their knowledge-requirements.
hasAnyCoreKnowledgeRequirements :: StudentBodyRegister level stream teachingRatio -> Bool
hasAnyCoreKnowledgeRequirements	= Data.Foldable.any Data.Student.hasAnyCoreKnowledgeRequirements

-- | True if any /student-body/ has specified an optional /subject/ in their knowledge-requirements.
hasAnyOptionalKnowledgeRequirements :: StudentBodyRegister level stream teachingRatio -> Bool
hasAnyOptionalKnowledgeRequirements	= Data.Foldable.any Data.Student.hasAnyOptionalKnowledgeRequirements

