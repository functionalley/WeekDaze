-- Copyright (C) 2015 Dr. Alistair Ward
--
-- This file is part of WeekDaze.
--
-- WeekDaze is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- WeekDaze is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with WeekDaze.  If not, see <http://www.gnu.org/licenses/>.
--
-- Create the functions required shortly in triggers.

DROP FUNCTION IF EXISTS `inClosedUnitInterval`;
CREATE FUNCTION inClosedUnitInterval(f DOUBLE) RETURNS BOOL COMMENT 'Whether the specified floating-point number lies within the inclusive range, zero to one.' DETERMINISTIC
	RETURN	f BETWEEN 0 AND 1;

DROP FUNCTION IF EXISTS `isValidConfigVersion`;
CREATE FUNCTION isValidConfigVersion(configVersion VARCHAR(16)) RETURNS BOOL COMMENT 'Whether the format of the specified configuration-version is acceptable.' DETERMINISTIC
	RETURN	configVersion REGEXP "^[0-9]+(\.[0-9]+)*$";

DROP FUNCTION IF EXISTS `isValidEmailAddressFormat`;
CREATE FUNCTION isValidEmailAddressFormat(emailAddress VARCHAR(255)) RETURNS BOOL COMMENT 'Validates the format of the specified email-address, but not the existence of any MX-record.' DETERMINISTIC
	RETURN	emailAddress REGEXP "^[-#$^&|!%'`?*+/={}~[:alnum:]]+(\.[-#$^&|!%'`?*+/={}~[:alnum:]]+)*@([[:alnum:]][-[:alnum:]]{0,62}\.)+[[:alpha:]]{2,}$";

DROP FUNCTION IF EXISTS `facilityProvidersExist`;
CREATE FUNCTION facilityProvidersExist(
	pProjectId	INT UNSIGNED,
	pFacilityTypeId	INT UNSIGNED
) RETURNS BOOL COMMENT 'Whether the specified facility-type is advertised by a location in the specified project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_locationCatalogue` NATURAL JOIN `tbl_facility` /* USING (`locationCatalogueId`) */ WHERE `projectId` = pProjectId AND `facilityTypeId` = pFacilityTypeId
	);

DROP FUNCTION IF EXISTS `areAvailableMemberStudentBodies`;
CREATE FUNCTION areAvailableMemberStudentBodies(
	pProjectId	INT UNSIGNED,
	pGroupId	VARCHAR(32),
	pDay		ENUM('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
) RETURNS BOOL COMMENT 'Whether all the student-bodies who\'re members of the specified group, are available to meet on the specified day.'
	RETURN NOT EXISTS (
		SELECT 1 FROM `tbl_studentBodyRegister` NATURAL JOIN `tbl_studentGroupMembership` /* USING (`studentBodyRegisterId`) */ WHERE `projectId` = pProjectId AND `groupId` = pGroupId AND FIND_IN_SET(pDay, availability) = 0 /* unavailable */
	);

DROP FUNCTION IF EXISTS `areAvailableMemberTeachers`;
CREATE FUNCTION areAvailableMemberTeachers(
	pProjectId	INT UNSIGNED,
	pGroupId	VARCHAR(32),
	pDay		ENUM('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
) RETURNS BOOL COMMENT 'Whether all the teachers who\'re members of the specified group, are available to meet on the specified day.'
	RETURN NOT EXISTS (
		SELECT 1 FROM `tbl_teacherRegister` NATURAL JOIN `tbl_teacherGroupMembership` /* USING (`teacherRegisterId`) */ WHERE `projectId` = pProjectId AND `groupId` = pGroupId AND FIND_IN_SET(pDay, availability) = 0 /* unavailable */
	);

-- DROP FUNCTION IF EXISTS `facilityIsRequired`;
-- CREATE FUNCTION facilityIsRequired(
-- 	pProjectId	INT UNSIGNED,
-- 	pFacilityTypeId	INT UNSIGNED
-- ) RETURNS BOOL COMMENT 'Whether the specified facility-type is required by any course, offered by any teacher in the specified project.'
-- 	RETURN EXISTS (
-- 		SELECT 1 FROM `tbl_teacherRegister` NATURAL JOIN `tbl_requiredFacility` /* USING (`teacherRegisterId`) */ WHERE `projectId` = pProjectId AND `facilityTypeId` = pFacilityTypeId
-- 	);

DROP FUNCTION IF EXISTS `groupIdExists`;
CREATE FUNCTION groupIdExists(
	pProjectId	INT UNSIGNED,
	pGroupId	VARCHAR(32)
) RETURNS BOOL COMMENT 'Whether the specified group has been defined in the specified project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_groupCatalogue` WHERE `projectId` = pProjectId AND `groupId` = pGroupId
	);

DROP FUNCTION IF EXISTS `groupIdByStudentBodyRegisterIdExists`;
CREATE FUNCTION groupIdByStudentBodyRegisterIdExists(
	pStudentBodyRegisterId	INT UNSIGNED,
	pGroupId		VARCHAR(32)
) RETURNS BOOL COMMENT 'Whether the specified group has been defined in the implied project.'
	RETURN groupIdExists(
		(SELECT `projectId` FROM `tbl_studentBodyRegister` WHERE `studentBodyRegisterId` = pStudentBodyRegisterId),
		pGroupId
	);

DROP FUNCTION IF EXISTS `groupIdByTeacherRegisterIdExists`;
CREATE FUNCTION groupIdByTeacherRegisterIdExists(
	pTeacherRegisterId	INT UNSIGNED,
	pGroupId		VARCHAR(32)
) RETURNS BOOL COMMENT 'Whether the specified group has been defined in the implied project.'
	RETURN groupIdExists(
		(SELECT `projectId` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = pTeacherRegisterId),
		pGroupId
	);

DROP FUNCTION IF EXISTS `isAvailableFacility`;
CREATE FUNCTION isAvailableFacility(
	pTeacherRegisterId	INT UNSIGNED,
	pFacilityTypeId		INT UNSIGNED
) RETURNS BOOL COMMENT 'Whether the specified facility-type is available @ any location in the implied project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_locationCatalogue` NATURAL JOIN `tbl_facility` /* USING (`locationCatalogueId`) */ WHERE `projectId` = (
			SELECT `projectId` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = pTeacherRegisterId
		) AND `facilityTypeId` = pFacilityTypeId
	);

DROP FUNCTION IF EXISTS `isAvailableGroupForAllMandatoryMeetings`;
CREATE FUNCTION isAvailableGroupForAllMandatoryMeetings(
	pProjectId	INT UNSIGNED,
	pGroupId	VARCHAR(32),
	pAvailability	SET('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
) RETURNS BOOL COMMENT 'Whether all the mandatory meeting-times defined for the specified group, fall within the specified set of available days.'
	RETURN NOT EXISTS (
		SELECT 1 FROM `tbl_groupCatalogue` NATURAL JOIN `tbl_meetingTime` /* USING (`groupCatalogueId`) */ WHERE `projectId` = pProjectId AND `groupId` = pGroupId AND `mandatesAttendance` <=> TRUE AND FIND_IN_SET(day, pAvailability) = 0 /* unavailable */
	);

-- CAVEAT: check for NULL locationId before calling.
DROP FUNCTION IF EXISTS `isAvailableLocation`;
CREATE FUNCTION isAvailableLocation(
	pProjectId	INT UNSIGNED,
	pLocationId	VARCHAR(32),
	pDay		ENUM('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
) RETURNS BOOL COMMENT 'Whether the specified location, in the specified project, is available on the specified day.'
	RETURN FIND_IN_SET(
		pDay,
		(
			SELECT `availability` FROM `tbl_locationCatalogue` WHERE `projectId` = pProjectId AND `locationId` = pLocationId
		)
	) > 0;

DROP FUNCTION IF EXISTS `matchingCourseExists`;
CREATE FUNCTION matchingCourseExists(
	pProjectId	INT UNSIGNED,
	pTopic		VARCHAR(32),
	pLevel		VARCHAR(32)
) RETURNS BOOL COMMENT 'Whether the specified subject is offered in any course, of any teacher in the specified project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_teacherRegister` NATURAL JOIN `tbl_service` /* USING (`teacherRegisterId`) */ WHERE `projectId` = pProjectId AND `topic` = pTopic AND `level` = pLevel
	);

DROP FUNCTION IF EXISTS `meetingTimeExists`;
CREATE FUNCTION meetingTimeExists(pProjectId INT UNSIGNED) RETURNS BOOL COMMENT 'Whether any meeting-times have been defined in the specified project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_groupCatalogue` NATURAL JOIN `tbl_meetingTime` /* USING (`groupCatalogueId`) */ WHERE `projectId` = pProjectId
	);

DROP FUNCTION IF EXISTS `specificTimeRequestExists`;
CREATE FUNCTION specificTimeRequestExists(pProjectId INT UNSIGNED) RETURNS BOOL COMMENT 'Whether any specific time requests have been defined in the specified project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_teacherRegister` NATURAL JOIN `tbl_specificTimeRequest` /* USING (`teacherRegisterId`) */ WHERE `projectId` = pProjectId
	);

DROP FUNCTION IF EXISTS `studentExists`;
CREATE FUNCTION studentExists(
	pStudentBodyRegisterId	INT UNSIGNED,
	pStudentId		VARCHAR(32)
) RETURNS BOOL COMMENT 'Whether the specified student exists in any student-body in the implied project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_studentBodyRegister` NATURAL JOIN `tbl_studentBodyMembership` /* USING (`studentBodyRegisterId`) */ WHERE `projectId` = (
			SELECT `projectId` FROM `tbl_studentBodyRegister` WHERE `studentBodyRegisterId` = pStudentBodyRegisterId
		) AND `studentId` = pStudentId
	);

DROP FUNCTION IF EXISTS `subjectIsOffered`;
CREATE FUNCTION subjectIsOffered(
	pProjectId	INT UNSIGNED,
	pTopic		VARCHAR(32),
	pLevel		VARCHAR(32)
) RETURNS BOOL COMMENT 'Whether the specified subject is offered by any teachers in the specified project.'
	RETURN EXISTS (
		SELECT DISTINCT `teacherRegisterId` FROM `tbl_service` WHERE `teacherRegisterId` IN (
			SELECT `teacherRegisterId` FROM `tbl_teacherRegister` WHERE `projectId` = pProjectId
		) AND `topic` = pTopic AND `level` = pLevel
	);

DROP FUNCTION IF EXISTS `subjectIsRequired`;
CREATE FUNCTION subjectIsRequired(
	pProjectId	INT UNSIGNED,
	pTopic		VARCHAR(32),
	pLevel		VARCHAR(32)
) RETURNS BOOL COMMENT 'Whether the specified subject is included in the knowledge-requirements, of any student-body in the specified project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_studentBodyRegister` INNER JOIN `tbl_knowledgeRequirement` AS k USING (studentBodyRegisterId) WHERE `projectId` = pProjectId AND `topic` = pTopic AND k.`level` = pLevel
	);

DROP FUNCTION IF EXISTS `areLocationAndFacilityTypeInSameProject`;
CREATE FUNCTION areLocationAndFacilityTypeInSameProject(
	pLocationCatalogueId	INT UNSIGNED,
	pFacilityTypeId		INT UNSIGNED
) RETURNS BOOL COMMENT 'Whether the referenced location & facility-type exist in the same project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_locationCatalogue` NATURAL JOIN `tbl_facilityType` WHERE `locationCatalogueId` = pLocationCatalogueId AND `facilityTypeId` = pFacilityTypeId
	);

DROP FUNCTION IF EXISTS `areTeacherAndFacilityTypeInSameProject`;
CREATE FUNCTION areTeacherAndFacilityTypeInSameProject(
	pTeacherRegisterId	INT UNSIGNED,
	pFacilityTypeId		INT UNSIGNED
) RETURNS BOOL COMMENT 'Whether the referenced teacher & facility-type exist in the same project.'
	RETURN EXISTS (
		SELECT 1 FROM `tbl_teacherRegister` NATURAL JOIN `tbl_facilityType` WHERE `teacherRegisterId` = pTeacherRegisterId AND `facilityTypeId` = pFacilityTypeId
	);

-- Create the procedures required shortly in triggers.

DROP PROCEDURE IF EXISTS `deleteKnowledgeRequirements`;
CREATE PROCEDURE deleteKnowledgeRequirements(
	IN pProjectId	INT UNSIGNED,
	IN pTopic	VARCHAR(32),
	IN pLevel	VARCHAR(32)
) COMMENT 'Deletes matching knowledgeRequirements.'
	DELETE FROM `tbl_knowledgeRequirement` WHERE `studentBodyRegisterId` IN (
		SELECT `studentBodyRegisterId` FROM `tbl_studentBodyRegister` WHERE `projectId` = pProjectId
	) AND `topic` = pTopic AND `level` = pLevel AND NOT subjectIsOffered(pProjectId, pTopic, pLevel);

DROP PROCEDURE IF EXISTS `deleteKnowledgeRequirementsLackingCourse`;
CREATE PROCEDURE deleteKnowledgeRequirementsLackingCourse(IN pProjectId INT UNSIGNED) COMMENT 'Deletes knowledgeRequirements lacking a course.'
	DELETE FROM `tbl_knowledgeRequirement` WHERE `studentBodyRegisterId` IN (
		SELECT `studentBodyRegisterId` FROM `tbl_studentBodyRegister` WHERE `projectId` = pProjectId
	) AND NOT EXISTS (
		SELECT 1 FROM `tbl_service` AS s NATURAL JOIN /* USING (`teacherRegisterId`) */ `tbl_teacherRegister` WHERE `projectId` = pProjectId AND `tbl_knowledgeRequirement`.`topic` = `s`.`topic` AND `tbl_knowledgeRequirement`.`level` = `s`.`level`
	);

DROP PROCEDURE IF EXISTS `deleteRequiredFacility`;
CREATE PROCEDURE deleteRequiredFacility(
	IN pProjectId		INT UNSIGNED,
	IN pFacilityTypeId	INT UNSIGNED
) COMMENT 'Deletes the specified requiredFacilities.'
	DELETE FROM `tbl_requiredFacility` WHERE `teacherRegisterId` IN (
		SELECT `teacherRegisterId` FROM `tbl_teacherRegister` WHERE `projectId` = pProjectId
	) AND `facilityTypeId` = pFacilityTypeId;

DROP PROCEDURE IF EXISTS `deleteMatchingRequiredFacilities`;
CREATE PROCEDURE deleteMatchingRequiredFacilities(
	IN pTeacherRegisterId	INT UNSIGNED,
	IN pTopic		VARCHAR(32),
	IN pLevel		VARCHAR(32)
) COMMENT 'Deletes matching requiredFacilities.'
	DELETE FROM `tbl_requiredFacility` WHERE `teacherRegisterId` = pTeacherRegisterId AND `topic` = pTopic AND `level` = pLevel;

DROP PROCEDURE IF EXISTS `deleteSpecificTimeRequests`;
CREATE PROCEDURE deleteSpecificTimeRequests(
	IN pTeacherRegisterId	INT UNSIGNED,
	IN pTopic		VARCHAR(32),
	IN pLevel		VARCHAR(32)
) COMMENT 'Deletes matching specificTimeRequests.'
	DELETE FROM `tbl_specificTimeRequest` WHERE `teacherRegisterId` = pTeacherRegisterId AND `topic` = pTopic AND `level` = pLevel;

DROP PROCEDURE IF EXISTS `deleteUnadvertisedRequiredFacilities`;
CREATE PROCEDURE deleteUnadvertisedRequiredFacilities(IN pProjectId INT UNSIGNED) COMMENT 'Deletes unspecified requiredFacilities which are not advertised by any location.'
	DELETE FROM `tbl_requiredFacility` WHERE `teacherRegisterId` IN (
		SELECT `teacherRegisterId` FROM `tbl_teacherRegister` WHERE `projectId` = pProjectId
	) AND `facilityTypeId` NOT IN (
		SELECT `facilityTypeId` FROM `tbl_facility` NATURAL JOIN /* USING (`locationCatalogueId`)*/ `tbl_locationCatalogue` WHERE `projectId` = pProjectId
	);

-- Temporarily redefine MySqlClient's statement-delimiter, to distinguish it from the SQL statement-separator.
DELIMITER //

DROP PROCEDURE IF EXISTS `deleteGroupMembership`;
CREATE PROCEDURE deleteGroupMembership(
	IN pProjectId	INT UNSIGNED,
	IN pGroupId	VARCHAR(32)
) COMMENT 'Deletes all memberships of the specified group.'
	BEGIN
		DELETE FROM `tbl_studentGroupMembership` WHERE `studentBodyRegisterId` IN (
			SELECT `studentBodyRegisterId` FROM `tbl_studentBodyRegister` WHERE `projectId` = pProjectId
		) AND `groupId` = pGroupId;

		DELETE FROM `tbl_teacherGroupMembership` WHERE `teacherRegisterId` IN (
			SELECT `teacherRegisterId` FROM `tbl_teacherRegister` WHERE `projectId` = pProjectId
		) AND `groupId` = pGroupId;
	END
//
/*
	Create triggers to express a more sophisticated referential integrity than a mere foreign key can express.
	CAVEAT: in MySql, "Triggers are not activated by foreign key actions"; https://dev.mysql.com/doc/refman/5.5/en/stored-program-restrictions.html
	CAVEAT: for security, don't print embed any VARCHAR fields into error-messages, since they may contain arbitrary text & this database may be used as part of a web-application.
*/
CREATE TRIGGER `user.beforeInsert` BEFORE INSERT ON `tbl_user`
	FOR EACH ROW /* inserted */
	BEGIN
		IF NOT isValidEmailAddressFormat(NEW.`emailAddress`) THEN
			SET @msg	= CONCAT('user: invalid "emailAddress"; where userId=', NEW.`userId`, '.');	-- CAVEAT: for security, don't print the `email-address` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_user', COLUMN_NAME='emailAddress', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `user.beforeUpdate` BEFORE UPDATE ON `tbl_user`
	FOR EACH ROW /* updated */
	BEGIN
		IF NOT isValidEmailAddressFormat(NEW.`emailAddress`) THEN
			SET @msg	= CONCAT('user: invalid "emailAddress"; where userId=', NEW.`userId`, '.');	-- CAVEAT: for security, don't print the `email-address` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_user', COLUMN_NAME='emailAddress', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `configVersion.beforeInsert` BEFORE INSERT ON `tbl_configVersion`
	FOR EACH ROW /* inserted */
	BEGIN
		IF NOT isValidConfigVersion(NEW.`configVersion`) THEN
			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_configVersion', COLUMN_NAME='configVersion', MESSAGE_TEXT='configVersion: invalid "configVersion"; dotted-decimal required.';	-- CAVEAT: for security, don't print the `configVersion` which may contain arbitrary text.
		END IF;
	END
//
CREATE TRIGGER `configVersion.beforeUpdate` BEFORE UPDATE ON `tbl_configVersion`
	FOR EACH ROW /* updated */
	BEGIN
		IF NOT isValidConfigVersion(NEW.`configVersion`) THEN
			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_configVersion', COLUMN_NAME='configVersion', MESSAGE_TEXT='configVersion: invalid "configVersion"; dotted-decimal required.';	-- CAVEAT: for security, don't print the `configVersion` which may contain arbitrary text.
		END IF;
	END
//
-- This won't trigger on deleting a foreign key 'tbl_project.projectId', but it will cascade-delete the affected rows from 'tbl_specificTimeRequest', 'tbl_service' (via tbl_teacherRegister) & 'tbl_meetingTime', thus rendering the issue mute.
CREATE TRIGGER `timeslotIdBounds.beforeDelete` BEFORE DELETE ON `tbl_timeslotIdBounds`
	FOR EACH ROW /* deleted */
	BEGIN
		DELETE FROM `tbl_meetingTime` WHERE `groupCatalogueId` IN (
			SELECT `groupCatalogueId` FROM `tbl_groupCatalogue` WHERE `projectId` = OLD.`projectId`
		); -- Nothing depends on these.

		DELETE FROM `tbl_service` WHERE `teacherRegisterId` IN (
			SELECT `teacherRegisterId` FROM `tbl_teacherRegister` WHERE `projectId` = OLD.`projectId`
		) AND (
			`idealTimeslotRequest` IS NOT NULL OR `minimumConsecutiveLessons` > 1
		); -- CAVEAT: this might result in some knowledge-requirements being deleted also.

		DELETE FROM `tbl_specificTimeRequest` WHERE `teacherRegisterId` IN (
			SELECT `teacherRegisterId` FROM `tbl_teacherRegister` WHERE `projectId` = OLD.`projectId`
		); -- Nothing depends on these.
	END
//
CREATE TRIGGER `timeslotIdBounds.beforeInsert` BEFORE INSERT ON `tbl_timeslotIdBounds`
	FOR EACH ROW /* inserted */
	BEGIN
		IF NEW.`min` > NEW.`max` THEN
			SET @msg	= CONCAT('timeslotIdBounds: min=', NEW.`min`, ' exceeds max=', NEW.`max`, '; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_timeslotIdBounds', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `timeslotIdBounds.beforeUpdate` BEFORE UPDATE ON `tbl_timeslotIdBounds`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_timeslotIdBounds';

		IF NEW.`projectId` <> OLD.`projectId` /* the old timeslot-id bounds have effectively been deleted */ AND (
			specificTimeRequestExists(OLD.`projectId`) OR meetingTimeExists(OLD.`projectId`)
		) THEN
			SET @msg	= CONCAT('timeslotIdBounds: specific time requests or meeting-times still reference this range; where projectId=', OLD.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='projectId', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`min` > NEW.`max` THEN
			SET @msg	= CONCAT('timeslotIdBounds: min=', NEW.`min`, ' exceeds max=', NEW.`max`, '; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSE
			DELETE FROM `tbl_meetingTime` WHERE `groupCatalogueId` IN (
				SELECT `groupCatalogueId` FROM `tbl_groupCatalogue` WHERE `projectId` = OLD.`projectId`
			) AND `timeslotId` NOT BETWEEN NEW.`min` AND NEW.`max`;	-- Nothing depends on these.

			DELETE FROM `tbl_service` WHERE `teacherRegisterId` IN (
				SELECT `teacherRegisterId` FROM `tbl_teacherRegister` WHERE `projectId` = OLD.`projectId`
			) AND (
				(
					`idealTimeslotRequest` IS NOT NULL AND `idealTimeslotRequest` NOT BETWEEN NEW.`min` AND NEW.`max`
				) OR `minimumConsecutiveLessons` > 1 + NEW.`max` - NEW.`min`
			); -- CAVEAT: this might result in some knowledge-requirements being deleted also.

			DELETE FROM `tbl_specificTimeRequest` WHERE `teacherRegisterId` IN (
				SELECT `teacherRegisterId` FROM `tbl_teacherRegister` WHERE `projectId` = OLD.`projectId`
			) AND `timeslotId` NOT BETWEEN NEW.`min` AND NEW.`max`;	-- Nothing depends on these.
		END IF;
	END
//
CREATE TRIGGER `outputOptions.beforeInsert` BEFORE INSERT ON `tbl_outputOptions`
	FOR EACH ROW /* inserted */
	BEGIN
		IF NEW.`nDecimalDigits` = 0 THEN
			SET @msg	= CONCAT('outputOptions: "nDecimalDigits" must exceed zero; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_outputOptions', COLUMN_NAME='nDecimalDigits', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `outputOptions.beforeUpdate` BEFORE UPDATE ON `tbl_outputOptions`
	FOR EACH ROW /* updated */
	BEGIN
		IF NEW.`nDecimalDigits` = 0 THEN
			SET @msg	= CONCAT('outputOptions: "nDecimalDigits" must exceed zero; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_outputOptions', COLUMN_NAME='nDecimalDigits', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `evolutionStrategies.beforeInsert` BEFORE INSERT ON `tbl_evolutionStrategies`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_evolutionStrategies';

		IF NEW.`fecundityDecayRatio` IS NOT NULL AND NOT inClosedUnitInterval(NEW.`fecundityDecayRatio`) THEN
			SET @msg	= CONCAT('evolutionStrategies: fecundityDecayRatio=', NEW.`fecundityDecayRatio`, ' is not in the closed unit-interval; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='fecundityDecayRatio', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`minimumPopulationDiversityRatio` IS NOT NULL AND NOT inClosedUnitInterval(NEW.`minimumPopulationDiversityRatio`) THEN
			SET @msg	= CONCAT('evolutionStrategies: minimumPopulationDiversityRatio=', NEW.`minimumPopulationDiversityRatio`, ' is not in the closed unit-interval; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='minimumPopulationDiversityRatio', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `evolutionStrategies.beforeUpdate` BEFORE UPDATE ON `tbl_evolutionStrategies`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_evolutionStrategies';

		IF NEW.`fecundityDecayRatio` IS NOT NULL AND NOT inClosedUnitInterval(NEW.`fecundityDecayRatio`) THEN
			SET @msg	= CONCAT('evolutionStrategies: fecundityDecayRatio=', NEW.`fecundityDecayRatio`, ' is not in the closed unit-interval; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='fecundityDecayRatio', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`minimumPopulationDiversityRatio` IS NOT NULL AND NOT inClosedUnitInterval(NEW.`minimumPopulationDiversityRatio`) THEN
			SET @msg	= CONCAT('evolutionStrategies: minimumPopulationDiversityRatio=', NEW.`minimumPopulationDiversityRatio`, ' is not in the closed unit-interval; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='minimumPopulationDiversityRatio', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `tbl_optimiseLessonCriteriaWeights.beforeInsert` BEFORE INSERT ON `tbl_optimiseLessonCriteriaWeights`
	FOR EACH ROW /* inserted */
	BEGIN
		IF NEW.`reductionFactor` IS NOT NULL AND NOT inClosedUnitInterval(NEW.`reductionFactor`) THEN
			SET @msg	= CONCAT('optimiseLessonCriteriaWeights: reductionFactor=', NEW.`reductionFactor`, ' is not in the closed unit-interval; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_optimiseLessonCriteriaWeights', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `tbl_optimiseLessonCriteriaWeights.beforeUpdate` BEFORE UPDATE ON `tbl_optimiseLessonCriteriaWeights`
	FOR EACH ROW /* updated */
	BEGIN
		IF NEW.`reductionFactor` IS NOT NULL AND NOT inClosedUnitInterval(NEW.`reductionFactor`) THEN
			SET @msg	= CONCAT('optimiseLessonCriteriaWeights: reductionFactor=', NEW.`reductionFactor`, ' is not in the closed unit-interval; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_optimiseLessonCriteriaWeights', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `lessonCriteriaWeights.beforeInsert` BEFORE INSERT ON `tbl_lessonCriteriaWeights`
	FOR EACH ROW /* inserted */
	BEGIN
		IF GREATEST(
			NEW.`areResourcesReused`,
			NEW.`greatestSynchronisedCourseSetSize`,
			NEW.`greatestMinimumConsecutiveLessons`,
			NEW.`greatestRemainingCourseLessons`,
			NEW.`isCoreKnowledgeRequirement`,
			NEW.`isSpecialistInTopic`,
			NEW.`matchCourseClassSizeToLocationCapacity`,
			NEW.`maximiseRelativeFacilityUtilisation`,
			NEW.`maximiseStudentClassSizeOverCourseClassSize`,
			NEW.`maximiseStudentClassSizeOverLocationCapacity`,
			NEW.`minimiseBookingAtAnotherCoursesSpecifiedTime`,
			NEW.`minimiseBookingOfLocationByOtherTeachers`,
			NEW.`minimiseDeviationFromTimeslotRequest`,
			NEW.`minimiseInterCampusMigrationsOfStudents`,
			NEW.`minimiseInterCampusMigrationsOfTeachers`,
			NEW.`minimiseStudentBodyCombinations`,
			NEW.`minimiseTeachersLocusOperandi`,
			NEW.`minimiseWasteOfScarceFacilities`
		) > 1.0 THEN
			SET @msg	= CONCAT('lessonCriteriaWeights: weight can\'t exceed one; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_lessonCriteriaWeights', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `lessonCriteriaWeights.beforeUpdate` BEFORE UPDATE ON `tbl_lessonCriteriaWeights`
	FOR EACH ROW /* updated */
	BEGIN
		IF GREATEST(
			NEW.`areResourcesReused`,
			NEW.`greatestSynchronisedCourseSetSize`,
			NEW.`greatestMinimumConsecutiveLessons`,
			NEW.`greatestRemainingCourseLessons`,
			NEW.`isCoreKnowledgeRequirement`,
			NEW.`isSpecialistInTopic`,
			NEW.`matchCourseClassSizeToLocationCapacity`,
			NEW.`maximiseRelativeFacilityUtilisation`,
			NEW.`maximiseStudentClassSizeOverCourseClassSize`,
			NEW.`maximiseStudentClassSizeOverLocationCapacity`,
			NEW.`minimiseBookingAtAnotherCoursesSpecifiedTime`,
			NEW.`minimiseBookingOfLocationByOtherTeachers`,
			NEW.`minimiseDeviationFromTimeslotRequest`,
			NEW.`minimiseInterCampusMigrationsOfStudents`,
			NEW.`minimiseInterCampusMigrationsOfTeachers`,
			NEW.`minimiseStudentBodyCombinations`,
			NEW.`minimiseTeachersLocusOperandi`,
			NEW.`minimiseWasteOfScarceFacilities`
		) > 1.0 THEN
			SET @msg	= CONCAT('lessonCriteriaWeights: weight can\'t exceed one; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_lessonCriteriaWeights', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `timetableCriteriaWeights.beforeInsert` BEFORE INSERT ON `tbl_timetableCriteriaWeights`
	FOR EACH ROW /* inserted */
	BEGIN
		IF GREATEST(
			NEW.`maximiseComplianceWithFreePeriodPreferences`,
			NEW.`maximiseMeanRatioOfStudentClassSizeToLocationCapacity`,
			NEW.`maximiseMeanStudentClassSize`,
			NEW.`maximiseSynchronisationOfSynchronisedCourses`,
			NEW.`maximiseWeightedMeanStudentBodyUtilisationRatio`,
			NEW.`minimiseAverageAbsoluteDeviationFromIdealTimeslotRequest`,
			NEW.`minimiseDispersionOfStudentFreePeriodsPerDay`,
			NEW.`minimiseDispersionOfTeacherFreePeriodsPerDay`,
			NEW.`minimiseDispersionOfTeacherWorkload`,
			NEW.`minimiseMeanInterCampusMigrationsOfStudents`,
			NEW.`minimiseMeanInterCampusMigrationsOfTeachers`,
			NEW.`minimiseMeanLocationChangesOfTeachers`,
			NEW.`minimiseMeanLocusOperandiOfTeachers`,
			NEW.`minimiseMeanRatioOfIncompletelyBookedCoreKnowledge`,
			NEW.`minimiseMeanRatioOfIncompletelyBookedOptionalKnowledge`,
			NEW.`minimiseMeanStudentBodyCombinationsPerLesson`,
			NEW.`minimiseRatioOfConsecutiveEqualLessons`,
			NEW.`minimiseRatioOfSeparatedEqualLessonsWithinAnyDay`
		) > 1.0 THEN
			SET @msg	= CONCAT('timetableCriteriaWeights: weight can\'t exceed one; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_timetableCriteriaWeights', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `timetableCriteriaWeights.beforeUpdate` BEFORE UPDATE ON `tbl_timetableCriteriaWeights`
	FOR EACH ROW /* updated */
	BEGIN
		IF GREATEST(
			NEW.`maximiseComplianceWithFreePeriodPreferences`,
			NEW.`maximiseMeanRatioOfStudentClassSizeToLocationCapacity`,
			NEW.`maximiseMeanStudentClassSize`,
			NEW.`maximiseSynchronisationOfSynchronisedCourses`,
			NEW.`maximiseWeightedMeanStudentBodyUtilisationRatio`,
			NEW.`minimiseAverageAbsoluteDeviationFromIdealTimeslotRequest`,
			NEW.`minimiseDispersionOfStudentFreePeriodsPerDay`,
			NEW.`minimiseDispersionOfTeacherFreePeriodsPerDay`,
			NEW.`minimiseDispersionOfTeacherWorkload`,
			NEW.`minimiseMeanInterCampusMigrationsOfStudents`,
			NEW.`minimiseMeanInterCampusMigrationsOfTeachers`,
			NEW.`minimiseMeanLocationChangesOfTeachers`,
			NEW.`minimiseMeanLocusOperandiOfTeachers`,
			NEW.`minimiseMeanRatioOfIncompletelyBookedCoreKnowledge`,
			NEW.`minimiseMeanRatioOfIncompletelyBookedOptionalKnowledge`,
			NEW.`minimiseMeanStudentBodyCombinationsPerLesson`,
			NEW.`minimiseRatioOfConsecutiveEqualLessons`,
			NEW.`minimiseRatioOfSeparatedEqualLessonsWithinAnyDay`
		) > 1.0 THEN
			SET @msg	= CONCAT('timetableCriteriaWeights: weight can\'t exceed one; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_timetableCriteriaWeights', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `locationCatalogue.beforeDelete` BEFORE DELETE ON `tbl_locationCatalogue`
	FOR EACH ROW /* deleted */
	BEGIN
		DELETE FROM `tbl_studentGroupMembership` WHERE `studentBodyRegisterId` IN (
			SELECT `studentBodyRegisterId` FROM `tbl_studentBodyRegister` WHERE `projectId` = OLD.`projectId`
		) AND `groupId` IN (
			SELECT `groupId` FROM `tbl_groupCatalogue` WHERE `projectId` = OLD.`projectId` AND `locationId` <=> OLD.`locationId`
		);

		DELETE FROM `tbl_teacherGroupMembership` WHERE `teacherRegisterId` IN (
			SELECT `teacherRegisterId` FROM `tbl_teacherRegister` WHERE `projectId` = OLD.`projectId`
		) AND `groupId` IN (
			SELECT `groupId` FROM `tbl_groupCatalogue` WHERE `projectId` = OLD.`projectId` AND `locationId` <=> OLD.`locationId`
		);
	END
//
CREATE TRIGGER `locationCatalogue.afterDelete` AFTER DELETE ON `tbl_locationCatalogue`
	FOR EACH ROW /* deleted */
	BEGIN
/*
	If one attempts to sequentially determine whether each requiredFacility lacks a corresponding facility, BEFORE any of the locations in the request are deleted,
	then one may find that each is still advertised by one of the other moribund locations in the request.
*/
		CALL deleteUnadvertisedRequiredFacilities(OLD.`projectId`);	-- Though facilities are deleted to preserve referential integrity, this won't fire the DELETE trigger for tbl_facility; we must do it manually.
	END
//
CREATE TRIGGER `locationCatalogue.beforeInsert` BEFORE INSERT ON `tbl_locationCatalogue`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_locationCatalogue';

		IF NEW.`locationId` = '' THEN
			SET @msg	= CONCAT('locationCatalogue: anonymous "locationId"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='locationId', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`availability` = '' THEN
			SET @msg	= CONCAT('locationCatalogue: insufficient "availability"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='availability', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`capacity` = 0 THEN
			SET @msg	= CONCAT('locationCatalogue: zero "capacity"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='capacity', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `locationCatalogue.beforeUpdate` BEFORE UPDATE ON `tbl_locationCatalogue`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_locationCatalogue';

		IF NEW.`locationId` = '' THEN
			SET @msg	= CONCAT('locationCatalogue: anonymous "locationId"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='locationId', MESSAGE_TEXT=@msg;
		ELSEIF EXISTS (
			SELECT 1 FROM `tbl_groupCatalogue` NATURAL JOIN `tbl_meetingTime` /* USING (`groupCatalogueId`) */ WHERE `projectId` = NEW.`projectId` AND `locationId` <=> NEW.`locationId` AND FIND_IN_SET(`day`, NEW.`availability`) = 0 /* unavailable */
		) THEN
			SET @msg	= CONCAT('locationCatalogue: "locationId" is unavailable @ the meeting-times of groups held there; where projectId=', NEW.`projectId`, '.');	-- CAVEAT: for security, don't print the `locationId` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='locationId', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`availability` = '' THEN
			SET @msg	= CONCAT('locationCatalogue: insufficient "availability"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='availability', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`capacity` = 0 THEN
			SET @msg	= CONCAT('locationCatalogue: zero "capacity"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='capacity', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `locationCatalogue.afterUpdate` AFTER UPDATE ON `tbl_locationCatalogue`
	FOR EACH ROW /* updated */
	BEGIN
/*
	If one attempts to sequentially determine whether each requiredFacility lacks a corresponding facility, BEFORE any of the locations in the request are deleted,
	then one may find that each is still advertised by one of the other moribund locations in the request.
*/
		IF NEW.`projectId` <> OLD.`projectId` /* the old location has effectively been deleted */ THEN
			CALL deleteUnadvertisedRequiredFacilities(OLD.`projectId`);
		END IF;
	END
//
CREATE TRIGGER `facility.afterDelete` AFTER DELETE ON `tbl_facility`
	FOR EACH ROW /* deleted */
	BEGIN
		SELECT `projectId` FROM `tbl_locationCatalogue` WHERE `locationCatalogueId` = OLD.`locationCatalogueId` INTO @oldProjectId;

		IF NOT facilityProvidersExist(@oldProjectId, OLD.`facilityTypeId`) THEN
			CALL deleteRequiredFacility(@oldProjectId, OLD.`facilityTypeId`);
		END IF;
	END
//
CREATE TRIGGER `facility.beforeInsert` BEFORE INSERT ON `tbl_facility`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_facility';

		IF NOT areLocationAndFacilityTypeInSameProject(NEW.`locationCatalogueId`, NEW.`facilityTypeId`) THEN
			SET @msg	= CONCAT('facility: locationCatalogueId=', NEW.`locationCatalogueId` ,' is not in the same project as facilityTypeId=', NEW.`facilityTypeId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `facility.beforeUpdate` BEFORE UPDATE ON `tbl_facility`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_facility';

		IF NOT areLocationAndFacilityTypeInSameProject(NEW.`locationCatalogueId`, NEW.`facilityTypeId`) THEN
			SET @msg	= CONCAT('facility: locationCatalogueId=', NEW.`locationCatalogueId` ,' is not in the same project as facilityTypeId=', NEW.`facilityTypeId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `facility.afterUpdate` AFTER UPDATE ON `tbl_facility`
	FOR EACH ROW /* updated */
	BEGIN
		SELECT `projectId` FROM `tbl_locationCatalogue` WHERE `locationCatalogueId` = OLD.`locationCatalogueId` INTO @oldProjectId;

		IF (
			NEW.`facilityTypeId` <> OLD.`facilityTypeId` OR (
				SELECT `projectId` FROM `tbl_locationCatalogue` WHERE `locationCatalogueId` = NEW.`locationCatalogueId`
			) <> @oldProjectId	-- The old facility has effectively been deleted.
		) AND NOT facilityProvidersExist(@oldProjectId, OLD.`facilityTypeId`) THEN
			CALL deleteRequiredFacility(@oldProjectId, OLD.`facilityTypeId`);
		END IF;
	END
//
-- This won't trigger on deleting a foreign key 'tbl_project.projectId', but it will cascade-delete the affected rows from 'tbl_studentGroupMembersip' & 'tbl_teacherGroupMembership', thus rendering the issue mute.
CREATE TRIGGER `groupCatalogue.beforeDelete` BEFORE DELETE ON `tbl_groupCatalogue`
	FOR EACH ROW /* deleted */
	BEGIN
		CALL deleteGroupMembership(OLD.`projectId`, OLD.`groupId`);
	END
//
CREATE TRIGGER `groupCatalogue.beforeInsert` BEFORE INSERT ON `tbl_groupCatalogue`
	FOR EACH ROW /* inserted */
	BEGIN
		IF NEW.`groupId` = '' THEN
			SET @msg	= CONCAT('groupCatalogue: anonymous "groupId"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_groupCatalogue', COLUMN_NAME='groupId', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `groupCatalogue.beforeUpdate` BEFORE UPDATE ON `tbl_groupCatalogue`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_groupCatalogue';

		IF NEW.`groupId` = '' THEN
			SET @msg	= CONCAT('groupCatalogue: anonymous "groupId"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='groupId', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`projectId` <> OLD.`projectId` OR NEW.`groupId` <> OLD.`groupId` /* the old group has effectively been deleted */ THEN
			CALL deleteGroupMembership(OLD.`projectId`, OLD.`groupId`);
		END IF;
	END
//
CREATE TRIGGER `meetingTime.beforeInsert` BEFORE INSERT ON `tbl_meetingTime`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_meetingTime';

		SELECT `projectId` FROM `tbl_groupCatalogue` WHERE `groupCatalogueId` = NEW.`groupCatalogueId` INTO @newProjectId;
		SELECT `min`, `max` FROM `tbl_timeslotIdBounds` WHERE `projectId` = @newProjectId INTO @min, @max;

		IF @max IS NULL THEN
			SET @msg	= CONCAT('meetingTime: first one must populate the timeslot-id bounds for projectId=', @newProjectId, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NEW.`timeslotId` NOT BETWEEN @min AND @max THEN
			SET @msg	= CONCAT('meetingTime: timeslotId=', NEW.`timeslotId`, ', not in closed interval [', @min, ', ', @max, ']; where groupCatalogueId=', NEW.`groupCatalogueId` , '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='timeslotId', MESSAGE_TEXT=@msg;
		ELSE
			SELECT `groupId`, `locationId`, `mandatesAttendance` FROM `tbl_groupCatalogue` WHERE `projectId` = @newProjectId AND `groupCatalogueId` = NEW.`groupCatalogueId` INTO @groupId, @locationId, @mandatesAttendance;

			IF @mandatesAttendance <=> TRUE THEN
				IF NOT areAvailableMemberStudentBodies(@newProjectId, @groupId, NEW.`day`) THEN
					SET @msg	= CONCAT('meetingTime: some student-bodies who\'re members of groupCatalogueId=', NEW.groupCatalogueId /* CAVEAT: for security, don't print the `groupId` which may contain arbitrary text */, ', which mandates attendance, are unavailable on "', NEW.`day`, '".');

					SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='day', MESSAGE_TEXT=@msg;
				ELSEIF NOT areAvailableMemberTeachers(@newProjectId, @groupId, NEW.`day`) THEN
					SET @msg	= CONCAT('meetingTime: some teachers who\'re members of groupCatalogueId=', NEW.groupCatalogueId /* CAVEAT: for security, don't print the `groupId` which may contain arbitrary text */, ', which mandates attendance, are unavailable on "', NEW.`day`, '".');

					SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='day', MESSAGE_TEXT=@msg;
				END IF;
			END IF;

			IF NOT @locationId IS NULL AND NOT isAvailableLocation(@newProjectId, @locationId, NEW.`day`) THEN
				SET @msg	= CONCAT('meetingTime: the specified "locationId" is unavailable on "', NEW.`day`, '"; where projectId=', @newProjectId, '.');	-- CAVEAT: for security, don't print the `locationId` which may contain arbitrary text.

				SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='day', MESSAGE_TEXT=@msg;
			END IF;
		END IF;
	END
//
CREATE TRIGGER `meetingTime.beforeUpdate` BEFORE UPDATE ON `tbl_meetingTime`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_meetingTime';

		SELECT `projectId` FROM `tbl_groupCatalogue` WHERE `groupCatalogueId` = NEW.`groupCatalogueId` INTO @newProjectId;
		SELECT `min`, `max` FROM `tbl_timeslotIdBounds` WHERE `projectId` = @newProjectId INTO @min, @max;

		IF @max IS NULL THEN
			SET @msg	= CONCAT('meetingTime: first one must populate the timeslot-id bounds for projectId=', @newProjectId, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NEW.`timeslotId` NOT BETWEEN @min AND @max THEN
			SET @msg	= CONCAT('meetingTime: timeslotId=', NEW.`timeslotId`, ', not in closed interval [', @min, ', ', @max, ']; where groupCatalogueId=', NEW.`groupCatalogueId` , '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='timeslotId', MESSAGE_TEXT=@msg;
		ELSE
			SELECT `groupId`, `locationId`, `mandatesAttendance` FROM `tbl_groupCatalogue` WHERE `projectId` = @newProjectId AND `groupCatalogueId` = NEW.`groupCatalogueId` INTO @groupId, @locationId, @mandatesAttendance;

			IF @mandatesAttendance <=> TRUE THEN
				IF NOT areAvailableMemberStudentBodies(@newProjectId, @groupId, NEW.`day`) THEN
					SET @msg	= CONCAT('meetingTime: some student-bodies who\'re members of groupCatalogueId=', NEW.groupCatalogueId /* CAVEAT: for security, don't print the `groupId` which may contain arbitrary text */, ', which mandates attendance, are unavailable on "', NEW.`day`, '".');

					SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='day', MESSAGE_TEXT=@msg;
				ELSEIF NOT areAvailableMemberTeachers(@newProjectId, @groupId, NEW.`day`) THEN
					SET @msg	= CONCAT('meetingTime: some teachers who\'re members of groupCatalogueId=', NEW.groupCatalogueId /* CAVEAT: for security, don't print the `groupId` which may contain arbitrary text */, ', which mandates attendance, are unavailable on "', NEW.`day`, '".');

					SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='day', MESSAGE_TEXT=@msg;
				END IF;
			END IF;

			IF NOT @locationId IS NULL AND NOT isAvailableLocation(@newProjectId, @locationId, NEW.`day`) THEN
				SET @msg	= CONCAT('meetingTime: the specified "locationId" is unavailable on "', NEW.`day`, '"; where projectId=', @newProjectId, '.');	-- CAVEAT: for security, don't print the `locationId` which may contain arbitrary text.

				SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='day', MESSAGE_TEXT=@msg;
			END IF;
		END IF;
	END
//
CREATE TRIGGER `studentBodyRegister.beforeInsert` BEFORE INSERT ON `tbl_studentBodyRegister`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_studentBodyRegister';

		IF NEW.`mnemonic` = '' THEN
			SET @msg	= CONCAT('studentBodyRegister: anonymous "mnemonic"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='mnemonic', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`availability` = '' THEN
			SET @msg	= CONCAT('studentBodyRegister: insufficient "availability"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='availability', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`teachingRatio` = 0 OR NEW.`teachingRatio` > 1.0 THEN
			SET @msg	= CONCAT('studentBodyRegister: "teachingRatio=', NEW.teachingRatio, ' must be in the semi-closed unit-interval (0,1]; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='teachingRatio', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `studentBodyRegister.beforeUpdate` BEFORE UPDATE ON `tbl_studentBodyRegister`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_studentBodyRegister';

		IF NEW.`mnemonic` = '' THEN
			SET @msg	= CONCAT('studentBodyRegister: anonymous "mnemonic"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='mnemonic', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`availability` = '' THEN
			SET @msg	= CONCAT('studentBodyRegister: insufficient "availability"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='availability', MESSAGE_TEXT=@msg;
		ELSEIF EXISTS (
			SELECT 1 FROM `tbl_groupCatalogue` NATURAL JOIN `tbl_meetingTime` /* USING (`groupCatalogueId`) */ WHERE `projectId` = NEW.`projectId` AND `groupId` IN (
				SELECT `groupId` FROM `tbl_studentGroupMembership` WHERE `studentBodyRegisterId` = NEW.`studentBodyRegisterId`
			) AND `mandatesAttendance` <=> TRUE AND FIND_IN_SET(day, NEW.`availability`) = 0 /* unavailable */
		) THEN
			SET @msg	= CONCAT('studentBodyRegister: some groups of which this student-body is a member, mandate attendance when they\'re unavailable; where projectId=', NEW.`projectId`, '.');	-- CAVEAT: for security, don't print the `mnemonic` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='availability', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`teachingRatio` = 0 OR NEW.`teachingRatio` > 1.0 THEN
			SET @msg	= CONCAT('studentBodyRegister: "teachingRatio=', NEW.teachingRatio, ' must be in the semi-closed unit-interval (0,1]; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='teachingRatio', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `studentBodyMembership.beforeInsert` BEFORE INSERT ON `tbl_studentBodyMembership`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_studentBodyMembership';

		IF NEW.`studentId` = '' THEN
			SET @msg	= CONCAT('studentBodyMembership: anonymous "studentId"; where studentBodyRegisterId=', NEW.`studentBodyRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='studentId', MESSAGE_TEXT=@msg;
		ELSEIF studentExists(NEW.`studentBodyRegisterId`, NEW.`studentId`) THEN
			SET @msg	= CONCAT('studentBodyMembership: the specified "studentId" already exists; where projectId=', (SELECT `projectId` FROM `tbl_studentBodyRegister` WHERE `studentBodyRegisterId` = NEW.`studentBodyRegisterId`), '.');	-- CAVEAT: for security, don't print the `studentId` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='studentId', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `studentBodyMembership.beforeUpdate` BEFORE UPDATE ON `tbl_studentBodyMembership`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_studentBodyMembership';

		IF NEW.`studentId` = '' THEN
			SET @msg	= CONCAT('studentBodyMembership: anonymous "studentId"; where studentBodyRegisterId=', NEW.`studentBodyRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='studentId', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`studentId` <> OLD.`studentId` && studentExists(NEW.`studentBodyRegisterId`, NEW.`studentId`) THEN
			SET @msg	= CONCAT('studentBodyMembership: the specified "studentId" already exists; where projectId=', (SELECT `projectId` FROM `tbl_studentBodyRegister` WHERE `studentBodyRegisterId` = NEW.`studentBodyRegisterId`), '.');	-- CAVEAT: for security, don't print the `studentId` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='studentId', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `studentGroupMembership.beforeInsert` BEFORE INSERT ON `tbl_studentGroupMembership`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_studentGroupMembership';

		SELECT `projectId`, `availability` FROM `tbl_studentBodyRegister` WHERE `studentBodyRegisterId` = NEW.`studentBodyRegisterId` INTO @projectId, @availability;

		IF NOT groupIdByStudentBodyRegisterIdExists(NEW.`studentBodyRegisterId`, NEW.`groupId`) THEN
			SET @msg	= CONCAT('studentGroupMembership: the specified "groupId" doesn\'t exist; where studentBodyRegisterId=', NEW.`studentBodyRegisterId`, '.');	-- CAVEAT: for security, don't print the `groupId` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NOT isAvailableGroupForAllMandatoryMeetings(@projectId, NEW.`groupId`, @availability) THEN
			SET @msg	= CONCAT('studentGroupMembership: some groups of which studentBodyRegisterId=', NEW.`studentBodyRegisterId`, ' is a member, mandate attendance when they\'re unavailable; where projectId=', @projectId, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `studentGroupMembership.beforeUpdate` BEFORE UPDATE ON `tbl_studentGroupMembership`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_studentGroupMembership';

		SELECT `projectId`, `availability` FROM `tbl_studentBodyRegister` WHERE `studentBodyRegisterId` = NEW.`studentBodyRegisterId` INTO @projectId, @availability;

		IF NOT groupIdByStudentBodyRegisterIdExists(NEW.`studentBodyRegisterId`, NEW.`groupId`) THEN
			SET @msg	= CONCAT('studentGroupMembership: the specified "groupId" doesn\'t exist; where studentBodyRegisterId=', NEW.`studentBodyRegisterId`, '.');	-- CAVEAT: for security, don't print the `groupId` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NOT isAvailableGroupForAllMandatoryMeetings(@projectId, NEW.`groupId`, @availability) THEN
			SET @msg	= CONCAT('studentGroupMembership: some groups of which studentBodyRegisterId=', NEW.`studentBodyRegisterId`, ' is a member, mandate attendance when they\'re unavailable; where projectId=', @projectId, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `teacherRegister.afterDelete` AFTER DELETE ON `tbl_teacherRegister`
	FOR EACH ROW /* deleted */
	BEGIN
		CALL deleteKnowledgeRequirementsLackingCourse(OLD.`projectId`);
	END
//
CREATE TRIGGER `teacherRegister.beforeInsert` BEFORE INSERT ON `tbl_teacherRegister`
	FOR EACH ROW /* inserted */
	BEGIN
		DECLARE thisTeacherRegisterId	INT UNSIGNED;
		DECLARE thisAvailability	SET('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
		DECLARE done			BOOL	DEFAULT FALSE;
		DECLARE locationCursor		CURSOR FOR SELECT `teacherRegisterId`, `availability` FROM `tbl_teacherRegister` WHERE `projectId` = NEW.`projectId` AND `locationId` <=> NEW.`locationId` /* CAVEAT: both could be NULL */;

		DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

		SET @tableName	= 'tbl_teacherRegister';

		IF NEW.`teacherId` = '' THEN
			SET @msg	= CONCAT('teacherRegister: anonymous "teacherId"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='teacherId', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`availability` = '' THEN
			SET @msg	= CONCAT('teacherRegister: insufficient "availability"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='availability', MESSAGE_TEXT=@msg;
		ELSEIF NOT inClosedUnitInterval(NEW.`maximumTeachingRatio`) THEN
			SET @msg	= CONCAT('teacherRegister: maximumTeachingRatio=', NEW.`maximumTeachingRatio`, ' is not in the closed unit-interval; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='maximumTeachingRatio', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`locationId` IS NOT NULL THEN
			OPEN locationCursor;

			locationLoop: LOOP
				FETCH locationCursor INTO thisTeacherRegisterId, thisAvailability;

				IF done THEN
					LEAVE locationLoop;
				ELSEIF thisAvailability & /* intersection */ NEW.`availability` THEN
					SET @msg	= CONCAT('teacherRegister: the specified "locationId" is taken by teacherRegisterId=', thisTeacherRegisterId, ' whose availability="', thisAvailability, '" intersects.');	-- CAVEAT: for security, don't print the `locationId` which may contain arbitrary text.

					SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='locationId', MESSAGE_TEXT=@msg;
				END IF;
			END LOOP locationLoop;

			CLOSE locationCursor;
		END IF;
	END
//
CREATE TRIGGER `teacherRegister.beforeUpdate` BEFORE UPDATE ON `tbl_teacherRegister`
	FOR EACH ROW /* updated */
	BEGIN
		DECLARE thisTeacherRegisterId	INT UNSIGNED;
		DECLARE thisAvailability	SET('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
		DECLARE done			BOOL	DEFAULT FALSE;
		DECLARE locationCursor		CURSOR FOR SELECT `teacherRegisterId`, `availability` FROM `tbl_teacherRegister` WHERE `projectId` = NEW.`projectId` AND `locationId` <=> NEW.`locationId` /* CAVEAT: both could be NULL */;

		DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

		SET @tableName	= 'tbl_teacherRegister';

		IF NEW.`teacherId` = '' THEN
			SET @msg	= CONCAT('teacherRegister: anonymous "teacherId"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='teacherId', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`availability` = '' THEN
			SET @msg	= CONCAT('teacherRegister: insufficient "availability"; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='availability', MESSAGE_TEXT=@msg;
		ELSEIF EXISTS (
			SELECT 1 FROM `tbl_groupCatalogue` NATURAL JOIN `tbl_meetingTime` /* USING (`groupCatalogueId`) */ WHERE `projectId` = NEW.`projectId` AND `groupId` IN (
				SELECT `groupId` FROM `tbl_teacherGroupMembership` WHERE `teacherRegisterId` = NEW.`teacherRegisterId`
			) AND `mandatesAttendance` <=> TRUE AND FIND_IN_SET(day, NEW.`availability`) = 0 /* unavailable */
		) THEN
			SET @msg	= CONCAT('teacherRegister: some groups of which the specified "teacherId" is a member, mandate attendance when they\'re unavailable; where projectId=', NEW.`projectId`, '.');	-- CAVEAT: for security, don't print the `teacherId` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='availability', MESSAGE_TEXT=@msg;
		ELSEIF NOT inClosedUnitInterval(NEW.`maximumTeachingRatio`) THEN
			SET @msg	= CONCAT('teacherRegister: maximumTeachingRatio=', NEW.`maximumTeachingRatio`, ' is not in the closed unit-interval; where projectId=', NEW.`projectId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='maximumTeachingRatio', MESSAGE_TEXT=@msg;
		ELSE
			IF NEW.`locationId` IS NOT NULL AND (
				NEW.`projectId` <> OLD.`projectId` OR NEW.`availability` <> OLD.`availability` OR NEW.`locationId` <=> OLD.`locationId`
			) THEN
				OPEN locationCursor;

				locationLoop: LOOP
					FETCH locationCursor INTO thisTeacherRegisterId, thisAvailability;

					IF done THEN
						LEAVE locationLoop;
					ELSEIF thisTeacherRegisterId <> OLD.`teacherRegisterId` AND thisAvailability & /* intersection */ NEW.`availability` THEN
						SET @msg	= CONCAT('teacherRegister: the specified "locationId" is taken by teacherRegisterId=', thisTeacherRegisterId, ' whose availability="', thisAvailability, '" intersects.');	-- CAVEAT: for security, don't print the `locationId` which may contain arbitrary text.

						SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='locationId', MESSAGE_TEXT=@msg;
					END IF;
				END LOOP locationLoop;

				CLOSE locationCursor;
			END IF;
		END IF;
	END
//
CREATE TRIGGER `teacherRegister.afterUpdate` AFTER UPDATE ON `tbl_teacherRegister`
	FOR EACH ROW /* updated */
	BEGIN
		IF NEW.`projectId` <> OLD.`projectId` /* the old teacher has effectively been deleted */ THEN
			CALL deleteKnowledgeRequirementsLackingCourse(OLD.`projectId`);
		END IF;
	END
//
CREATE TRIGGER `teacherGroupMembership.beforeInsert` BEFORE INSERT ON `tbl_teacherGroupMembership`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_teacherGroupMembership';

		SELECT `projectId`, `availability` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = NEW.`teacherRegisterId` INTO @projectId, @availability;

		IF NOT groupIdByTeacherRegisterIdExists(NEW.`teacherRegisterId`, NEW.`groupId`) THEN
			SET @msg	= CONCAT('teacherGroupMembership: the specified "groupId" doesn\'t exist; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');	-- CAVEAT: for security, don't print the `groupId` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NOT isAvailableGroupForAllMandatoryMeetings(@projectId, NEW.`groupId`, @availability) THEN
			SET @msg	= CONCAT('teacherGroupMembership: some groups of which teacherRegisterId=', NEW.`teacherRegisterId`, ' is a member, mandate attendance when they\'re unavailable; where projectId=', @projectId, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `teacherGroupMembership.beforeUpdate` BEFORE UPDATE ON `tbl_teacherGroupMembership`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_teacherGroupMembership';

		SELECT `projectId`, `availability` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = NEW.`teacherRegisterId` INTO @projectId, @availability;

		IF NOT groupIdByTeacherRegisterIdExists(NEW.`teacherRegisterId`, NEW.`groupId`) THEN
			SET @msg	= CONCAT('teacherGroupMembership: the specified "groupId" doesn\'t exist; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');	-- CAVEAT: for security, don't print the `groupId` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NOT isAvailableGroupForAllMandatoryMeetings(@projectId, NEW.`groupId`, @availability) THEN
			SET @msg	= CONCAT('teacherGroupMembership: some groups of which teacherRegisterId=', NEW.`teacherRegisterId`, ' is a member, mandate attendance when they\'re unavailable; where projectId=', @projectId, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `service.beforeDelete` BEFORE DELETE ON `tbl_service`
	FOR EACH ROW /* deleted */
	BEGIN
		CALL deleteMatchingRequiredFacilities(OLD.`teacherRegisterId`, OLD.`topic`, OLD.`level`);
		CALL deleteSpecificTimeRequests(OLD.`teacherRegisterId`, OLD.`topic`, OLD.`level`);
	END
//
CREATE TRIGGER `service.afterDelete` AFTER DELETE ON `tbl_service`
	FOR EACH ROW /* deleted */
	BEGIN
		SELECT `projectId` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = OLD.`teacherRegisterId` INTO @oldProjectId;
/*
	If one attempts to sequentially determine whether each knowledge-requirement lacks a teacher, BEFORE any of the courses in the request are deleted,
	then one may find that each is still offered by the other moribund courses in the request.
*/
		CALL deleteKnowledgeRequirements(@oldProjectId, OLD.`topic`, OLD.`level`);
	END
//
CREATE TRIGGER `service.beforeInsert` BEFORE INSERT ON `tbl_service`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_service';

		SELECT `min`, `max` FROM `tbl_teacherRegister` NATURAL JOIN `tbl_timeslotIdBounds` /* USING (`projectId`) */ WHERE `teacherRegisterId` = NEW.`teacherRegisterId` INTO @min, @max;

		SET @timeslotsPerDay	= 1 + @max - @min;

		IF NEW.`minimumConsecutiveLessons` > NEW.`requiredLessonsPerWeek` THEN
			SET @msg	= CONCAT('service: minimumConsecutiveLessons=', NEW.`minimumConsecutiveLessons`, ' exceeds requiredLessonsPerWeek=', NEW.`requiredLessonsPerWeek`, '; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NEW.`minimumConsecutiveLessons` = 0 THEN
			SET @msg	= CONCAT('service: "minimumConsecutiveLessons" must exceed zero; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='minimumConsecutiveLessons', MESSAGE_TEXT=@msg;
		ELSEIF @max IS NULL THEN
			SET @msg	= CONCAT('service: first one must populate the timeslot-id bounds for projectId=', (SELECT `projectId` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = NEW.`teacherRegisterId`), '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NEW.`minimumConsecutiveLessons` > @timeslotsPerDay THEN
			SET @msg	= CONCAT('service: minimumConsecutiveLessons=', NEW.`minimumConsecutiveLessons`, ' exceeds time-slots/day=', @timeslotsPerDay, '; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='minimumConsecutiveLessons', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`maximumClassSize` <=> 0 THEN
			SET @msg	= CONCAT('service: "maximumClassSize" must exceed zero; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='maximumClassSize', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`idealTimeslotRequest` IS NOT NULL AND NEW.`idealTimeslotRequest` NOT BETWEEN @min AND @max THEN
			SET @msg	= CONCAT('service: idealTimeslotRequest=', NEW.`idealTimeslotRequest`, ' is not in the closed interval [', @min, ', ', @max, ']; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='idealTimeslotRequest', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `service.beforeUpdate` BEFORE UPDATE ON `tbl_service`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_service';

		SELECT `projectId` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = OLD.`teacherRegisterId` INTO @oldProjectId;
		SELECT `projectId` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = NEW.`teacherRegisterId` INTO @newProjectId;
		SELECT `min`, `max` FROM `tbl_teacherRegister` NATURAL JOIN `tbl_timeslotIdBounds` /* USING (`projectId`) */ WHERE `teacherRegisterId` = NEW.`teacherRegisterId` INTO @min, @max;

		SET @timeslotsPerDay	= 1 + @max - @min;

		IF NEW.`minimumConsecutiveLessons` > NEW.`requiredLessonsPerWeek` THEN
			SET @msg	= CONCAT('service: minimumConsecutiveLessons=', NEW.`minimumConsecutiveLessons`, ' exceeds requiredLessonsPerWeek=', NEW.`requiredLessonsPerWeek`, '; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NEW.`minimumConsecutiveLessons` = 0 THEN
			SET @msg	= CONCAT('service: "minimumConsecutiveLessons" must exceed zero; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='minimumConsecutiveLessons', MESSAGE_TEXT=@msg;
		ELSEIF @max IS NULL THEN
			SET @msg	= CONCAT('service: first one must populate the timeslot-id bounds for projectId=', @newProjectId, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NEW.`minimumConsecutiveLessons` > @timeslotsPerDay THEN
			SET @msg	= CONCAT('service: minimumConsecutiveLessons=', NEW.`minimumConsecutiveLessons`, ' exceeds time-slots/day=', @timeslotsPerDay, '; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='minimumConsecutiveLessons', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`maximumClassSize` <=> 0 THEN
			SET @msg	= CONCAT('service: "maximumClassSize" must exceed zero; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='maximumClassSize', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`idealTimeslotRequest` IS NOT NULL AND NEW.`idealTimeslotRequest` NOT BETWEEN @min AND @max THEN
			SET @msg	= CONCAT('service: idealTimeslotRequest=', NEW.`idealTimeslotRequest`, ' is not in the closed interval [', @min, ', ', @max, ']; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='idealTimeslotRequest', MESSAGE_TEXT=@msg;
		ELSEIF NEW.`teacherRegisterId` <> OLD.`teacherRegisterId` OR NEW.`topic` <> OLD.`topic` OR NEW.`level` <> OLD.`level` THEN	-- The course has effectively been deleted.
			CALL deleteMatchingRequiredFacilities(OLD.`teacherRegisterId`, OLD.`topic`, OLD.`level`);
			CALL deleteSpecificTimeRequests(OLD.`teacherRegisterId`, OLD.`topic`, OLD.`level`);
		END IF;
	END
//
CREATE TRIGGER `service.afterUpdate` AFTER UPDATE ON `tbl_service`
	FOR EACH ROW /* updated */
	BEGIN
		SELECT `projectId` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = OLD.`teacherRegisterId` INTO @oldProjectId;

		IF @newProjectId <> @oldProjectId OR NEW.`topic` <> OLD.`topic` OR NEW.`level` <> OLD.`level` THEN	-- The course has effectively been deleted from this teacher's service.
/*
	If one attempts to sequentially determine whether each knowledge-requirement lacks a teacher, BEFORE any of the courses in the request are updated,
	then one may find that each is still offered by the other courses in the request.
*/
			CALL deleteKnowledgeRequirements(@oldProjectId, OLD.`topic`, OLD.`level`);
		END IF;
	END
//
CREATE TRIGGER `specificTimeRequest.beforeInsert` BEFORE INSERT ON `tbl_specificTimeRequest`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_specificTimeRequest';

		SELECT `projectId` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = NEW.`teacherRegisterId` INTO @newProjectId;
		SELECT `min`, `max` FROM `tbl_timeslotIdBounds` WHERE `projectId` = @newProjectId INTO @min, @max;

		IF @max IS NULL THEN
			SET @msg	= CONCAT('specificTimeRequest: first one must populate the timeslot-id bounds for projectId=', @newProjectId, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NEW.`timeslotId` NOT BETWEEN @min AND @max THEN
			SET @msg	= CONCAT('specificTimeRequest: timeslotId=', NEW.`timeslotId`, ' is not in the closed interval [', @min, ', ', @max, ']; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='timeslotId', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `specificTimeRequest.beforeUpdate` BEFORE UPDATE ON `tbl_specificTimeRequest`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_specificTimeRequest';

		SELECT `projectId` FROM `tbl_teacherRegister` WHERE `teacherRegisterId` = NEW.`teacherRegisterId` INTO @newProjectId;
		SELECT `min`, `max` FROM `tbl_timeslotIdBounds` WHERE `projectId` = @newProjectId INTO @min, @max;

		IF @max IS NULL THEN
			SET @msg	= CONCAT('specificTimeRequest: first one must populate the timeslot-id bounds for projectId=', @newProjectId, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NEW.`timeslotId` NOT BETWEEN @min AND @max THEN
			SET @msg	= CONCAT('specificTimeRequest: timeslotId=', NEW.`timeslotId`, ' is not in the closed interval [', @min, ', ', @max, ']; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, COLUMN_NAME='timeslotId', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `requiredFacility.beforeInsert` BEFORE INSERT ON `tbl_requiredFacility`
	FOR EACH ROW /* inserted */
	BEGIN
		SET @tableName	= 'tbl_requiredFacility';

		IF NOT areTeacherAndFacilityTypeInSameProject(NEW.`teacherRegisterId`, NEW.`facilityTypeId`) THEN
			SET @msg	= CONCAT('requiredFacility: teacherRegisterId=', NEW.`teacherRegisterId` ,' is not in the same project as facilityTypeId=', NEW.`facilityTypeId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NOT isAvailableFacility(NEW.`teacherRegisterId`, NEW.`facilityTypeId`) THEN
			SET @msg	= CONCAT('requiredFacility: zero locations offer facilityTypeId=', NEW.facilityTypeId ,'; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `requiredFacility.beforeUpdate` BEFORE UPDATE ON `tbl_requiredFacility`
	FOR EACH ROW /* updated */
	BEGIN
		SET @tableName	= 'tbl_requiredFacility';

		IF NOT areTeacherAndFacilityTypeInSameProject(NEW.`teacherRegisterId`, NEW.`facilityTypeId`) THEN
			SET @msg	= CONCAT('requiredFacility: teacherRegisterId=', NEW.`teacherRegisterId` ,' is not in the same project as facilityTypeId=', NEW.`facilityTypeId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		ELSEIF NOT isAvailableFacility(NEW.`teacherRegisterId`, NEW.`facilityTypeId`) THEN
			SET @msg	= CONCAT('requiredFacility: zero locations offer facilityTypeId=', NEW.facilityTypeId ,'; where teacherRegisterId=', NEW.`teacherRegisterId`, '.');

			SIGNAL SQLSTATE '45000' SET TABLE_NAME=@tableName, MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `knowledgeRequirement.beforeInsert` BEFORE INSERT ON `tbl_knowledgeRequirement`
	FOR EACH ROW /* inserted */
	BEGIN
		SELECT `projectId` FROM `tbl_studentBodyRegister` WHERE `studentBodyRegisterId` = NEW.`studentBodyRegisterId` INTO @projectId;

		IF matchingCourseExists(@projectId, NEW.`topic`, NEW.`level`) = 0 THEN
			SET @msg	= CONCAT('knowledgeRequirement: zero services match both the specified "topic" & "level"; where projectId=', @projectId, '.');	-- CAVEAT: for security, don't print either the `topic` or the `level` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_knowledgeRequirement', MESSAGE_TEXT=@msg;
		END IF;
	END
//
CREATE TRIGGER `knowledgeRequirement.beforeUpdate` BEFORE UPDATE ON `tbl_knowledgeRequirement`
	FOR EACH ROW /* updated */
	BEGIN
		SELECT `projectId` FROM `tbl_studentBodyRegister` WHERE `studentBodyRegisterId` = NEW.`studentBodyRegisterId` INTO @projectId;

		IF matchingCourseExists(@projectId, NEW.`topic`, NEW.`level`) = 0 THEN
			SET @msg	= CONCAT('knowledgeRequirement: zero services match both the specified "topic" & "level"; where projectId=', @projectId, '.');	-- CAVEAT: for security, don't print either the `topic` or the `level` which may contain arbitrary text.

			SIGNAL SQLSTATE '45000' SET TABLE_NAME='tbl_knowledgeRequirement', MESSAGE_TEXT=@msg;
		END IF;
	END
//
DELIMITER ;

