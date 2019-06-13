-- Copyright (C) 2014-2015 Dr. Alistair Ward
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

SET foreign_key_checks=1;	-- A Server System Variable; see 'SHOW VARIABLES;'.

-- Drop all the tables we're about to create, but in reverse order.
DROP TABLE IF EXISTS `tbl_knowledgeRequirement`, `tbl_requiredFacility`, `tbl_specificTimeRequest`, `tbl_specialtyTopic`, `tbl_service`, `tbl_teacherGroupMembership`, `tbl_teacherRegister`, `tbl_studentGroupMembership`, `tbl_studentBodyMembership`, `tbl_studentBodyRegister`, `tbl_stream`, `tbl_meetingTime`, `tbl_groupCatalogue`, `tbl_facility`, `tbl_facilityType`, `tbl_locationCatalogue`, `tbl_timetableCriteriaWeights`, `tbl_lessonCriteriaWeights`, `tbl_optimiseLessonCriteriaWeights`, `tbl_evolutionStrategies`, `tbl_outputOptions`, `tbl_style`, `tbl_traversalOrder`, `tbl_executionOptions`, `tbl_timetableValidationSwitches`, `tbl_problemValidationSwitches`, `tbl_timeslotIdBounds`, `tbl_options`, `tbl_configVersion`, `tbl_project`, `tbl_user`;

CREATE TABLE `tbl_user` (
	`userId`		INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY			COMMENT 'Unique row-identifier.',
	`name`			VARCHAR(64)	NOT NULL					COMMENT 'The real name of the user. This is free-format & not necessarily unique, because of which it\'s for documentary purposes only.',
	`emailAddress`		VARCHAR(255)	NOT NULL					COMMENT 'Only used to guarantee uniqueness amongst users.',
	`salt`			BINARY(64)			DEFAULT NULL			COMMENT 'Prepended to the password before applying a cryptographic hash. Only NULL when "saltedPasswordHash" is.',
	`saltedPasswordHash`	BINARY(64)			DEFAULT NULL			COMMENT 'Cryptographic hash of the salt prepended to the password. Field-length is adequate for SHA-512 stored in raw (not hex-encoded) binary. Password-checking will be disabled if set to NULL.',
	`executable`		BOOL		NOT NULL	DEFAULT TRUE			COMMENT 'Allows the site-administrator to prevent an account from running any projects.',
	`mutable`		BOOL		NOT NULL	DEFAULT TRUE			COMMENT 'Permits a site-administrator to prevent any changes to an account.',
	`created`		TIMESTAMP	NOT NULL	DEFAULT CURRENT_TIMESTAMP	COMMENT 'When this account was created.',
	`lastLogin`		TIMESTAMP	NULL		DEFAULT NULL			COMMENT 'The last time this user was authenticated.',
	CONSTRAINT `user.unique` UNIQUE KEY (`emailAddress`)					COMMENT 'Real email-addresses are unique by nature, though the existence of an MX-record is not checked here.'
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Defines the users of the application. N.B.: raw BINARY data can be inserted in hex, or queried using "SELECT hex(`salt`),hex(`saltedPasswordHash`)".';

CREATE TABLE `tbl_project` (
	`userId`	INT UNSIGNED	NOT NULL					COMMENT 'Each user has an independent set of projects.',
	`projectName`	VARCHAR(32)	NOT NULL					COMMENT 'A free-format personal mnemonic to qualify the "projectId.',
	`projectId`	INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY			COMMENT 'Unique row-identifier.',
	`created`	TIMESTAMP	NOT NULL	DEFAULT CURRENT_TIMESTAMP	COMMENT 'When this project was created.',
	CONSTRAINT `project.unique` UNIQUE KEY (`userId`, `projectName`)		COMMENT 'Within the domain of a "userId", each "projectName" is distinct.',
	CONSTRAINT `project.userId` FOREIGN KEY (`userId`) REFERENCES `tbl_user` (`userId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Each user can maintain several independent projects.';

CREATE TABLE `tbl_configVersion` (
	`configVersion`	VARCHAR(16)	PRIMARY KEY	COMMENT 'A possible version of the database-schema, in dotted-decimal.'
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Defines the set of valid configuration-versions to which a project can conform; see "tbl_options".';

CREATE TABLE `tbl_options` (
	`projectId`	INT UNSIGNED	PRIMARY KEY	COMMENT 'Each project defines its own options.',
	`configVersion`	VARCHAR(16)	NOT NULL	COMMENT 'The version of the database-schema to which this project conforms.',
	CONSTRAINT `options.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE,
	CONSTRAINT `options.configVersion` FOREIGN KEY (`configVersion`) REFERENCES `tbl_configVersion` (`configVersion`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1 COMMENT='Defines the version of the database-schema to which each project conforms.';

CREATE TABLE `tbl_timeslotIdBounds` (
	`projectId`	INT UNSIGNED	PRIMARY KEY			COMMENT 'Each project defines its own time-slot bounds.',
	`min`		TINYINT		NOT NULL	DEFAULT 0	COMMENT 'The lower inclusive bound for the time-slots in each day.',
	`max`		TINYINT		NOT NULL			COMMENT 'The upper inclusive bound for the time-slots in each day. There\'s no significance to the specific integer used, only the range of values.',
	CONSTRAINT `timeslotIdBounds.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Defines the range of timeslot-identifiers.';

-- Some of these switches have been disabled, since they're already implemented by native referential integrity checks.
CREATE TABLE `tbl_problemValidationSwitches` (
	`projectId`							INT UNSIGNED	PRIMARY KEY			COMMENT 'Each project defines its own switches.',
	`checkAvailabilityOfAnyGroupMember`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that @ least one member, can attend each meeting of a group.',
	`checkCapacityOfLocationsForMeetings`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that any location, specified for the meetings of each group, has adequate capacity.',
	`checkCoursesForSynchronousSpecifiedTimes`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that no courses, required to satisfy the knowledge-requirements of a student, specify the same booking-time.',
	`checkDuplicateMeetingLocationIds`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that when more than one group requires a location, that their meeting-times are mutually exclusive.',
--	`checkDuplicateOwnLocationIds`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that when more than one teacher claims a single location as their own, their working-weeks are mutually exclusive.',
	`checkForAlternativesToSynchronisedCourses`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Ensure that students can migrate easily between synchronised courses, by prohibiting any teacher from offering an alternative course outside the synchronised set, but with an identical subject to one of the member-courses.',
--	`checkForDuplicateStudentIds`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that no student-identifier belongs to more than one student-body.',
	`checkForIdleStudents`						BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for students who require zero subjects, but have allocated one or more time-slots for teaching.',
	`checkForIdleTeachers`						BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for teachers who offer zero courses required by students, but have allocated one or more time-slots for teaching.',
--	`checkForInvalidMeetingTimes`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all the timeslot-Ids, used to define the meeting-times of groups, are within permissible bounds.',
	`checkForMultipleCoursesPerTeacherPerSynchronisationId`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that no teacher offers more than one course with the same "synchronisationId".',
	`checkForNonExistentFacilities`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether all facilities, referenced by those courses offered by teachers, exist in @ least one location.',
--	`checkForNonExistentGroupIds`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether all "groupId"s, referenced by human-resources, exist independently.',
--	`checkForNonExistentMeetingLocationIds`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether all "locationId"s, required for the meetings of groups, exist.',
--	`checkForNonExistentOwnLocationIds`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether all "locationId"s, referenced by teachers, exist independently.',
	`checkForOverloadedStudents`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for students who have allocated zero time-slots for teaching, but have requested one or more subjects.',
	`checkForSingletonSynchronisedCourses`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for teachers whose courses are members of singleton sets of cynchronised courses.',
	`checkForStudentsRequiringMultipleSynchronisedSubjects`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for any student-body requiring multiple subjects only offered within one set of synchronised courses.',
	`checkForStudentsWithUnrealisableFreePeriodPreference`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for any student-body whose preference for the position of free time-slots, within those days when they are available, can never be realised because of the meeting-times of groups of which they are members.',
	`checkForSynchronisedCoursesWithDifferentIdealTimeslots`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that synchronised courses don\'t define different "idealTimeslotRequest"s.',
	`checkForSynchronisedCoursesWithDifferentLessonsPerWeek`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that synchronised courses don\'t define different "requiredLessonsPerWeek".',
	`checkForSynchronisedCoursesWithExcessLessonsPerWeek`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for synchronised courses which require more lessons per week, than the number of time-slots for which all the interested student-bodies & required teachers, are simultaneously available.',
	`checkForSynchronisedCoursesWithExcessSpecifiedTimes`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for synchronised courses which have fewer lessons per week than specified booking-times.',
	`checkForSynchronisedCoursesWithExcessTimeslotRequests`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for synchronised courses which both define "idealTimeslotRequest" & have an entry in "tbl_specificTimeRequest".',
	`checkForSynchronisedCoursesWithoutSuitableLocations`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for synchronised courses which lack sufficient locations offering the required facilities, which are simultaneously available to the teachers & all interested students.',
	`checkForSynchronisedCoursesWithUnavailableSpecifiedDays`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for synchronised courses which specify times, on days when not all the interested student-bodies & required teachers, are available.',
	`checkForTeachersWithUnrealisableFreePeriodPreference`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for any teacher whose preference for the position of free time-slots, within those days when they are available, can never be realised because of the meeting-times of groups of which they are members.',
	`checkIfStudentBodiesExceedTeachers`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether there are sufficient teachers for the number of student-bodies, on each day.',
	`checkIfStudentBodySizeExceedsCapacityOfAllLocations`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether each student-body can be accommodated by the largest available location.',
	`checkIfStudentBodySizeExceedsLocationCapacity`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether all student-bodies can be accommodated within the available locations, on each day.',
	`checkIndependenceOfStudentTimeslotsRequestsAndMeetings`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that none of the courses a teacher offers, specifies a booking-time, which matches any of the meeting-times of the groups of which they are members.',
	`checkIndependenceOfTeacherTimeslotsRequestsAndMeetings`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that none of the subjects a student requires, are offered by courses which all specify a booking-time, which matches any of the meeting-times of the groups of which they are members.',
	`checkLocationsAvailabilityToSupportCourses`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that there are sufficient locations, simultaneously available with at least one student-body & at least one teacher, to support the total number of time-slots required for the courses offered by all teachers.',
	`checkLocationsForSynchronousSpecifiedTimes`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check there are enough locations, in which to book lessons, @ those times specified by different teachers.',
	`checkMeetingLocationsAvailability`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that any location required for a group, is available @ all meeting-times.',
--	`checkMinimumConsecutiveLessons`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that the "minimumConsecutiveLessons" of a course, doesn\'t exceed the number of time-slots per day.',
--	`checkNullGroupId`						BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for null identifiers in the group-catalogue.',
	`checkNullLocationCatalogue`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether "tbl_locationCatalogue" is empty for a specified "projectId".',
	`checkNullStudentBodyRegister`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether "tbl_studentBodyRegister" is empty for a specified "projectId".',
	`checkNullTeacherRegister`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check whether "tbl_teacherRegister" is empty for a specified "projectId".',
	`checkOwnLocationsAvailability`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that if teachers specify their own location, that there\'s some common availability.',
	`checkRequiredLessonsPerWeek`					BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that no course is offered, where "requiredLessonsPerWeek" exceeds those the teacher works.',
	`checkSimultaneousAvailabilityOfGroupMembers`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all the members of each group, are simultaneously available.',
	`checkStudentsAvailabilityForMandatoryMeetings`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that students are available for the meeting-times of the groups of which they are members.',
	`checkStudentsAvailabilityForSpecifiedTimes`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that no course, required to satisfy the knowledge-requirements of a student, specifies any time, on a day when they are unavailable.',
--	`checkStudentsForMultipleLevelsOfSameTopic`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that the knowledge-requirements of each student-body, don\'t specify more than one level in any topic. This might occur legitimately where studying one subject for two different exams e.g. Scottish Higher & A-level.',
	`checkStudentsForSynchronousMeetings`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that students aren\'t members of groups with synchronous meeting-times.',
	`checkStudentsLowerWorkloadBound`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that students book sufficient courses.',
	`checkStudentsUpperWorkloadBound`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that students don\'t book excess courses.',
--	`checkSubjectExistence`						BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for the existence of the requested subjects.',
	`checkSuitableLocationsForKnowledgeRequirements`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that there are suitable locations for each knowledge-requirement of each student-body.',
	`checkTeachersAvailabilityForMandatoryMeetings`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that teachers are available for the meeting-times of the groups of which they are members.',
	`checkTeachersForSynchronousMeetings`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that teachers aren\'t members of groups with synchronous meeting-times.',
	`checkTeachersUpperWorkloadBound`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that teachers aren\'t members of too many groups.',
	`checkTeachingCapacityBySubject`				BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that there\'s sufficient teaching-capacity, to satisfy the demand by students for each subject.',
--	`checkTimeslotIdBounds`						BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check the range of timeslot-ids defined, is sufficient.',
--	`checkTimeslotRequests`						BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all timeslot-requests reference a valid timeslot-id.',
	CONSTRAINT `problemValidationSwitches.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Switches which control which checks are to be performed by the application, on the problem-parameters.';

CREATE TABLE `tbl_timetableValidationSwitches` (
	`projectId`				INT UNSIGNED	PRIMARY KEY			COMMENT 'Each project defines its own switches.',
	`checkBookedSubjectsAreRequired`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all subjects for which there\'s a booking, are required by the student-body.',
	`checkBookedSubjectsHaveService`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all subjects for which there\'s a booking, have been offered as a course, by the referenced teacher.',
	`checkSynchronisedCourses`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all the teachers of a set of synchronised courses, have booked synchronous lessons.',
	`checkExistenceOfLocationIds`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all locationIds referenced in an imported student-view timetable, exist in "tbl_locationCatalogue".',
	`checkExistenceOfStudentBodies`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all student-bodies referenced in an imported student-view timetable, exist in "tbl_studentBodyRegister".',
	`checkExistenceOfTeacherIds`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all teacherIds referenced in an imported student-view timetable, exist in "tbl_teacherRegister".',
	`checkForBookingMeetingClash`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for lessons booked @ the same time as a meeting, with a common resource-requirement.',
	`checkForMisplacedLessons`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for lessons booked @ locations lacking either adequate capacity or facilities for their course.',
	`checkForOverbookedSubjects`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for student-bodies for whom more lessons have been booked, in a specific subject, that required by the course of which they are a part.',
	`checkForOverloadedStudentBodies`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for student-bodies for whom more lessons have been booked, than allocated for tuition in one week.',
	`checkForOverloadedTeachers`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for teachers for whom more lessons have been booked, than allocated for tuition in one week.',
	`checkForResourceConflicts`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for times @ which resources have been allocated in more than one location.',
	`checkMinimumConsecutiveLessons`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check for lessons which have been booked in runlengths shorter than required by the course of which they are a part.',
	`checkResourceAvailability`		BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that resources have been booked only on days when they are available.',
	`checkTimeslotsPerDay`			BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Check that all days have the correct number of time-slots.',
	CONSTRAINT `timetableValidationSwitches.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Switches which control which checks are to be performed by the application, on any imported student-view timetable.';

CREATE TABLE `tbl_executionOptions` (
	`projectId`				INT UNSIGNED	PRIMARY KEY	COMMENT 'Each project defines its own execution-options.',
	`randomSeed`				INT		DEFAULT NULL	COMMENT 'Optionally seed the pseudo-random number-generator to obtain a deterministic sequence.',
	`permitTemporaryStudentBodyMerger`	BOOL		DEFAULT TRUE	COMMENT 'Whether to permit a temporary merger between student-bodies, to permit a teacher & location to be shared for the duration of a course.',
	`reduceStudentBodyRegister`		BOOL		DEFAULT TRUE	COMMENT 'Whether to permanently merge student-bodies with identical profiles, thus reducing the number of independently schedulable entities.',
	`removeRedundantCourses`		BOOL		DEFAULT TRUE	COMMENT 'Whether to remove any courses offered, which aren\'t required by any student.',
	`removePointlessGroups`			BOOL		DEFAULT TRUE	COMMENT 'Whether to remove groups from the group-catalogue, for which zero meetings have been scheduled',
	`removeUnsubscribedGroups`		BOOL		DEFAULT TRUE	COMMENT 'Whether to remove groups from the group-catalogue, to which neither student-bodies nor teachers have subscribed.',
	`zeroInappropriateOptions`		BOOL		DEFAULT TRUE	COMMENT 'Whether to zero the weight of each lesson-criterion & timetable-criterion, & the fecundity of each evolution-strategy, which is either inappropriate for, or irrelevant to, the specified problem-parameters.',
	CONSTRAINT `executionOptions.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Defines the parameters which tune the solution-mechanism.';

CREATE TABLE `tbl_traversalOrder` (
	`id`		INT UNSIGNED				AUTO_INCREMENT	PRIMARY KEY	COMMENT 'Unique row-identifier.',
	`projectId`	INT UNSIGNED				NOT NULL			COMMENT 'Each project defines its own traversal-order.',
	`sense`		ENUM('+','-')				NULL		DEFAULT NULL	COMMENT 'Optionally defines the direction of travel along an "axis". If unspecified then both senses will be tried',
	`axis`		ENUM('ObserverId','Day','TimeslotId')	NOT NULL			COMMENT 'Names an axis of the conceptually 3-D data-structure of the timetable.',
	CONSTRAINT `traversalOrder.unique` UNIQUE KEY (`projectId`, `axis`)			COMMENT 'Within the domain of a "projectId", the axes are distinct.',
	CONSTRAINT `traversalOrder.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Defines a raster-scan through the coordinates of a timetable, using an ordered triple of orthogonal timetable-axes.';

CREATE TABLE `tbl_style` (
	`projectId`				INT UNSIGNED									PRIMARY KEY									COMMENT 'Each project defines its own style.',
	`cssURL`				VARCHAR(128)									NOT NULL	DEFAULT 'https://functionalley.com/WeekDaze/css/weekdaze.css'	COMMENT 'Optional URL for a Cascading Style-Sheet.',
	`mergeDuplicateTimeslotsByDay`		BOOL										NOT NULL	DEFAULT TRUE							COMMENT 'Option to merge duplicate lessons between adjacent days.',
	`mergeDuplicateTimeslotsByTimeslot`	BOOL										NOT NULL	DEFAULT TRUE							COMMENT 'Option to merge duplicate lessons between adjacent time-slots.',
	`displayAxisLabelsByDay`		BOOL										NOT NULL	DEFAULT TRUE							COMMENT 'Option to display labels on the day-axis',
	`displayAxisLabelsByTimeslot`		BOOL										NOT NULL	DEFAULT TRUE							COMMENT 'Option to display labels on the time-slot axis',
	`displayRuntimeInformation`		BOOL										NOT NULL	DEFAULT TRUE							COMMENT 'Option to display both warning-messages & the runtime-log.',
	`displaySupplementaryInformation`	BOOL										NOT NULL	DEFAULT TRUE							COMMENT 'Option to augment the output with supplementary information.',
	`weekend`				SET('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')	NOT NULL	DEFAULT 'Saturday,Sunday'					COMMENT 'The days of which the weekend is composed. This doesn\'t affect the nature of the solution, only the presentation.',
	`generateLessonColourFrom`		ENUM('Lesson','Subject','Topic','Level','ResourceIds')						DEFAULT NULL							COMMENT 'Optionally specify the data-source from which to automatically generate the HTML-colour of a lesson; otherwise this will be delegated to the CSS-file specified via "cssURL"',
	`minimumContrastRatio`			DOUBLE UNSIGNED									NULL		DEFAULT NULL							COMMENT 'The minimum acceptable contrast-ratio between the foreground lesson-colour (generated according to "generateLessonColourFrom") & its complementary background-colour. A default is used when null.',

	CONSTRAINT `style.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Options which govern the style of the result generated by the application.';

CREATE TABLE `tbl_outputOptions` (
	`projectId`			INT UNSIGNED					PRIMARY KEY				COMMENT 'Each project defines its own output-options.',
	`nDecimalDigits`		TINYINT UNSIGNED				NOT NULL	DEFAULT 3		COMMENT 'The precision to which fractional auxiliary data is displayed.',
	`studentBodyMnemonicSeparator`	VARCHAR(4)							DEFAULT NULL		COMMENT 'The separator used when merging the "mnemonic"s of student-bodies.',
	`verbosity`			ENUM('Silent','Normal','Verbose','Deafening')	NOT NULL	DEFAULT 'Normal'	COMMENT 'Set the threshold for ancillary information-output.',
	CONSTRAINT `outputOptions.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Options which govern the format of the result generated by the application.';

CREATE TABLE `tbl_evolutionStrategies` (
	`projectId`						INT UNSIGNED	PRIMARY KEY			COMMENT 'Each project independently defines the parameters governing its evolution-strategies.',
	`synchronisedCourseMutationDeterministicCTor`		INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking the lessons of synchronised courses, before deterministic reconstruction.',
	`synchronisedCourseMutationRandomCTor`			INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking the lessons of synchronised courses, before random reconstruction.',
	`synchronisedCourseByDayMutationDeterministicCTor`	INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking synchronous groups of lessons for synchronised courses, before deterministic reconstruction.',
	`synchronisedCourseByDayMutationRandomCTor`		INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking synchronous groups of lessons for synchronised courses, before random reconstruction.',
	`excessRunlengthMutationDeterministicCTor`		INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking the terminal lessons of excessive runlengths, before deterministic reconstruction.',
	`excessRunlengthMutationRandomCTor`			INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking the terminal lessons of excessive runlengths, before random reconstruction.',
	`homogeneousStudentViewLessonMutationDeterministicCTor`	INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking groups of homogeneous student-view lessons, before deterministic reconstruction.',
	`homogeneousStudentViewLessonMutationRandomCTor`	INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking groups of homogeneous student-view lessons, before random reconstruction.',
	`incompleteCourseMutationDeterministicCTor`		INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking all the lessons for incomplete courses, in the timetable for each student, before deterministic reconstruction.',
	`incompleteCourseMutationRandomCTor`			INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking all the lessons for incomplete courses, in the timetable for each student, before random reconstruction.',
	`randomLessonMutationDeterministicCTor`			INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking randomly selected lessons from the timetable, before deterministic reconstruction.',
	`randomLessonMutationRandomCTor`			INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking randomly selected lessons from the timetable, before random reconstruction.',
	`singletonStudentClassMutationDeterministicCTor`	INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking lessons whose student-classes have been composed from a single student-body, before deterministic reconstruction.',
	`singletonStudentClassMutationRandomCTor`		INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking lessons whose student-classes have been composed from a single student-body, before random reconstruction.',
	`splitSessionMutationDeterministicCTor`			INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking runlengths of lessons which occur in split sessions within a day, before deterministic reconstruction.',
	`splitSessionMutationRandomCTor`			INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking runlengths of lessons which occur in split sessions within a day, before random reconstruction.',
	`studentBodyCombinationMutationDeterministicCTor`	INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking lessons for a course, whose student-classes have been composed from more than one combination of student-bodies, before deterministic reconstruction.',
	`studentBodyCombinationMutationRandomCTor`		INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking lessons for a course, whose student-classes have been composed from more than one combination of student-bodies, before random reconstruction.',
	`studentViewTimetableForDayMutationDeterministicCTor`	INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking the specified number of whole days, from each student-view timetable-for-week in isolation, before deterministically reconstructing.',
	`studentViewTimetableForDayMutationRandomCTor`		INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking the specified number of whole days, from each student-view timetable-for-week in isolation, before randomly reconstructing.',
	`studentViewTimetableForWeekMutationDeterministicCTor`	INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking a small number of randomly selected lessons, from each student-view timetable-for-week in isolation, before deterministic reconstruction.',
	`studentViewTimetableForWeekMutationRandomCTor`		INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking a small number of randomly selected lessons, from each student-view timetable-for-week in isolation, before random reconstruction.',
	`synchronousLessonMutationDeterministicCTor`		INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking groups of synchronous of heterogeneous lessons, before deterministic reconstruction.',
	`synchronousLessonMutationRandomCTor`			INT UNSIGNED	NOT NULL	DEFAULT 0	COMMENT 'Whether to attempt to evolve the timetable, by unbooking groups of synchronous of heterogeneous lessons, before random reconstruction.',
	`randomLessonMutationNTrials`				INT UNSIGNED			DEFAULT NULL	COMMENT 'The number of random trials in "randomLessonMutation". A default is used when null.',
	`randomLessonMutationNTimeslots`			TINYINT UNSIGNED		DEFAULT NULL	COMMENT 'The number of random time-slots to unbook in each trials during "randomLessonMutation". A default is used when null.',
	`studentViewTimetableForDayMutationNDays`		TINYINT UNSIGNED		DEFAULT NULL	COMMENT 'The number of days to unbook in "studentViewTimetableForDayMutation". When set to null, all valid values will be tried.',
	`studentViewTimetableForWeekMutationNTrials`		TINYINT UNSIGNED		DEFAULT NULL	COMMENT 'The number of random trials in "studentViewTimetableForWeekMutation". A default is used when null.',
	`studentViewTimetableForWeekMutationNTimeslots`		TINYINT UNSIGNED		DEFAULT NULL	COMMENT 'The number of random time-slots to unbook in each trials during "studentViewTimetableForWeekMutation". A default is used when null.',
	`fecundityDecayRatio`					DOUBLE UNSIGNED			DEFAULT NULL	COMMENT 'The factor (in the closed unit-interval) by which the fecundity of the breeding-program for future generations, is multiplied (& therefore reduced), when the population-diversity ratio falls beneath "minimumPopulationDiversityRatio". A default value is used when null.',
	`minimumPopulationDiversityRatio`			DOUBLE UNSIGNED			DEFAULT NULL	COMMENT 'The population-diversity ratio (in the closed unit-interval), beneath which a reduction in the fecundity of the breeding-program for future generations, by a factor of "fecundityDecayRatio", is triggered. A default value is used when null.',
	`nInitialScouts`					TINYINT UNSIGNED		DEFAULT NULL	COMMENT 'The initial number of candidates to select from each generation in the evolution of the timetable. A variable number is used when null.',
	CONSTRAINT `evolutionStrategies.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Defines the fecundity of each evolution-stategy, i.e. how large a candidate population to breed when using it.';

CREATE TABLE `tbl_optimiseLessonCriteriaWeights` (
	`projectId`			INT UNSIGNED	PRIMARY KEY			COMMENT 'Each project defines the optimisation of its own lesson-criteria weights.',
	`nTrials`			INT UNSIGNED	NULL				COMMENT 'The number of random trials used when attempting to optimise the lesson-criteria weights. A default value is used when null.',
	`changeMagnitude`		DOUBLE UNSIGNED	NULL				COMMENT 'The magnitude of the random change applied to the individual lesson-criteria, while attempting to find a better set. A default value is used when null.',
	`reductionFactor`		DOUBLE UNSIGNED	NULL				COMMENT 'The factor (in the closed unit-interval) used to reduce the change-magnitude after each success. A default value is used when null.',
	`useMeanOverRasterScans`	BOOL		NOT NULL	DEFAULT TRUE	COMMENT 'Whether to accept a proposed set of lesson-criteria weights on the basis of the mean (as opposed to the maximum) over the specified raster-scans, of the weighted mean over all heterogeneous timetable-criteria.',
	CONSTRAINT `optimiseLessonCriteriaWeights.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Defines the automatic optimisation of lesson-criteria weights.';

CREATE TABLE `tbl_lessonCriteriaWeights` (
	`projectId`					INT UNSIGNED	PRIMARY KEY			COMMENT 'Each project defines its own independent set of weights.',
	`areResourcesReused`				DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A lesson is preferred, if the required resources (locations & teachers) are shared with other student-bodies, by merging them into a single larger student-class.',
	`greatestMinimumConsecutiveLessons`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A lesson is preferred, if the course to which it belongs, has a greater "minimumConsecutiveLessons"; since this makes it harder to book later, into the smaller spaces remaining.',
	`greatestRemainingCourseLessons`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A course is preferred, if it has greater unbooked lessons, since a course requiring only one lesson can be booked on any day.',
	`greatestSynchronisedCourseSetSize`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A course is preferred, if it\'s a member of a larger set of synchronised courses; it seems prudent to book lessons for the most constrained course early.',
	`isCoreKnowledgeRequirement`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A subject is preferred, if it\'s categorised by the student, as a "core" knowledge-requirement.',
	`isSpecialistInTopic`				DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A teacher is preferred, if their "specialtyTopic" is the proposed "topic".',
	`matchCourseClassSizeToLocationCapacity`	DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A location is preferred, when its "capacity" matches the optional maximum class-size for the course.',
	`maximiseRelativeFacilityUtilisation`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A location is preferred, the more its facilities are used; cf. "minimiseWasteOfScarceFacilities", which tries to make a distinction based on the type of the wasted facility.',
	`maximiseStudentClassSizeOverCourseClassSize`	DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A course is preferred, the more any maximum class-size is filled by the proposed student-class.',
	`maximiseStudentClassSizeOverLocationCapacity`	DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A location is preferred, the more its "capacity" is filled by the proposed student-class.',
	`minimiseBookingAtAnotherCoursesSpecifiedTime`	DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A booking-time is preferred, if there\'s minimal probability that courses for the other knowledge-requirements of the student-body, will require that time according to specific time requests; the probability becomes a certainty for those knowledge-requirements which can only be satisfied by one course, allowing corresponding lessons to be filtered from the candidates, before this evaluation.',
	`minimiseBookingOfLocationByOtherTeachers`	DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A location is preferred, when booked by the fewest other teachers.',
	`minimiseDeviationFromTimeslotRequest`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A course is preferred, if it\'s booked closer to any timeslot-request.',
	`minimiseInterCampusMigrationsOfStudents`	DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A course is preferred, if inter-campus migrations of students are minimised.',
	`minimiseInterCampusMigrationsOfTeachers`	DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A course is preferred, if inter-campus migrations of teachers are minimised.',
	`minimiseStudentBodyCombinations`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A lesson is preferred, when the number of student-body combinations is minimised.',
	`minimiseTeachersLocusOperandi`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A location is preferred, if the size of locus-operandi for the teacher, is minimised; based merely on the number of distinct locations booked, rather than the distance between them.',
	`minimiseWasteOfScarceFacilities`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A lesson is preferred, if the unused facilities @ the location, are commonly available elsewhere.',
	CONSTRAINT `lessonCriteriaWeights.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Defines the weights, in the closed unit-interval, of all lesson-criteria. CAVEAT: the default of zero, renders the corresponding criterion irrelevant.';

CREATE TABLE `tbl_timetableCriteriaWeights` (
	`projectId`							INT UNSIGNED	PRIMARY KEY			COMMENT 'Each project defines its own independent set of weights.',
	`maximiseComplianceWithFreePeriodPreferences`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when it complies with any "freePeriodPreference", of student-bodies & teachers.',
	`maximiseMeanRatioOfStudentClassSizeToLocationCapacity`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the mean over all bookings, of the ratio of the size of the student-class to the "capacity" of the location, is greatest.',
	`maximiseMeanStudentClassSize`					DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the mean over the size of student-classes, relative to the total number of students, is greatest.',
	`maximiseSynchronisationOfSynchronisedCourses`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the lessons of synchronised courses are consistently synchronised.',
	`maximiseWeightedMeanStudentBodyUtilisationRatio`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the mean over all student-bodies, of their utilisation-ratio, is maximised. It doesn\'t distinguish between "core" & "optional" subjects like "minimiseMeanRatioOfIncompletelyBookedCoreKnowledge" & "minimiseMeanRatioOfIncompletelyBookedOptionalKnowledge", but it does value partially booked courses.',
	`minimiseAverageAbsoluteDeviationFromIdealTimeslotRequest`	DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when lessons are booked close to any "idealTimeslotRequest" for their course.',
	`minimiseDispersionOfStudentFreePeriodsPerDay`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when free periods of students are spread evenly across their working-week.',
	`minimiseDispersionOfTeacherFreePeriodsPerDay`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when free periods for the teacher are spread evenly across their working-week.',
	`minimiseDispersionOfTeacherWorkload`				DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the relative workload between all teachers, is similar.',
	`minimiseMeanInterCampusMigrationsOfStudents`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the average over all students, of the number of inter-campus migrations in a week, is minimised.',
	`minimiseMeanInterCampusMigrationsOfTeachers`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the average over all teachers, of the number of inter-campus migrations in a week, is minimised.',
	`minimiseMeanLocationChangesOfTeachers`				DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the average over all teachers, of the number of changes in their location in a week, is minimised.',
	`minimiseMeanLocusOperandiOfTeachers`				DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the average over all teachers, of the size of their locus-operandi, is minimised; based merely on the number of locations, rather than the distance or the number of changes, between them.',
	`minimiseMeanRatioOfIncompletelyBookedCoreKnowledge`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the average over all student-bodies, of the ratio of incompletely booked core knowledge-requirements, to all requirements, is minimised. All incomplete courses are valued equally, regardless of the extent.',
	`minimiseMeanRatioOfIncompletelyBookedOptionalKnowledge`	DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the average over all student-bodies, of the ratio of incompletely booked optional knowledge-requirements, to all requirements, is minimised. All incomplete courses are valued equally, regardless of the extent.',
	`minimiseMeanStudentBodyCombinationsPerLesson`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the average number of combinations of student-class per lesson-definition, is minimised; ideally a student-body always studies a subject, in a class composed from the same other student-bodies.',
	`minimiseRatioOfConsecutiveEqualLessons`			DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when blocks of consecutive identical lessons are booked, whose length most closely corresponds, to that requested for the course to which they belong.',
	`minimiseRatioOfSeparatedEqualLessonsWithinAnyDay`		DOUBLE UNSIGNED	NOT NULL	DEFAULT 0.0	COMMENT 'A timetable is preferred, when the lessons of any course, are on any day, booked in one contiguous block.',
	CONSTRAINT `timetableCriteriaWeights.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Defines the weights, in the closed unit-interval, of all timetable-criteria. CAVEAT: the default of zero, renders the corresponding criterion irrelevant.';

CREATE TABLE `tbl_locationCatalogue` (
	`locationCatalogueId`	INT UNSIGNED									AUTO_INCREMENT	PRIMARY KEY								COMMENT 'Unique row-identifier.',
	`projectId`		INT UNSIGNED									NOT NULL										COMMENT 'Each project defines its own catalogue of locations.',
	`locationId`		VARCHAR(32)									NOT NULL										COMMENT 'The name of this location. CAVEAT: should start with an alphabetic character & should not contain spaces.',
	`availability`		SET('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')	NOT NULL	DEFAULT 'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday'	COMMENT 'The location may have a periodic availability. Defaults to all days.',
	`capacity`		SMALLINT UNSIGNED								NOT NULL										COMMENT 'Quantifies the ability to cater for students; typically the number of desks.',
	`campus`		VARCHAR(32)											DEFAULT NULL								COMMENT 'Locations are grouped by mutual proximity, into zones, between which intra-day migration is non-trivial.',
	CONSTRAINT `locationCatalogue.unique` UNIQUE KEY (`projectId`, `locationId`)															COMMENT 'Within the domain of a projectId, all "locationId"s are distinct.',
	CONSTRAINT `locationCatalogue.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Defines the name, "capacity" & the days on which it\'s available for use, of each location.';

CREATE TABLE `tbl_facilityType` (
	`facilityTypeId`	INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY		COMMENT 'Unique row-identifier.',
	`projectId`		INT UNSIGNED	NOT NULL				COMMENT 'Each project defines its own set of facility-types.',
	`facilityName`		VARCHAR(32)	NOT NULL				COMMENT 'A free-format identifier for a type of facility which any location in this project, may advertise.',
	CONSTRAINT `facilityType.unique` UNIQUE KEY (`projectId`, `facilityName`)	COMMENT 'Within the domain of a "projectId", all "facilityName"s are distinct.',
	CONSTRAINT `facilityType.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='The set of facility-types which any location may advertise.';

CREATE TABLE `tbl_facility` (
	`id`			INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY			COMMENT 'Unique row-identifier.',
	`locationCatalogueId`	INT UNSIGNED	NOT NULL					COMMENT 'Each "projectId"/"locationId", defines its own independent set of facilities.',
	`facilityTypeId`	INT UNSIGNED	NOT NULL					COMMENT 'References a type of facility which this location provides.',
	CONSTRAINT `facility.unique` UNIQUE KEY (`locationCatalogueId`, `facilityTypeId`)	COMMENT 'Within the domain of a "locationCatalogueId", all "facilityTypeId"s are distinct.',
	CONSTRAINT `facility.locationCatalogueId` FOREIGN KEY (`locationCatalogueId`) REFERENCES `tbl_locationCatalogue` (`locationCatalogueId`) ON DELETE CASCADE,
	CONSTRAINT `facility.facilityTypeId` FOREIGN KEY (`facilityTypeId`) REFERENCES `tbl_facilityType` (`facilityTypeId`) ON DELETE CASCADE	-- "facility.beforeInsert" prevents one referencing a different 'projectId' from that implied by 'locationCatalogueId'.
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Advertises the set of facilities provided by each location.';

CREATE TABLE `tbl_groupCatalogue` (
	`groupCatalogueId`	INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY	COMMENT 'Unique row-identifier.',
	`projectId`		INT UNSIGNED	NOT NULL			COMMENT 'Each project defines its own catalogue of groups.',
	`groupId`		VARCHAR(32)	NOT NULL			COMMENT 'The name (typically the purpose or remit) of this group. CAVEAT: should start with an alphabetic character & should not contain spaces.',
	`locationId`		VARCHAR(32)			DEFAULT NULL	COMMENT 'Each group optionally references a location (in the same project) @ which to meet.',
	`mandatesAttendance`	BOOL				DEFAULT FALSE	COMMENT 'Whether each group-member is required to attend all meetings.',
	CONSTRAINT `groupCatalogue.unique` UNIQUE KEY (`projectId`, `groupId`)	COMMENT 'Within the domain of a "projectId", all "groupId"s are distinct.',
	INDEX `groupCatalogue(groupId)` (`groupId`)				COMMENT 'Required by foreign-key constraint "teacherGroupMembership.groupId" & "studentGroupMembership.groupId"',
	CONSTRAINT `groupCatalogue.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE,
	CONSTRAINT `groupCatalogue.(projectId,locationId)` FOREIGN KEY (`projectId`, `locationId`) REFERENCES `tbl_locationCatalogue` (`projectId`, `locationId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Defines the groups to which student-bodies & teachers can subscribe';

CREATE TABLE `tbl_meetingTime` (
	`id`			INT UNSIGNED									AUTO_INCREMENT	PRIMARY KEY	COMMENT 'Unique row-identifier.',
	`groupCatalogueId`	INT UNSIGNED									NOT NULL			COMMENT 'Each "projectId"/"groupId" defines its own times.',
	`day`			ENUM('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')	NOT NULL			COMMENT 'The day of the week @ which to meet.',
	`timeslotId`		TINYINT										NOT NULL			COMMENT 'The time-slot within the specified day, @ which to meet.',
	CONSTRAINT `meetingTime.unique` UNIQUE KEY (`groupCatalogueId`, `day`, `timeslotId`)							COMMENT 'Within the domain of a "projectId"/"groupId", all times are distinct.',
	CONSTRAINT `meetingTime.groupCatalogueId` FOREIGN KEY (`groupCatalogueId`) REFERENCES `tbl_groupCatalogue` (`groupCatalogueId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='The times @ which groups meets.';

CREATE TABLE `tbl_stream` (
	`streamId`	INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY		COMMENT 'Unique row-identifier.',
	`projectId`	INT UNSIGNED	NOT NULL				COMMENT 'Each project defines its own set of streams.',
	`streamName`	VARCHAR(32)	NOT NULL				COMMENT 'A free-format identifier for a stream to which any student-body in this project may be assigned.',
	CONSTRAINT `stream.unique` UNIQUE KEY (`projectId`, `streamName`)	COMMENT 'Within the domain of a "projectId", all "streamName"s are distinct.',
	INDEX `stream.(projectId,streamId)` (`projectId`, `streamId`)		COMMENT 'Required by foreign-key constraint "studentBodyRegister.(projectId,streamId)"',
	CONSTRAINT `stream.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='The streams to which student-bodies may be assigned.';

CREATE TABLE `tbl_studentBodyRegister` (
	`studentBodyRegisterId`	INT UNSIGNED									AUTO_INCREMENT	PRIMARY KEY								COMMENT 'Unique row-identifier.',
	`projectId`		INT UNSIGNED									NOT NULL										COMMENT 'Each project defines its own register of student-bodies.',
	`mnemonic`		VARCHAR(32)									NOT NULL										COMMENT 'The name of the student-body. CAVEAT: should start with an alphabetic character & should not contain spaces.',
	`availability`		SET('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')	NOT NULL	DEFAULT 'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday'	COMMENT 'The common working week of the member-students.',
	`streamId`		INT UNSIGNED											DEFAULT NULL								COMMENT 'Optionally references the stream to which this student-body has been assigned.',
	`teachingRatio`		DOUBLE UNSIGNED									NOT NULL	DEFAULT 1.0								COMMENT 'The ratio in the semi-closed unit-interval (0,1], of the time when each member-student is available, which is allocated to tuition.',
	`freePeriodPreference`	ENUM('Pre','Post','Terminal')									DEFAULT NULL								COMMENT 'Any mutual preference for the position within each day, of unallocated time-slots.',
	CONSTRAINT `studentBodyRegister.unique` UNIQUE KEY (`projectId`, `mnemonic`)															COMMENT 'Within the domain of a "projectId", all student-body "mnemonic"s are distinct',
	CONSTRAINT `studentBodyRegister.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE,
	CONSTRAINT `studentBodyRegister.(projectId,streamId)` FOREIGN KEY (`projectId`, `streamId`) REFERENCES `tbl_stream` (`projectId`, `streamId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Defines the "mnemonic" & the parameters of each student-body (i.e. groups of students whose requirements are identical).';

CREATE TABLE `tbl_studentBodyMembership` (
	`id`			INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY				COMMENT 'Unique row-identifier.',
	`studentBodyRegisterId`	INT UNSIGNED	NOT NULL						COMMENT 'Each "projectId"/"mnemonic" defines their own member-students.',
	`studentId`		VARCHAR(32)	NOT NULL						COMMENT 'The name of a member of this student-body. CAVEAT: should start with an alphabetic character & should not contain spaces.',
	CONSTRAINT `studentBodyMembership.unique` UNIQUE KEY (`studentBodyRegisterId`, `studentId`)	COMMENT 'Within the domain of a "projectId"/"mnemonic", all "studentId"s are distinct.',
	CONSTRAINT `studentBodyMembership.studentBodyRegisterId` FOREIGN KEY (`studentBodyRegisterId`) REFERENCES `tbl_studentBodyRegister` (`studentBodyRegisterId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Defines sets of students with identical requirements.';

CREATE TABLE `tbl_studentGroupMembership` (
	`id`			INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY				COMMENT 'Unique row-identifier.',
	`studentBodyRegisterId`	INT UNSIGNED	NOT NULL						COMMENT 'Each "projectId"/"mnemonic" defines the groups of which they are members.',
	`groupId`		VARCHAR(32)	NOT NULL						COMMENT 'The name of the group of which this student-body is a member.',
	CONSTRAINT `studentGroupMembership.unique` UNIQUE KEY (`studentBodyRegisterId`, `groupId`)	COMMENT 'Within the domain of a "projectId"/"mnemonic", all "groupId"s are distinct.',
	CONSTRAINT `studentGroupMembership.studentBodyRegisterId` FOREIGN KEY (`studentBodyRegisterId`) REFERENCES `tbl_studentBodyRegister` (`studentBodyRegisterId`) ON DELETE CASCADE,
	CONSTRAINT `studentGroupMembership.groupId` FOREIGN KEY (`groupId`) REFERENCES `tbl_groupCatalogue` (`groupId`) ON DELETE CASCADE	-- "studentGroupMembership.beforeInsert" prevents one referencing a different 'projectId' from that implied by 'studentBodyRegisterId'.
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='The groups of which student-bodies are members.';

CREATE TABLE `tbl_teacherRegister` (
	`teacherRegisterId`	INT UNSIGNED									AUTO_INCREMENT	PRIMARY KEY								COMMENT 'Unique row-identifier.',
	`projectId`		INT UNSIGNED									NOT NULL										COMMENT 'Each project defines its own register of teachers.',
	`teacherId`		VARCHAR(32)									NOT NULL										COMMENT 'The name of the teacher. CAVEAT: should start with an alphabetic character & should not contain spaces.',
	`availability`		SET('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')	NOT NULL	DEFAULT 'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday'	COMMENT 'The working week for this teacher.',
	`locationId`		VARCHAR(32)											DEFAULT NULL								COMMENT 'A teacher may have a fixed abode; e.g. a form-room.',
	`maximumTeachingRatio`	DOUBLE UNSIGNED									NOT NULL										COMMENT 'The maximum ratio of the working week for a teacher (rather than the whole week), which is devoted to teaching.',
	`freePeriodPreference`	ENUM('Pre','Post','Terminal')									DEFAULT NULL								COMMENT 'A teacher may have a preference for the position within each day, of unallocated time-slots.',
	CONSTRAINT `teacherRegister.unique` UNIQUE KEY (`projectId`, `teacherId`)															COMMENT 'Within the domain of a "projectId", all "teacherId"s are distinct.',
	CONSTRAINT `teacherRegister.projectId` FOREIGN KEY (`projectId`) REFERENCES `tbl_project` (`projectId`) ON DELETE CASCADE,
	CONSTRAINT `teacherRegister.(projectId,locationId)` FOREIGN KEY (`projectId`, `locationId`) REFERENCES `tbl_locationCatalogue` (`projectId`, `locationId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Defines the name & attributes of each teacher.';

CREATE TABLE `tbl_teacherGroupMembership` (
	`id`			INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY			COMMENT 'Unique row-identifier.',
	`teacherRegisterId`	INT UNSIGNED	NOT NULL					COMMENT 'Each "projectId"/"teacherId" defines the groups of which they are members.',
	`groupId`		VARCHAR(32)	NOT NULL					COMMENT 'The name of the group of which this teacher is a member.',
	CONSTRAINT `teacherGroupMembership.unique` UNIQUE KEY (`teacherRegisterId`, `groupId`)	COMMENT 'Within the domain of a "projectId"/"teacherId", all "groupId"s are distinct.',
	CONSTRAINT `teacherGroupMembership.teacherRegisterId` FOREIGN KEY (`teacherRegisterId`) REFERENCES `tbl_teacherRegister` (`teacherRegisterId`) ON DELETE CASCADE,
	CONSTRAINT `teacherGroupMembership.groupId` FOREIGN KEY (`groupId`) REFERENCES `tbl_groupCatalogue` (`groupId`) ON DELETE CASCADE	-- "teacherGroupMembership.beforeInsert" prevents one referencing a different 'projectId' from that implied by 'teacherRegisterId'.
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='The groups of which teachers are members.';

CREATE TABLE `tbl_service` (
	`id`				INT UNSIGNED		AUTO_INCREMENT	PRIMARY KEY	COMMENT 'Unique row-identifier.',
	`teacherRegisterId`		INT UNSIGNED		NOT NULL			COMMENT 'Each "projectId"/"teacherId" defines their own service.',
	`topic`				VARCHAR(32)		NOT NULL			COMMENT 'The topic of study.',
	`level`				VARCHAR(32)		NOT NULL			COMMENT 'The level @ which the "topic" is to be taught, typically the academic year (though it\'s actually free-format).',
	`requiredLessonsPerWeek`	TINYINT UNSIGNED	NOT NULL			COMMENT 'The subject is assumed to take a minimum time-duration to teach, & for ease of scheduling, we assume that this is divided into a constant number of lessons per week.',
	`minimumConsecutiveLessons`	TINYINT UNSIGNED	NOT NULL	DEFAULT 1	COMMENT 'Some topics may require too great a preparation-time for an isolated lesson to be practical.',
	`maximumClassSize`		SMALLINT UNSIGNED			DEFAULT NULL	COMMENT 'The optional upper bound on the number of students for whom the teacher can cater.',
	`synchronisationId`		VARCHAR(32)				DEFAULT NULL	COMMENT 'The optional identifier, shared by courses whose bookings must be synchronised.',
	`idealTimeslotRequest`		TINYINT					DEFAULT NULL	COMMENT 'The optional "timeslotId" @ which lessons in this course will ideally be booked.',
	CONSTRAINT `service.unique` UNIQUE KEY (`teacherRegisterId`, `topic`, `level`)		COMMENT 'Within the domain of a "projectId"/"teacherId" all subjects are distinct. Also required by foreign-key constraint "requiredFacility.(teacherRegisterId,topic,level)".',
	CONSTRAINT `service.teacherRegisterId` FOREIGN KEY (`teacherRegisterId`) REFERENCES `tbl_teacherRegister` (`teacherRegisterId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Defines the set of courses offered by each teacher.';

CREATE TABLE `tbl_specialtyTopic` (
	`teacherRegisterId`	INT UNSIGNED	PRIMARY KEY	COMMENT 'Each "teacherRegisterId" can optionally define a single specialty topic.',
	`topic`			VARCHAR(32)	NOT NULL	COMMENT 'The topic in which the teacher spcialises.',
	CONSTRAINT `specialtyTopic.(teacherRegisterId,topic)` FOREIGN KEY (`teacherRegisterId`, `topic`) REFERENCES `tbl_service` (`teacherRegisterId`, `topic`) ON DELETE CASCADE
) ENGINE=InnoDB	DEFAULT CHARSET=latin1	COMMENT='Identifies any single specialty topic amongst those offered in a teacher\'s service.';

-- Replacing the triple (`teacherRegisterId`, `day`, `timeslotId`) by `id`, results in 3 dependent drop-downs on web-i/f being replaced by a single long dropdown which is hard to navigate.
CREATE TABLE `tbl_specificTimeRequest` (
	`id`			INT UNSIGNED									AUTO_INCREMENT	PRIMARY KEY	COMMENT 'Unique row-identifier.',
	`teacherRegisterId`	INT UNSIGNED									NOT NULL			COMMENT 'Each "projectId"/"teacherId" defines any specific times @ which they want to book a lesson in a course.',
	`topic`			VARCHAR(32)									NOT NULL			COMMENT 'The topic of study.',
	`level`			VARCHAR(32)									NOT NULL			COMMENT 'The level of the "topic" which is being taught.',
	`day`			ENUM('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')	NOT NULL			COMMENT 'The day of the week on which to request a booking.',
	`timeslotId`		TINYINT										NOT NULL			COMMENT 'The time-slot within the specified day, @ which to request a booking.',
	CONSTRAINT `specificTimeRequest.unique` UNIQUE KEY (`teacherRegisterId`, `day`, `timeslotId`)						COMMENT 'Within the domain of a "projectId"/"teacherId", all specified times are distinct (irrespective of the subject booked).',
	CONSTRAINT `specificTimeRequest.(teacherRegisterId,topic,level)` FOREIGN KEY (`teacherRegisterId`, `topic`, `level`) REFERENCES `tbl_service` (`teacherRegisterId`, `topic`, `level`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Any specific times @ which lessons in a course must be booked.';

-- Replacing the triple (`teacherRegisterId`, `day`, `timeslotId`) by `id`, results in 3 dependent drop-downs on the web-i/f being replaced by a single long dropdown which is hard to navigate.
CREATE TABLE `tbl_requiredFacility` (
	`id`			INT UNSIGNED	AUTO_INCREMENT	PRIMARY KEY						COMMENT 'Unique row-identifier.',
	`teacherRegisterId`	INT UNSIGNED	NOT NULL								COMMENT 'Each "projectId"/"teacherId" defines the facilities required to teach a course.',
	`topic`			VARCHAR(32)	NOT NULL								COMMENT 'The topic of study.',
	`level`			VARCHAR(32)	NOT NULL								COMMENT 'The level @ which this "topic" is being taught.',
	`facilityTypeId`	INT UNSIGNED	NOT NULL								COMMENT 'References a type of facility which is required of the selected location, in order to teach this course there.',
	CONSTRAINT `requiredFacility.unique` UNIQUE KEY (`teacherRegisterId`, `topic`, `level`, `facilityTypeId`)	COMMENT 'Within the domain of a ("teacherRegisterId","topic","level"), all facility-types are distinct; i.e. all rows are different.',
	CONSTRAINT `requiredFacility.(teacherRegisterId,topic,level)` FOREIGN KEY (`teacherRegisterId`, `topic`, `level`) REFERENCES `tbl_service` (`teacherRegisterId`, `topic`, `level`) ON DELETE CASCADE,
	CONSTRAINT `requiredFacility.facilityTypeId` FOREIGN KEY (`facilityTypeId`) REFERENCES `tbl_facilityType` (`facilityTypeId`) ON DELETE CASCADE	-- "requiredFacility.beforeInsert" prevents one referencing a different 'projectId' from that implied by 'teacherRegisterId'.
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='Defines the set of facilities required to teach a subject.';

CREATE TABLE `tbl_knowledgeRequirement` (
	`id`			INT UNSIGNED		AUTO_INCREMENT	PRIMARY KEY		COMMENT 'Unique row-identifier.',
	`studentBodyRegisterId`	INT UNSIGNED		NOT NULL				COMMENT 'Each "projectId"/"mnemonic" defines their own knowledge-requirements.',
	`priority`		ENUM('core','optional')	NOT NULL	DEFAULT 'core'		COMMENT 'Whether the knowledge is required or merely desired.',
	`topic`			VARCHAR(32)		NOT NULL				COMMENT 'The topic to be studied.',
	`level`			VARCHAR(32)		NOT NULL				COMMENT 'The level @ which the "topic" is to be studied.',
	CONSTRAINT `knowledgeRequirement.unique` UNIQUE KEY (`studentBodyRegisterId`, `topic`)	COMMENT 'Within the domain of a "projectId"/"mnemonic", all "topics" are distinct (a student can\'t study a "topic" @ more than one "level").',
	CONSTRAINT `knowledgeRequirement.studentBodyRegisterId` FOREIGN KEY (`studentBodyRegisterId`) REFERENCES `tbl_studentBodyRegister` (`studentBodyRegisterId`) ON DELETE CASCADE
) ENGINE=InnoDB	AUTO_INCREMENT=1	DEFAULT CHARSET=latin1	COMMENT='The subjects required by each student.';

