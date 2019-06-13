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

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

/*
	CAVEAT: after reading the definition of a project from this database, weekdaze may export it as XML,
	so the data contained in some database-columns ("groupId", "locationId", "mnemonic", "studentId", "teacherId") which are defined in the DTD as XML "ID"-attributes,
	must start with an alphabetic character, & may not contain spaces.
*/

LOCK TABLES `tbl_user` WRITE;
INSERT INTO `tbl_user` (
	`userId`,
	`name`,
	`emailAddress`,
	`executable`,
	`mutable`,
	`created`
) VALUES (
	1,	'example',	'example@bogus.tld',	1,	0,	'2015-01-01 00:00:00'	-- The owner for the example-projects.
);
UNLOCK TABLES;

LOCK TABLES `tbl_project` WRITE;
INSERT INTO `tbl_project` (
	`userId`,
	`projectName`,
	`projectId`
) VALUES (
	1,	'example_01',	1
), (
	1,	'example_02',	2
), (
	1,	'example_03',	3
), (
	1,	'example_05',	4
), (
	1,	'example_08',	5
), (
	1,	'example_13',	6
);
UNLOCK TABLES;

LOCK TABLES `tbl_configVersion` WRITE;
INSERT INTO `tbl_configVersion` (`configVersion`) VALUES ('1.0');
UNLOCK TABLES;

LOCK TABLES `tbl_options` WRITE;
INSERT INTO `tbl_options` (
	`projectId`,
	`configVersion`
) VALUES (
	1,	'1.0'
), (
	2,	'1.0'
), (
	3,	'1.0'
), (
	4,	'1.0'
), (
	5,	'1.0'
), (
	6,	'1.0'
);
UNLOCK TABLES;

LOCK TABLES `tbl_timeslotIdBounds` WRITE;
INSERT INTO `tbl_timeslotIdBounds` (
	`projectId`,
	`max`
) VALUES (
	1,	0
), (
	2,	1
), (
	3,	2
), (
	4,	4
), (
	5,	7
), (
	6,	12
);
UNLOCK TABLES;

LOCK TABLES `tbl_problemValidationSwitches` WRITE;
INSERT INTO `tbl_problemValidationSwitches` (
	`projectId`,
	`checkIfStudentBodiesExceedTeachers`,
	`checkIfStudentBodySizeExceedsLocationCapacity`
) VALUES (
	1,	FALSE,	FALSE
), (
	2,	FALSE,	FALSE
), (
	3,	FALSE,	FALSE
), (
	4,	FALSE,	FALSE
), (
	5,	FALSE,	FALSE
), (
	6,	FALSE,	FALSE
);
UNLOCK TABLES;

LOCK TABLES `tbl_executionOptions` WRITE;
INSERT INTO `tbl_executionOptions` (
	`projectId`,
	`randomSeed`
) VALUES (
	1,	0
), (
	2,	0
), (
	3,	0
), (
	4,	0
), (
	5,	0
), (
	6,	0
);
UNLOCK TABLES;

LOCK TABLES `tbl_style` WRITE;
INSERT INTO `tbl_style` (
	`projectId`,
	`generateLessonColourFrom`,
	`minimumContrastRatio`
) VALUES (
	1,	"Topic",	0.05
), (
	2,	"Topic",	0.05
), (
	3,	NULL,		NULL
), (
	4,	NULL,		NULL
), (
	5,	NULL,		NULL
), (
	6,	NULL,		NULL
);
UNLOCK TABLES;

LOCK TABLES `tbl_outputOptions` WRITE;
INSERT INTO `tbl_outputOptions` (
	`projectId`
) VALUES (
	1
), (
	2
), (
	3
), (
	4
), (
	5
), (
	6
);
UNLOCK TABLES;

LOCK TABLES `tbl_evolutionStrategies` WRITE;
INSERT INTO `tbl_evolutionStrategies` (
	`projectId`,
	`synchronisedCourseMutationDeterministicCTor`,
	`synchronisedCourseMutationRandomCTor`,
	`synchronisedCourseByDayMutationDeterministicCTor`,
	`synchronisedCourseByDayMutationRandomCTor`,
	`excessRunlengthMutationDeterministicCTor`,
	`excessRunlengthMutationRandomCTor`,
	`homogeneousStudentViewLessonMutationDeterministicCTor`,
	`homogeneousStudentViewLessonMutationRandomCTor`,
	`incompleteCourseMutationDeterministicCTor`,
	`incompleteCourseMutationRandomCTor`,
	`randomLessonMutationDeterministicCTor`,
	`randomLessonMutationRandomCTor`,
	`singletonStudentClassMutationDeterministicCTor`,
	`singletonStudentClassMutationRandomCTor`,
	`splitSessionMutationDeterministicCTor`,
	`splitSessionMutationRandomCTor`,
	`studentBodyCombinationMutationDeterministicCTor`,
	`studentBodyCombinationMutationRandomCTor`,
	`studentViewTimetableForDayMutationDeterministicCTor`,
	`studentViewTimetableForDayMutationRandomCTor`,
	`studentViewTimetableForWeekMutationDeterministicCTor`,
	`studentViewTimetableForWeekMutationRandomCTor`,
	`synchronousLessonMutationDeterministicCTor`,
	`synchronousLessonMutationRandomCTor`,
/*
	`randomLessonMutationNTrials`,
	`randomLessonMutationNTimeslots`,
	`studentViewTimetableForDayMutationNDays`,
	`studentViewTimetableForWeekMutationNTrials`,
	`studentViewTimetableForWeekMutationNTimeslots`,
*/
	`nInitialScouts`
) VALUES (
	1,	0,	0,	0,	0,	32,	64,	32,	64,	32,	64,	32,	64,	32,	64,	0,	64,	32,	64,	32,	64,	32,	64,	32,	64,	NULL
), (
	2,	0,	0,	0,	0,	128,	256,	128,	256,	128,	256,	128,	256,	128,	256,	0,	256,	128,	256,	128,	256,	128,	256,	128,	256,	NULL
), (
	3,	0,	0,	0,	0,	128,	256,	128,	256,	128,	256,	128,	256,	128,	256,	0,	256,	128,	256,	256,	256,	128,	256,	128,	256,	2
), (
	4,	128,	256,	128,	256,	128,	256,	128,	256,	128,	256,	128,	256,	128,	256,	0,	256,	128,	256,	256,	256,	128,	256,	128,	256,	2
), (
	5,	512,	1024,	512,	1024,	512,	1024,	512,	1024,	512,	1024,	1024,	2048,	512,	1024,	0,	1024,	512,	1024,	1024,	1024,	512,	1024,	512,	1024,	2
), (
	6,	512,	1024,	512,	1024,	512,	1024,	512,	1024,	512,	1024,	2048,	4096,	512,	1024,	0,	1024,	512,	1024,	1024,	1024,	512,	1024,	512,	1024,	2
);
UNLOCK TABLES;

LOCK TABLES `tbl_lessonCriteriaWeights` WRITE;
INSERT INTO `tbl_lessonCriteriaWeights` (
	`projectId`,
	`areResourcesReused`,
	`greatestMinimumConsecutiveLessons`,
	`greatestRemainingCourseLessons`,
	`greatestSynchronisedCourseSetSize`,
	`isCoreKnowledgeRequirement`,
	`isSpecialistInTopic`,
	`matchCourseClassSizeToLocationCapacity`,
	`maximiseRelativeFacilityUtilisation`,
	`minimiseBookingAtAnotherCoursesSpecifiedTime`,
	`minimiseBookingOfLocationByOtherTeachers`,
	`minimiseDeviationFromTimeslotRequest`,
	`minimiseInterCampusMigrationsOfStudents`,
	`minimiseInterCampusMigrationsOfTeachers`,
	`minimiseStudentBodyCombinations`,
	`minimiseTeachersLocusOperandi`,
	`minimiseWasteOfScarceFacilities`
) VALUES (
	1,	0.5227,	0,	0.1588,	0,	0.0373,	0.0547,	0.072,	0.0675,	0,	0.0707,	0,	0,	0,	0.0843,	0.0503,	1
), (
	2,	0.5227,	0,	0.1588,	0,	0.0373,	0.0547,	0.072,	0.0675,	0,	0.0707,	0,	0,	0,	0.0843,	0.0503,	1
), (
	3,	0.1074,	0.0183,	0.0339,	0,	0.0136,	0.0134,	0.0293,	0.0078,	0,	0.0196,	0,	0.0625,	0.0698,	0.0354,	0.0065,	1
), (
	4,	0.0031,	0.0353,	0.0062,	0,	0.0035,	0.0028,	0.0065,	0.0049,	1,	0.0054,	0.0056,	0.035,	0.0383,	0.0701,	0.0078,	0.0383
), (
	5,	0.01,	0.0753,	0.0084,	0.2653,	0.0229,	0.0018,	0.0112,	0.0079,	0.1947,	0.0034,	0.028,	0.0682,	0.0396,	0.2577,	0.007,	1
), (
	6,	0.0048,	0.0915,	0.0141,	0.1467,	0.012,	0.0052,	0.0053,	0.0057,	1,	0.0073,	0.0187,	0.1178,	0.0632,	0.0342,	0.0059,	0.0761
);
UNLOCK TABLES;

LOCK TABLES `tbl_timetableCriteriaWeights` WRITE;
INSERT INTO `tbl_timetableCriteriaWeights` (
	`projectId`,
	`maximiseComplianceWithFreePeriodPreferences`,
	`maximiseMeanRatioOfStudentClassSizeToLocationCapacity`,
	`maximiseMeanStudentClassSize`,
	`maximiseSynchronisationOfSynchronisedCourses`,
	`maximiseWeightedMeanStudentBodyUtilisationRatio`,
	`minimiseAverageAbsoluteDeviationFromIdealTimeslotRequest`,
	`minimiseDispersionOfStudentFreePeriodsPerDay`,
	`minimiseDispersionOfTeacherFreePeriodsPerDay`,
	`minimiseDispersionOfTeacherWorkload`,
	`minimiseMeanInterCampusMigrationsOfStudents`,
	`minimiseMeanInterCampusMigrationsOfTeachers`,
	`minimiseMeanLocationChangesOfTeachers`,
	`minimiseMeanLocusOperandiOfTeachers`,
	`minimiseMeanRatioOfIncompletelyBookedCoreKnowledge`,
	`minimiseMeanRatioOfIncompletelyBookedOptionalKnowledge`,
	`minimiseMeanStudentBodyCombinationsPerLesson`,
	`minimiseRatioOfConsecutiveEqualLessons`,
	`minimiseRatioOfSeparatedEqualLessonsWithinAnyDay`
) VALUES (
	1,	0,		0.0625,		0.25,	0,	0.5,	0.125,		0.03125,	0.03125,	0.03125,	0,	0,	0.0625,	0.0625,	0.5,	0.25,	0.75,	0.125,	0
), (
	2,	0,		0.0625,		0.5,	0,	0.5,	0.125,		0.03125,	0.03125,	0.03125,	0,	0,	0.0625,	0.0625,	0.5,	0.25,	0.75,	0.125,	0
), (
	3,	0.0625,		0.0625,		0.5,	0,	0.5,	0.125,		0.03125,	0.03125,	0.03125,	0.5,	0.5,	0.0625,	0.0625,	0.5,	0.25,	0.75,	0.125,	0.25
), (
	4,	0.0625,		0.03125,	0.25,	0.5,	0.5,	0.0625,		0.015625,	0.015625,	0.03125,	0.5,	0.5,	0.0625,	0.0625,	0.5,	0.25,	0.75,	0.125,	0.25
), (
	5,	0.03125,	0.0625,		0.5,	0.5,	0.5,	0.0625,		0.03125,	0.03125,	0.03125,	0.25,	0.5,	0.0625,	0.0625,	0.5,	0.25,	0.75,	0.25,	0.25
), (
	6,	0.015625,	0.0625,		0.5,	0.5,	0.5,	0.125,		0.0078125,	0.015625,	0.03125,	0.5,	0.5,	0.0625,	0.0625,	0.5,	0.25,	0.75,	0.25,	0.25
);
UNLOCK TABLES;

LOCK TABLES `tbl_locationCatalogue` WRITE;
INSERT INTO `tbl_locationCatalogue` (
	`locationCatalogueId`,
	`projectId`,
	`locationId`,
	`availability`,
	`capacity`,
	`campus`
) VALUES (
	1,	1,	'Location_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	8,	NULL
), (
	2,	1,	'Location_2',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	8,	NULL
), (
	3,	2,	'Classroom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	4,	NULL
), (
	4,	2,	'Classroom_2',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	8,	NULL
), (
	5,	2,	'Kitchen_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	NULL
), (
	6,	2,	'MusicRoom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	4,	NULL
), (
	7,	2,	'PhysicsLab_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	NULL
), (
	8,	3,	'ChemistryLab_1',	'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	NULL
), (
	9,	3,	'Classroom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	4,	NULL
), (
	10,	3,	'Classroom_2',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	6,	NULL
), (
	11,	3,	'Gym',			'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	"Sports-complex"
), (
	12,	3,	'Kitchen_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	NULL
), (
	13,	3,	'MusicRoom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	4,	NULL
), (
	14,	3,	'PhysicsLab_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	NULL
), (
	15,	4,	'AssemblyHall',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	40,	NULL
), (
	16,	4,	'BiologyLab_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	NULL
), (
	17,	4,	'BiologyLab_2',		'Monday,Tuesday,Wednesday,Thursday,Friday',			8,	NULL
), (
	18,	4,	'ChemistryLab_1',	'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	NULL
), (
	19,	4,	'Classroom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	12,	NULL
), (
	20,	4,	'Classroom_2',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	16,	NULL
), (
	21,	4,	'Classroom_3',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	12,	NULL
), (
	22,	4,	'Classroom_4',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	16,	NULL
), (
	23,	4,	'GamesField',		'Tuesday,Wednesday',						40,	"Sports-complex"
), (
	24,	4,	'Gym',			'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	"Sports-complex"
), (
	25,	4,	'Kitchen_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	NULL
), (
	26,	4,	'Kitchen_2',		'Monday,Tuesday,Wednesday,Thursday,Friday',			8,	NULL
), (
	27,	4,	'MusicRoom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	4,	NULL
), (
	28,	4,	'MusicRoom_2',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	8,	NULL
), (
	29,	4,	'PhysicsLab_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			4,	NULL
), (
	30,	4,	'PhysicsLab_2',		'Monday,Tuesday,Wednesday,Thursday,Friday',			8,	NULL
), (
	31,	5,	'AssemblyHall',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	40,	NULL
), (
	32,	5,	'BiologyLab_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	NULL
), (
	33,	5,	'BiologyLab_2',		'Monday,Tuesday,Wednesday,Thursday,Friday',			12,	NULL
), (
	34,	5,	'ChemistryLab_1',	'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	NULL
), (
	35,	5,	'Classroom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	12,	NULL
), (
	36,	5,	'Classroom_2',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	16,	NULL
), (
	37,	5,	'Classroom_3',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	12,	NULL
), (
	38,	5,	'Classroom_4',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	16,	NULL
), (
	39,	5,	'DiningHall',		'Monday,Tuesday,Wednesday,Thursday,Friday',			40,	NULL
), (
	40,	5,	'GamesField',		'Tuesday,Wednesday',						40,	"Sports-complex"
), (
	41,	5,	'Gym',			'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	"Sports-complex"
), (
	42,	5,	'Kitchen_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	NULL
), (
	43,	5,	'Kitchen_2',		'Monday,Tuesday,Wednesday,Thursday,Friday',			12,	NULL
), (
	44,	5,	'MusicRoom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	6,	NULL
), (
	45,	5,	'MusicRoom_2',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	12,	NULL
), (
	46,	5,	'PhysicsLab_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	NULL
), (
	47,	5,	'PhysicsLab_2',		'Monday,Tuesday,Wednesday,Thursday,Friday',			12,	NULL
), (
	48,	6,	'ArtRoom',		'Monday,Tuesday,Wednesday,Thursday,Friday',			12,	NULL
), (
	49,	6,	'AssemblyHall',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	40,	NULL
), (
	50,	6,	'BiologyLab_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	NULL
), (
	51,	6,	'BiologyLab_2',		'Monday,Tuesday,Wednesday,Thursday,Friday',			12,	NULL
), (
	52,	6,	'ChemistryLab_1',	'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	NULL
), (
	53,	6,	'Classroom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	12,	NULL
), (
	54,	6,	'Classroom_2',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	16,	NULL
), (
	55,	6,	'Classroom_3',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	12,	NULL
), (
	56,	6,	'Classroom_4',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	16,	NULL
), (
	57,	6,	'DiningHall',		'Monday,Tuesday,Wednesday,Thursday,Friday',			60,	NULL
), (
	58,	6,	'GamesField',		'Sunday,Tuesday,Wednesday,Saturday',				40,	"Sports-complex"
), (
	59,	6,	'Gym',			'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	"Sports-complex"
), (
	60,	6,	'Kitchen_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	NULL
), (
	61,	6,	'Kitchen_2',		'Monday,Tuesday,Wednesday,Thursday,Friday',			12,	NULL
), (
	62,	6,	'MusicRoom_1',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	6,	NULL
), (
	63,	6,	'MusicRoom_2',		'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	12,	NULL
), (
	64,	6,	'PhysicsLab_1',		'Monday,Tuesday,Wednesday,Thursday,Friday',			6,	NULL
), (
	65,	6,	'PhysicsLab_2',		'Monday,Tuesday,Wednesday,Thursday,Friday',			12,	NULL
), (
	66,	6,	'SwimmingBaths',	'Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday',	20,	"Sports-complex"
), (
	67,	6,	'WoodworkRoom',		'Monday,Tuesday,Wednesday,Thursday,Friday',			12,	NULL
);
UNLOCK TABLES;

LOCK TABLES `tbl_facilityType` WRITE;
INSERT INTO `tbl_facilityType` (
	`facilityTypeId`,
	`projectId`,
	`facilityName`
) VALUES (
	1,	2,	'Blackboard'
), (
	2,	2,	'Desks'
), (
	3,	2,	'Kitchen-equipment'
), (
	4,	2,	'Musical instruments'
), (
	5,	2,	'Physics-lab'
), (
	6,	3,	'Blackboard'
), (
	7,	3,	'Chemistry-lab'
), (
	8,	3,	'Desks'
), (
	9,	3,	'Gym-equipment'
), (
	10,	3,	'Kitchen-equipment'
), (
	11,	3,	'Musical instruments'
), (
	12,	3,	'Physics-lab'
), (
	13,	4,	'Biology-lab'
), (
	14,	4,	'Blackboard'
), (
	15,	4,	'Chemistry-lab'
), (
	16,	4,	'Desks'
), (
	17,	4,	'Goal-posts'
), (
	18,	4,	'Gym-equipment'
), (
	19,	4,	'Kitchen-equipment'
), (
	20,	4,	'Lectern'
), (
	21,	4,	'Musical instruments'
), (
	22,	4,	'Physics-lab'
), (
	23,	4,	'Pipe-organ'
), (
	24,	4,	'Running-track'
), (
	25,	4,	'Seats'
), (
	26,	4,	'Showers'
), (
	27,	5,	'Biology-lab'
), (
	28,	5,	'Blackboard'
), (
	29,	5,	'Chemistry-lab'
), (
	30,	5,	'Desks'
), (
	31,	5,	'Goal-posts'
), (
	32,	5,	'Gym-equipment'
), (
	33,	5,	'Kitchen-equipment'
), (
	34,	5,	'Lectern'
), (
	35,	5,	'Musical instruments'
), (
	36,	5,	'Pipe-organ'
), (
	37,	5,	'Physics-lab'
), (
	38,	5,	'Running-track'
), (
	39,	5,	'Seats'
), (
	40,	5,	'Showers'
), (
	41,	5,	'Tables'
), (
	42,	6,	'Art-tools'
), (
	43,	6,	'Biology-lab'
), (
	44,	6,	'Blackboard'
), (
	45,	6,	'Chemistry-lab'
), (
	46,	6,	'Desks'
), (
	47,	6,	'Drawing-desks'
), (
	48,	6,	'Easels'
), (
	49,	6,	'Goal-posts'
), (
	50,	6,	'Gym-equipment'
), (
	51,	6,	'Kitchen-equipment'
), (
	52,	6,	'Lectern'
), (
	53,	6,	'Musical instruments'
), (
	54,	6,	'Physics-lab'
), (
	55,	6,	'Pipe-organ'
), (
	56,	6,	'Running-track'
), (
	57,	6,	'Seats'
), (
	58,	6,	'Showers'
), (
	59,	6,	'Sinks'
), (
	60,	6,	'Swimming-pool'
), (
	61,	6,	'Tables'
), (
	62,	6,	'Wood-tools'
), (
	63,	6,	'Work-benches'
);
UNLOCK TABLES;

LOCK TABLES `tbl_facility` WRITE;
INSERT INTO `tbl_facility` (
	`locationCatalogueId`,
	`facilityTypeId`
) VALUES (
	3,	1	-- 'Blackboard'
), (
	3,	2	-- 'Desks'
), (
	4,	1	-- 'Blackboard'
), (
	4,	2	-- 'Desks'
), (
	5,	3	-- 'Kitchen-equipment'
), (
	6,	4	-- 'Musical instruments'
), (
	7,	1	-- 'Blackboard'
), (
	7,	2	-- 'Desks'
), (
	7,	5	-- 'Physics-lab'
), (
	8,	6	-- 'Blackboard'
), (
	8,	7	-- 'Chemistry-lab'
), (
	8,	8	-- 'Desks'
), (
	9,	6	-- 'Blackboard'
), (
	9,	8	-- 'Desks'
), (
	10,	6	-- 'Blackboard'
), (
	10,	8	-- 'Desks'
), (
	11,	9	-- 'Gym-equipment'
), (
	12,	10	-- 'Kitchen-equipment'
), (
	13,	11	-- 'Musical instruments'
), (
	14,	6	-- 'Blackboard'
), (
	14,	8	-- 'Desks'
), (
	14,	12	-- 'Physics-lab'
), (
	15,	20	-- 'Lectern'
), (
	15,	23	-- 'Pipe-organ'
), (
	15,	25	-- 'Seats'
), (
	16,	13	-- 'Biology-lab'
), (
	16,	14	-- 'Blackboard'
), (
	16,	16	-- 'Desks'
), (
	17,	13	-- 'Biology-lab'
), (
	17,	14	-- 'Blackboard'
), (
	17,	16	-- 'Desks'
), (
	18,	14	-- 'Blackboard'
), (
	18,	15	-- 'Chemistry-lab'
), (
	18,	16	-- 'Desks'
), (
	19,	14	-- 'Blackboard'
), (
	19,	16	-- 'Desks'
), (
	20,	14	-- 'Blackboard'
), (
	20,	16	-- 'Desks'
), (
	21,	14	-- 'Blackboard'
), (
	21,	16	-- 'Desks'
), (
	22,	14	-- 'Blackboard'
), (
	22,	16	-- 'Desks'
), (
	23,	17	-- 'Goal-posts'
), (
	23,	24	-- 'Running-track'
), (
	23,	26	-- 'Showers'
), (
	24,	18	-- 'Gym-equipment'
), (
	25,	19	-- 'Kitchen-equipment'
), (
	26,	19	-- 'Kitchen-equipment'
), (
	27,	21	-- 'Musical instruments'
), (
	28,	14	-- 'Blackboard'
), (
	28,	21	-- 'Musical instruments'
), (
	29,	14	-- 'Blackboard'
), (
	29,	16	-- 'Desks'
), (
	29,	22	-- 'Physics-lab'
), (
	30,	14	-- 'Blackboard'
), (
	30,	16	-- 'Desks'
), (
	30,	22	-- 'Physics-lab'
), (
	31,	34	-- 'Lectern'
), (
	31,	36	-- 'Pipe-organ'
), (
	31,	39	-- 'Seats'
), (
	32,	27	-- 'Biology-lab'
), (
	32,	28	-- 'Blackboard'
), (
	32,	30	-- 'Desks'
), (
	33,	27	-- 'Biology-lab'
), (
	33,	28	-- 'Blackboard'
), (
	33,	30	-- 'Desks'
), (
	34,	28	-- 'Blackboard'
), (
	34,	29	-- 'Chemistry-lab'
), (
	34,	30	-- 'Desks'
), (
	35,	28	-- 'Blackboard'
), (
	35,	30	-- 'Desks'
), (
	36,	28	-- 'Blackboard'
), (
	36,	30	-- 'Desks'
), (
	37,	28	-- 'Blackboard'
), (
	37,	30	-- 'Desks'
), (
	38,	28	-- 'Blackboard'
), (
	38,	30	-- 'Desks'
), (
	39,	39	-- 'Seats'
), (
	39,	41	-- 'Tables'
), (
	40,	31	-- 'Goal-posts'
), (
	40,	38	-- 'Running-track'
), (
	40,	40	-- 'Showers'
), (
	41,	32	-- 'Gym-equipment'
), (
	42,	33	-- 'Kitchen-equipment'
), (
	43,	33	-- 'Kitchen-equipment'
), (
	44,	35	-- 'Musical instruments'
), (
	45,	28	-- 'Blackboard'
), (
	45,	35	-- 'Musical instruments'
), (
	46,	28	-- 'Blackboard'
), (
	46,	30	-- 'Desks'
), (
	46,	37	-- 'Physics-lab'
), (
	47,	28	-- 'Blackboard'
), (
	47,	30	-- 'Desks'
), (
	47,	37	-- 'Physics-lab'
), (
	48,	42	-- 'Art-tools'
), (
	48,	47	-- 'Drawing-desks'
), (
	48,	48	-- 'Easels'
), (
	48,	59	-- 'Sinks'
), (
	49,	52	-- 'Lectern'
), (
	49,	55	-- 'Pipe-organ'
), (
	49,	57	-- 'Seats'
), (
	50,	43	-- 'Biology-lab'
), (
	50,	44	-- 'Blackboard'
), (
	50,	46	-- 'Desks'
), (
	51,	43	-- 'Biology-lab'
), (
	51,	44	-- 'Blackboard'
), (
	51,	46	-- 'Desks'
), (
	52,	44	-- 'Blackboard'
), (
	52,	45	-- 'Chemistry-lab'
), (
	52,	46	-- 'Desks'
), (
	53,	44	-- 'Blackboard'
), (
	53,	46	-- 'Desks'
), (
	54,	44	-- 'Blackboard'
), (
	54,	46	-- 'Desks'
), (
	55,	44	-- 'Blackboard'
), (
	55,	46	-- 'Desks'
), (
	56,	44	-- 'Blackboard'
), (
	56,	46	-- 'Desks'
), (
	57,	57	-- 'Seats'
), (
	57,	61	-- 'Tables'
), (
	58,	49	-- 'Goal-posts'
), (
	58,	56	-- 'Running-track'
), (
	58,	58	-- 'Showers'
), (
	59,	50	-- 'Gym-equipment'
), (
	60,	51	-- 'Kitchen-equipment'
), (
	61,	51	-- 'Kitchen-equipment'
), (
	62,	53	-- 'Musical instruments'
), (
	63,	44	-- 'Blackboard'
), (
	63,	53	-- 'Musical instruments'
), (
	64,	44	-- 'Blackboard'
), (
	64,	46	-- 'Desks'
), (
	64,	54	-- 'Physics-lab'
), (
	65,	44	-- 'Blackboard'
), (
	65,	46	-- 'Desks'
), (
	65,	54	-- 'Physics-lab'
), (
	66,	60	-- 'Swimming-pool'
), (
	67,	62	-- 'Wood-tools'
), (
	67,	63	-- 'Work-benches'
);
UNLOCK TABLES;

LOCK TABLES `tbl_groupCatalogue` WRITE;
INSERT INTO `tbl_groupCatalogue` (
	`groupCatalogueId`,
	`projectId`,
	`groupId`,
	`locationId`,
	`mandatesAttendance`
) VALUES (
	1,	4,	'Administrators',	NULL,		TRUE
), (
	2,	4,	'Assembly',		'AssemblyHall',	FALSE
), (
	3,	4,	'Choir',		'MusicRoom_1',	TRUE
), (
	4,	4,	'Christians',		NULL,		TRUE
), (
	5,	4,	'Drama',		'MusicRoom_2',	TRUE
), (
	6,	4,	'Musicians',		NULL,		TRUE
), (
	7,	4,	'Science-teachers',	NULL,		TRUE
), (
	8,	4,	'Sport-teachers',	NULL,		TRUE
), (
	9,	5,	'Administrators',	NULL,		TRUE
), (
	10,	5,	'Assembly',		'AssemblyHall',	FALSE
), (
	11,	5,	'Choir',		'MusicRoom_1',	TRUE
), (
	12,	5,	'Drama',		'MusicRoom_2',	TRUE
), (
	13,	5,	'Lunch',		'DiningHall',	FALSE
), (
	14,	5,	'Musicians',		NULL,		TRUE
), (
	15,	5,	'Science-teachers',	NULL,		TRUE
), (
	16,	5,	'Sport-teachers',	NULL,		TRUE
), (
	17,	6,	'Administrators',	NULL,		TRUE
), (
	18,	6,	'Assembly',		'AssemblyHall',	FALSE
), (
	19,	6,	'Chess',		'Classroom_1',	FALSE
), (
	20,	6,	'Choir',		'MusicRoom_1',	TRUE
), (
	21,	6,	'Debating',		'Classroom_2',	TRUE
), (
	22,	6,	'Drama',		'MusicRoom_2',	TRUE
), (
	23,	6,	'Lunch_A',		'DiningHall',	FALSE
), (
	24,	6,	'Lunch_B',		'DiningHall',	FALSE
), (
	25,	6,	'Science-teachers',	NULL,		TRUE
), (
	26,	6,	'Sport-teachers',	NULL,		TRUE
);
UNLOCK TABLES;

LOCK TABLES `tbl_meetingTime` WRITE;
INSERT INTO `tbl_meetingTime` (
	`groupCatalogueId`,
	`day`,
	`timeslotId`
) VALUES (
	1,	'Tuesday',	0
), (
	2,	'Monday',	0
), (
	2,	'Tuesday',	0
), (
	2,	'Wednesday',	0
), (
	2,	'Thursday',	0
), (
	2,	'Friday',	0
), (
	3,	'Friday',	4
), (
	5,	'Friday',	4
), (
	7,	'Wednesday',	0
), (
	9,	'Tuesday',	0
), (
	10,	'Monday',	0
), (
	10,	'Tuesday',	0
), (
	10,	'Wednesday',	0
), (
	10,	'Thursday',	0
), (
	10,	'Friday',	0
), (
	11,	'Friday',	7
), (
	12,	'Friday',	7
), (
	13,	'Monday',	4
), (
	13,	'Tuesday',	4
), (
	13,	'Wednesday',	4
), (
	13,	'Thursday',	4
), (
	13,	'Friday',	4
), (
	15,	'Wednesday',	0
), (
	17,	'Tuesday',	0
), (
	18,	'Monday',	0
), (
	18,	'Tuesday',	0
), (
	18,	'Wednesday',	0
), (
	18,	'Thursday',	0
), (
	18,	'Friday',	0
), (
	19,	'Thursday',	12
), (
	20,	'Friday',	12
), (
	21,	'Thursday',	12
), (
	22,	'Friday',	12
), (
	23,	'Monday',	6
), (
	23,	'Tuesday',	6
), (
	23,	'Wednesday',	6
), (
	23,	'Thursday',	6
), (
	23,	'Friday',	6
), (
	24,	'Monday',	7
), (
	24,	'Tuesday',	7
), (
	24,	'Wednesday',	7
), (
	24,	'Thursday',	7
), (
	24,	'Friday',	7
), (
	25,	'Wednesday',	0
);
UNLOCK TABLES;

LOCK TABLES `tbl_studentBodyRegister` WRITE;
INSERT INTO `tbl_studentBodyRegister` (
	`studentBodyRegisterId`,
	`projectId`,
	`mnemonic`,
	`availability`,
	`teachingRatio`,
	`freePeriodPreference`
) VALUES (
	1,	1,	'Body_1',	'Monday,Tuesday,Wednesday,Thursday,Friday',	1,	NULL
), (
	2,	1,	'Body_2',	'Monday,Tuesday,Wednesday,Thursday,Friday',	1,	NULL
), (
	3,	1,	'Body_3',	'Monday,Tuesday,Wednesday,Thursday,Friday',	1,	NULL
), (
	4,	2,	'Body_01',	'Monday,Tuesday,Wednesday,Thursday,Friday',	1,	NULL
), (
	5,	2,	'Body_02',	'Monday,Tuesday,Wednesday,Thursday,Friday',	1,	NULL
), (
	6,	3,	'Body_01',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.8,	NULL
), (
	7,	3,	'Body_02',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.8,	NULL
), (
	8,	4,	'Body_01',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.72,	NULL
), (
	9,	4,	'Body_02',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.72,	NULL
), (
	10,	4,	'Body_03',	'Tuesday,Wednesday,Thursday,Friday',		0.75,	NULL
), (
	11,	4,	'Body_04',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.72,	NULL
), (
	12,	4,	'Body_05',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.68,	NULL
), (
	13,	4,	'Body_06',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.72,	NULL
), (
	14,	4,	'Body_07',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.72,	NULL
), (
	15,	4,	'Body_08',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.72,	NULL
), (
	16,	4,	'Body_09',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.72,	NULL
), (
	17,	4,	'Body_10',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.8,	NULL
), (
	18,	4,	'Body_11',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.68,	NULL
), (
	19,	5,	'Body_01',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	20,	5,	'Body_02',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	21,	5,	'Body_03',	'Tuesday,Wednesday,Thursday,Friday',		0.625,	NULL
), (
	22,	5,	'Body_04',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	23,	5,	'Body_05',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	24,	5,	'Body_06',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	25,	5,	'Body_07',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	26,	5,	'Body_08',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	27,	5,	'Body_09',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	28,	5,	'Body_10',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	29,	5,	'Body_11',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.625,	NULL
), (
	30,	6,	'Body_1.0',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
), (
	31,	6,	'Body_1.1',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
), (
	32,	6,	'Body_1.2',	'Tuesday,Wednesday,Thursday,Friday',		0.693,	'Post'
), (
	33,	6,	'Body_1.3',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
), (
	34,	6,	'Body_2.0',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
), (
	35,	6,	'Body_2.1',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
), (
	36,	6,	'Body_2.2',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
), (
	37,	6,	'Body_2.3',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
), (
	38,	6,	'Body_3.0',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
), (
	39,	6,	'Body_3.1',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
), (
	40,	6,	'Body_3.2',	'Monday,Tuesday,Wednesday,Thursday,Friday',	0.693,	NULL
);
UNLOCK TABLES;

LOCK TABLES `tbl_studentBodyMembership` WRITE;
INSERT INTO `tbl_studentBodyMembership` (
	`studentBodyRegisterId`,
	`studentId`
) VALUES (
	1,	'Student_1.0'
), (
	1,	'Student_1.1'
), (
	1,	'Student_1.2'
), (
	2,	'Student_2.0'
), (
	2,	'Student_2.1'
), (
	3,	'Student_3.0'
), (
	4,	'BallsHarry'
), (
	4,	'BoardBill'
), (
	4,	'BullTerry'
), (
	5,	'BodyAnnie'
), (
	5,	'BoyleLance'
), (
	5,	'SpencerMarkN'
), (
	6,	'BallsHarry'
), (
	6,	'BoardBill'
), (
	6,	'BullTerry'
), (
	7,	'BodyAnnie'
), (
	7,	'BoyleLance'
), (
	7,	'SpencerMarkN'
), (
	8,	'BallsHarry'
), (
	8,	'BoardBill'
), (
	8,	'BullTerry'
), (
	9,	'BodyAnnie'
), (
	9,	'BoyleLance'
), (
	9,	'SpencerMarkN'
), (
	10,	'BushRose'
), (
	10,	'PondLilly'
), (
	11,	'DaySonny'
), (
	12,	'EsterPolly'
), (
	13,	'ForestWoody'
), (
	13,	'HareLotta'
), (
	13,	'LearSandy'
), (
	14,	'GunnTommy'
), (
	14,	'PostMark'
), (
	15,	'LottMona'
), (
	16,	'PipeDwain'
), (
	17,	'PoleLou'
), (
	17,	'PottJack'
), (
	18,	'ShawRick'
), (
	19,	'Student_01.0'
), (
	19,	'Student_01.1'
), (
	19,	'Student_01.2'
), (
	20,	'Student_02.0'
), (
	20,	'Student_02.1'
), (
	20,	'Student_02.2'
), (
	21,	'Student_03.0'
), (
	21,	'Student_03.1'
), (
	22,	'Student_04.0'
), (
	23,	'Student_05.0'
), (
	24,	'Student_06.0'
), (
	24,	'Student_06.1'
), (
	24,	'Student_06.2'
), (
	25,	'Student_07.0'
), (
	25,	'Student_07.1'
), (
	26,	'Student_08.0'
), (
	27,	'Student_09.0'
), (
	28,	'Student_10.0'
), (
	28,	'Student_10.1'
), (
	29,	'Student_11.0'
), (
	30,	'Student_1.0.0'
), (
	30,	'Student_1.0.1'
), (
	30,	'Student_1.0.2'
), (
	31,	'Student_1.1.0'
), (
	31,	'Student_1.1.1'
), (
	31,	'Student_1.1.2'
), (
	32,	'Student_1.2.0'
), (
	32,	'Student_1.2.1'
), (
	33,	'Student_1.3.0'
), (
	34,	'Student_2.0.0'
), (
	35,	'Student_2.1.0'
), (
	35,	'Student_2.1.1'
), (
	35,	'Student_2.1.2'
), (
	36,	'Student_2.2.0'
), (
	36,	'Student_2.2.1'
), (
	37,	'Student_2.3.0'
), (
	38,	'Student_3.0.0'
), (
	39,	'Student_3.1.0'
), (
	39,	'Student_3.1.1'
), (
	40,	'Student_3.2.0'
);
UNLOCK TABLES;

LOCK TABLES `tbl_studentGroupMembership` WRITE;
INSERT INTO `tbl_studentGroupMembership` (
	`studentBodyRegisterId`,
	`groupId`
) VALUES (
	8,	'Assembly'
), (
	9,	'Assembly'
), (
	10,	'Assembly'
), (
	10,	'Choir'
), (
	11,	'Assembly'
), (
	11,	'Drama'
), (
	12,	'Assembly'
), (
	12,	'Drama'
), (
	13,	'Assembly'
), (
	14,	'Assembly'
), (
	15,	'Assembly'
), (
	15,	'Drama'
), (
	16,	'Assembly'
), (
	17,	'Assembly'
), (
	18,	'Assembly'
), (
	18,	'Choir'
), (
	19,	'Assembly'
), (
	19,	'Lunch'
), (
	20,	'Assembly'
), (
	20,	'Lunch'
), (
	21,	'Assembly'
), (
	21,	'Choir'
), (
	21,	'Lunch'
), (
	22,	'Assembly'
), (
	22,	'Drama'
), (
	22,	'Lunch'
), (
	23,	'Assembly'
), (
	23,	'Drama'
), (
	23,	'Lunch'
), (
	24,	'Assembly'
), (
	24,	'Lunch'
), (
	25,	'Assembly'
), (
	25,	'Lunch'
), (
	26,	'Assembly'
), (
	26,	'Drama'
), (
	26,	'Lunch'
), (
	27,	'Assembly'
), (
	27,	'Lunch'
), (
	28,	'Assembly'
), (
	28,	'Lunch'
), (
	29,	'Assembly'
), (
	29,	'Choir'
), (
	29,	'Lunch'
), (
	30,	'Assembly'
), (
	30,	'Debating'
), (
	30,	'Lunch_A'
), (
	31,	'Assembly'
), (
	31,	'Chess'
), (
	31,	'Lunch_B'
), (
	32,	'Assembly'
), (
	32,	'Choir'
), (
	32,	'Lunch_A'
), (
	33,	'Assembly'
), (
	33,	'Drama'
), (
	33,	'Lunch_B'
), (
	34,	'Assembly'
), (
	34,	'Debating'
), (
	34,	'Lunch_A'
), (
	35,	'Assembly'
), (
	35,	'Chess'
), (
	35,	'Lunch_B'
), (
	36,	'Assembly'
), (
	36,	'Choir'
), (
	36,	'Lunch_A'
), (
	37,	'Assembly'
), (
	37,	'Drama'
), (
	37,	'Lunch_B'
), (
	38,	'Assembly'
), (
	38,	'Chess'
), (
	38,	'Lunch_A'
), (
	39,	'Assembly'
), (
	39,	'Debating'
), (
	39,	'Lunch_B'
), (
	40,	'Assembly'
), (
	40,	'Choir'
), (
	40,	'Lunch_A'
);
UNLOCK TABLES;

LOCK TABLES `tbl_teacherRegister` WRITE;
INSERT INTO `tbl_teacherRegister` (
	`teacherRegisterId`,
	`projectId`,
	`teacherId`,
	`availability`,
	`locationId`,
	`maximumTeachingRatio`,
	`freePeriodPreference`
) VALUES (
	1,	1,	'Teacher_1',		'Tuesday,Wednesday,Thursday',			NULL,			1,	NULL
), (
	2,	1,	'Teacher_2',		'Wednesday,Thursday,Friday',			NULL,			1,	NULL
), (
	3,	1,	'Teacher_3',		'Monday,Thursday,Friday',			NULL,			1,	NULL
), (
	4,	1,	'Teacher_4',		'Monday,Tuesday,Friday',			NULL,			1,	NULL
), (
	5,	2,	'AtrickJerry',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0,	NULL
), (
	6,	2,	'BannJoe',		'Monday,Tuesday,Wednesday',			'MusicRoom_1',		0.83,	NULL
), (
	7,	2,	'EasterleyGale',	'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.9,	NULL
), (
	8,	2,	'InnoveraMarge',	'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.9,	NULL
), (
	9,	2,	'MasseyHugh',		'Tuesday,Wednesday,Thursday,Friday',		NULL,			0.875,	NULL
), (
	10,	2,	'NuttHazel',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.9,	NULL
), (
	11,	2,	'TheronLou',		'Tuesday,Friday',				NULL,			0.75,	NULL
), (
	12,	3,	'AtrickJerry',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0,	NULL
), (
	13,	3,	'EasterleyGale',	'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.93,	NULL
), (
	14,	3,	'InnoveraMarge',	'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.93,	NULL
), (
	15,	3,	'IronsRusty',		'Monday,Tuesday,Wednesday,Thursday,Friday',	'ChemistryLab_1',	0.93,	NULL
), (
	16,	3,	'MasseyHugh',		'Tuesday,Wednesday,Thursday,Friday',		NULL,			0.92,	NULL
), (
	17,	3,	'NuttHazel',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.93,	NULL
), (
	18,	3,	'ShoeiJim',		'Monday,Tuesday,Wednesday,Friday,Saturday',	NULL,			0.93,	NULL
), (
	19,	3,	'TheronLou',		'Tuesday,Friday',				NULL,			0.83,	NULL
), (
	20,	3,	'WallCathR',		'Monday,Tuesday,Wednesday',			'MusicRoom_1',		0.89,	NULL
), (
	21,	4,	'AnderCorrie',		'Monday,Wednesday,Thursday,Friday',		NULL,			0.9,	NULL
), (
	22,	4,	'AtrickJerry',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0,	NULL
), (
	23,	4,	'BannJoe',		'Monday,Tuesday,Wednesday',			'MusicRoom_1',		0.8,	NULL
), (
	24,	4,	'BaptistAnna',		'Tuesday,Thursday',				NULL,			0.9,	NULL
), (
	25,	4,	'EasterleyGale',	'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.92,	NULL
), (
	26,	4,	'InnoveraMarge',	'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.96,	NULL
), (
	27,	4,	'IronsRusty',		'Monday,Tuesday,Wednesday,Thursday,Friday',	'ChemistryLab_1',	0.92,	NULL
), (
	28,	4,	'LynMandy',		'Tuesday,Thursday,Friday',			'MusicRoom_2',		0.7,	NULL
), (
	29,	4,	'LyonDanD',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.92,	NULL
), (
	30,	4,	'MasseyHugh',		'Tuesday,Wednesday,Thursday,Friday',		NULL,			0.9,	NULL
), (
	31,	4,	'NuttHazel',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.92,	NULL
), (
	32,	4,	'PaineJock',		'Tuesday,Wednesday,Thursday,Saturday',		NULL,			0.9,	NULL
), (
	33,	4,	'PoleDi',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.92,	NULL
), (
	34,	4,	'PooleJean',		'Monday,Tuesday,Wednesday,Friday',		'BiologyLab_2',		0.9,	NULL
), (
	35,	4,	'ShoeiJim',		'Monday,Tuesday,Wednesday,Friday,Saturday',	NULL,			0.92,	NULL
), (
	36,	4,	'SteinFrankN',		'Monday,Wednesday,Thursday,Friday',		'BiologyLab_1',		0.9,	NULL
), (
	37,	4,	'TheronLou',		'Tuesday,Friday',				NULL,			0.8,	NULL
), (
	38,	5,	'Teacher_01',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0,	NULL
), (
	39,	5,	'Teacher_02',		'Monday,Tuesday,Wednesday',			'MusicRoom_1',		0.625,	NULL
), (
	40,	5,	'Teacher_03',		'Tuesday,Thursday',				NULL,			0.625,	NULL
), (
	41,	5,	'Teacher_04',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.625,	NULL
), (
	42,	5,	'Teacher_05',		'Monday,Wednesday,Thursday,Friday',		NULL,			0.625,	NULL
), (
	43,	5,	'Teacher_06',		'Monday,Wednesday,Thursday,Friday',		'BiologyLab_1',		0.625,	NULL
), (
	44,	5,	'Teacher_07',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.625,	NULL
), (
	45,	5,	'Teacher_08',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.625,	NULL
), (
	46,	5,	'Teacher_09',		'Monday,Tuesday,Wednesday,Thursday,Friday',	'ChemistryLab_1',	0.625,	NULL
), (
	47,	5,	'Teacher_10',		'Tuesday,Wednesday,Thursday,Saturday',		NULL,			0.625,	NULL
), (
	48,	5,	'Teacher_11',		'Tuesday,Wednesday,Thursday,Friday',		NULL,			0.625,	NULL
), (
	49,	5,	'Teacher_12',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.625,	NULL
), (
	50,	5,	'Teacher_13',		'Monday,Tuesday,Wednesday,Friday',		'BiologyLab_2',		0.625,	NULL
), (
	51,	5,	'Teacher_14',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.625,	NULL
), (
	52,	5,	'Teacher_15',		'Monday,Tuesday,Wednesday,Friday,Saturday',	NULL,			0.625,	NULL
), (
	53,	5,	'Teacher_16',		'Tuesday,Thursday,Friday',			'MusicRoom_2',		0.625,	NULL
), (
	54,	5,	'Teacher_17',		'Tuesday,Friday',				NULL,			0.625,	NULL
), (
	55,	5,	'Teacher_18',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.625,	NULL
), (
	56,	6,	'Teacher_01',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0,	NULL
), (
	57,	6,	'Teacher_02',		'Monday,Tuesday,Wednesday',			'MusicRoom_1',		0.616,	NULL
), (
	58,	6,	'Teacher_03',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	59,	6,	'Teacher_04',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	'Post'
), (
	60,	6,	'Teacher_05',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	61,	6,	'Teacher_06',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	62,	6,	'Teacher_07',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	63,	6,	'Teacher_08',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	64,	6,	'Teacher_09',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	65,	6,	'Teacher_10',		'Monday,Tuesday,Wednesday,Thursday,Friday',	'ChemistryLab_1',	0.616,	NULL
), (
	66,	6,	'Teacher_11',		'Tuesday,Wednesday,Thursday,Saturday',		NULL,			0.616,	NULL
), (
	67,	6,	'Teacher_12',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	68,	6,	'Teacher_13',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	69,	6,	'Teacher_14',		'Monday,Tuesday,Wednesday,Friday',		'BiologyLab_2',		0.616,	NULL
), (
	70,	6,	'Teacher_15',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	'Terminal'
), (
	71,	6,	'Teacher_16',		'Monday,Tuesday,Wednesday,Friday,Saturday',	NULL,			0.616,	NULL
), (
	72,	6,	'Teacher_17',		'Thursday,Friday',				'MusicRoom_2',		0.616,	NULL
), (
	73,	6,	'Teacher_18',		'Tuesday,Friday',				NULL,			0.616,	NULL
), (
	74,	6,	'Teacher_19',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	75,	6,	'Teacher_20',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	76,	6,	'Teacher_21',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	77,	6,	'Teacher_22',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
), (
	78,	6,	'Teacher_23',		'Monday,Tuesday,Wednesday,Thursday,Friday',	NULL,			0.616,	NULL
);
UNLOCK TABLES;

LOCK TABLES `tbl_teacherGroupMembership` WRITE;
INSERT INTO `tbl_teacherGroupMembership` (
	`teacherRegisterId`,
	`groupId`
) VALUES (
	22,	'Assembly'
), (
	23,	'Assembly'
), (
	23,	'Musicians'
), (
	24,	'Administrators'
), (
	25,	'Drama'
), (
	26,	'Drama'
), (
	27,	'Science-teachers'
), (
	28,	'Assembly'
), (
	28,	'Choir'
), (
	28,	'Musicians'
), (
	30,	'Science-teachers'
), (
	32,	'Sport-teachers'
), (
	33,	'Science-teachers'
), (
	34,	'Science-teachers'
), (
	35,	'Sport-teachers'
), (
	36,	'Science-teachers'
), (
	37,	'Administrators'
), (
	38,	'Assembly'
), (
	38,	'Lunch'
), (
	39,	'Assembly'
), (
	39,	'Lunch'
), (
	39,	'Musicians'
), (
	40,	'Administrators'
), (
	40,	'Lunch'
), (
	41,	'Drama'
), (
	41,	'Lunch'
), (
	42,	'Lunch'
), (
	43,	'Lunch'
), (
	43,	'Science-teachers'
), (
	44,	'Drama'
), (
	44,	'Lunch'
), (
	45,	'Lunch'
), (
	46,	'Lunch'
), (
	46,	'Science-teachers'
), (
	47,	'Lunch'
), (
	47,	'Sport-teachers'
), (
	48,	'Lunch'
), (
	48,	'Science-teachers'
), (
	49,	'Lunch'
), (
	49,	'Science-teachers'
), (
	50,	'Lunch'
), (
	50,	'Science-teachers'
), (
	51,	'Lunch'
), (
	52,	'Lunch'
), (
	52,	'Sport-teachers'
), (
	53,	'Assembly'
), (
	53,	'Choir'
), (
	53,	'Lunch'
), (
	53,	'Musicians'
), (
	54,	'Administrators'
), (
	54,	'Lunch'
), (
	55,	'Lunch'
), (
	56,	'Assembly'
), (
	56,	'Lunch_B'
), (
	57,	'Assembly'
), (
	57,	'Lunch_A'
), (
	58,	'Lunch_B'
), (
	59,	'Administrators'
), (
	59,	'Lunch_A'
), (
	60,	'Drama'
), (
	60,	'Lunch_B'
), (
	61,	'Drama'
), (
	61,	'Lunch_A'
), (
	62,	'Drama'
), (
	62,	'Lunch_B'
), (
	63,	'Lunch_A'
), (
	64,	'Debating'
), (
	64,	'Lunch_B'
), (
	65,	'Lunch_A'
), (
	65,	'Science-teachers'
), (
	66,	'Lunch_B'
), (
	66,	'Sport-teachers'
), (
	67,	'Lunch_A'
), (
	67,	'Science-teachers'
), (
	68,	'Lunch_B'
), (
	68,	'Science-teachers'
), (
	69,	'Chess'
), (
	69,	'Lunch_A'
), (
	69,	'Science-teachers'
), (
	70,	'Lunch_B'
), (
	71,	'Lunch_A'
), (
	71,	'Sport-teachers'
), (
	72,	'Assembly'
), (
	72,	'Choir'
), (
	72,	'Lunch_B'
), (
	73,	'Administrators'
), (
	73,	'Lunch_A'
), (
	74,	'Lunch_B'
), (
	75,	'Lunch_A'
), (
	76,	'Lunch_B'
), (
	77,	'Lunch_A'
), (
	78,	'Lunch_B'
);
UNLOCK TABLES;

LOCK TABLES `tbl_service` WRITE;
INSERT INTO `tbl_service` (
	`teacherRegisterId`,
	`topic`,
	`level`,
	`requiredLessonsPerWeek`,
	`minimumConsecutiveLessons`,
	`maximumClassSize`,
	`synchronisationId`,
	`idealTimeslotRequest`
) VALUES (
	1,	'Geography',		'1',	1,	1,	NULL,	NULL,			NULL
), (
	1,	'History',		'1',	1,	1,	NULL,	NULL,			NULL
), (
	2,	'Maths',		'1',	1,	1,	NULL,	NULL,			NULL
), (
	2,	'Music',		'1',	1,	1,	NULL,	NULL,			NULL
), (
	3,	'Physics',		'1',	1,	1,	NULL,	NULL,			NULL
), (
	4,	'Domestic Science',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	4,	'Religious Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	6,	'Music',		'2',	2,	1,	NULL,	NULL,			NULL
), (
	7,	'Geography',		'1',	3,	1,	NULL,	NULL,			NULL
), (
	8,	'Maths',		'1',	2,	1,	NULL,	NULL,			NULL
), (
	9,	'Physics',		'1',	2,	1,	3,	NULL,			NULL
), (
	10,	'Domestic Science',	'1',	2,	1,	8,	NULL,			NULL
), (
	11,	'Religious Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	13,	'Geography',		'1',	2,	1,	NULL,	NULL,			NULL
), (
	13,	'Geography',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	14,	'Maths',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	15,	'Chemistry',		'1',	2,	1,	NULL,	NULL,			NULL
), (
	16,	'Physics',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	17,	'Domestic Science',	'1',	2,	2,	8,	NULL,			NULL
), (
	18,	'Physical Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	19,	'Religious Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	20,	'Music',		'2',	2,	1,	NULL,	NULL,			NULL
), (
	21,	'Domestic Science',	'1',	2,	2,	4,	NULL,			NULL
), (
	21,	'Domestic Science',	'2',	4,	2,	NULL,	NULL,			NULL
), (
	23,	'Music',		'1',	1,	1,	NULL,	NULL,			NULL
), (
	23,	'Music',		'2',	2,	1,	NULL,	NULL,			NULL
), (
	24,	'Religious Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	25,	'Geography',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	25,	'Geography',		'3',	4,	1,	NULL,	NULL,			NULL
), (
	26,	'Maths',		'1',	2,	1,	NULL,	NULL,			1
), (
	26,	'Maths',		'2',	3,	1,	NULL,	NULL,			1
), (
	27,	'Chemistry',		'1',	2,	1,	NULL,	NULL,			NULL
), (
	27,	'Chemistry',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	28,	'French',		'1',	2,	1,	NULL,	NULL,			NULL
), (
	28,	'Music',		'1',	1,	1,	NULL,	NULL,			NULL
), (
	29,	'French',		'1',	2,	1,	NULL,	NULL,			NULL
), (
	29,	'French',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	29,	'French',		'3',	4,	1,	NULL,	NULL,			NULL
), (
	30,	'Physics',		'1',	2,	1,	3,	NULL,			NULL
), (
	30,	'Physics',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	30,	'Physics',		'3',	4,	1,	NULL,	NULL,			NULL
), (
	31,	'Domestic Science',	'1',	2,	2,	8,	NULL,			NULL
), (
	31,	'Domestic Science',	'2',	4,	2,	8,	NULL,			NULL
), (
	32,	'Games',		'1',	3,	3,	NULL,	NULL,			NULL
), (
	32,	'Physical Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	33,	'Physics',		'3',	4,	1,	3,	NULL,			NULL
), (
	34,	'Biology',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	34,	'Biology',		'3',	4,	1,	NULL,	NULL,			NULL
), (
	35,	'Games',		'1',	3,	3,	NULL,	NULL,			NULL
), (
	35,	'Physical Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	36,	'Biology',		'1',	2,	1,	NULL,	NULL,			NULL
), (
	37,	'Religious Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	39,	'Music',		'1',	2,	1,	NULL,	NULL,			7
), (
	39,	'Music',		'2',	3,	1,	NULL,	NULL,			7
), (
	39,	'Music',		'3',	4,	1,	NULL,	NULL,			7
), (
	40,	'Religious Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	41,	'Geography',		'3',	4,	1,	NULL,	'Geography/History 3',	NULL
), (
	41,	'Geography',		'4',	5,	1,	NULL,	'Geography/History 4',	NULL
), (
	42,	'Domestic Science',	'1',	2,	2,	4,	NULL,			3
), (
	42,	'Domestic Science',	'2',	3,	2,	NULL,	NULL,			3
), (
	42,	'Domestic Science',	'3',	4,	2,	NULL,	NULL,			3
), (
	43,	'Biology',		'4',	5,	1,	NULL,	NULL,			NULL
), (
	44,	'Maths',		'2',	3,	1,	NULL,	NULL,			1
), (
	44,	'Maths',		'3',	4,	1,	NULL,	NULL,			1
), (
	44,	'Maths',		'4',	5,	1,	NULL,	NULL,			1
), (
	45,	'French',		'3',	4,	1,	NULL,	NULL,			NULL
), (
	45,	'French',		'4',	5,	1,	NULL,	NULL,			NULL
), (
	46,	'Chemistry',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	46,	'Chemistry',		'3',	4,	1,	NULL,	NULL,			NULL
), (
	46,	'Chemistry',		'4',	5,	1,	NULL,	NULL,			NULL
), (
	47,	'Games',		'',	3,	3,	NULL,	NULL,			NULL
), (
	47,	'Physical Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	48,	'Physics',		'3.A',	4,	1,	NULL,	'Physics 3 stream',	NULL
), (
	48,	'Physics',		'4',	5,	1,	NULL,	NULL,			NULL
), (
	49,	'Physics',		'3.B',	4,	1,	3,	'Physics 3 stream',	NULL
), (
	49,	'Physics',		'4',	5,	1,	NULL,	NULL,			NULL
), (
	50,	'Biology',		'3',	4,	1,	NULL,	NULL,			NULL
), (
	50,	'Biology',		'4',	5,	1,	NULL,	NULL,			NULL
), (
	51,	'Domestic Science',	'1',	2,	2,	8,	NULL,			3
), (
	51,	'Domestic Science',	'2',	3,	2,	NULL,	NULL,			3
), (
	51,	'Domestic Science',	'3',	4,	2,	8,	NULL,			3
), (
	52,	'Games',		'',	3,	3,	NULL,	NULL,			NULL
), (
	52,	'Physical Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	53,	'Music',		'1',	2,	1,	NULL,	NULL,			7
), (
	54,	'Religious Education',	'1',	1,	1,	NULL,	NULL,			NULL
), (
	55,	'History',		'3',	4,	1,	NULL,	'Geography/History 3',	NULL
), (
	55,	'History',		'4',	5,	1,	NULL,	'Geography/History 4',	NULL
), (
	57,	'Music',		'2',	2,	1,	NULL,	NULL,			NULL
), (
	57,	'Music',		'3',	3,	1,	NULL,	NULL,			NULL
), (
	58,	'German',		'1',	3,	1,	NULL,	'Foreign Languages/1',	NULL
), (
	58,	'German',		'2',	4,	1,	NULL,	'Foreign Languages/2',	NULL
), (
	58,	'German',		'3',	5,	1,	NULL,	'Foreign Languages/3',	NULL
), (
	59,	'English',		'1',	5,	1,	NULL,	NULL,			NULL
), (
	59,	'English',		'2',	5,	1,	NULL,	NULL,			NULL
), (
	60,	'Geography',		'1',	3,	1,	NULL,	NULL,			NULL
), (
	60,	'Geography',		'2',	4,	1,	NULL,	NULL,			NULL
), (
	60,	'Geography',		'3',	5,	1,	NULL,	'Geography/History 3',	NULL
), (
	61,	'Maths',		'1',	5,	1,	NULL,	NULL,			1
), (
	61,	'Maths',		'2',	5,	1,	NULL,	NULL,			1
), (
	61,	'Maths',		'3',	5,	1,	NULL,	NULL,			1
), (
	62,	'Maths',		'1',	5,	1,	NULL,	NULL,			1
), (
	62,	'Maths',		'2',	5,	1,	NULL,	NULL,			1
), (
	62,	'Maths',		'3',	5,	1,	NULL,	NULL,			1
), (
	63,	'French',		'1',	3,	1,	NULL,	'Foreign Languages/1',	NULL
), (
	63,	'French',		'2',	4,	1,	NULL,	'Foreign Languages/2',	NULL
), (
	64,	'English',		'2',	5,	1,	NULL,	NULL,			NULL
), (
	64,	'English',		'3',	5,	1,	NULL,	NULL,			NULL
), (
	65,	'Chemistry',		'1',	4,	2,	NULL,	NULL,			NULL
), (
	65,	'Chemistry',		'2',	6,	2,	NULL,	NULL,			NULL
), (
	65,	'Chemistry',		'3',	8,	2,	NULL,	NULL,			NULL
), (
	66,	'Games',		'',	5,	5,	NULL,	NULL,			NULL
), (
	66,	'Physical Education',	'1',	2,	1,	NULL,	NULL,			NULL
), (
	66,	'Physical Education',	'2',	2,	1,	NULL,	NULL,			NULL
), (
	66,	'Physical Education',	'3',	2,	1,	NULL,	NULL,			NULL
), (
	66,	'Swimming',		'1',	4,	2,	NULL,	NULL,			NULL
), (
	66,	'Swimming',		'2',	4,	2,	NULL,	NULL,			NULL
), (
	66,	'Swimming',		'3',	4,	2,	NULL,	NULL,			NULL
), (
	67,	'Physics',		'1',	4,	2,	NULL,	NULL,			NULL
), (
	67,	'Physics',		'2',	6,	2,	NULL,	NULL,			NULL
), (
	67,	'Physics',		'3.A',	8,	2,	NULL,	'Physics 3 stream',	NULL
), (
	68,	'Physics',		'1',	4,	2,	NULL,	NULL,			NULL
), (
	68,	'Physics',		'2',	6,	2,	NULL,	NULL,			NULL
), (
	68,	'Physics',		'3.B',	8,	2,	3,	'Physics 3 stream',	NULL
), (
	69,	'Biology',		'1',	4,	2,	NULL,	NULL,			NULL
), (
	69,	'Biology',		'2',	6,	2,	NULL,	NULL,			NULL
), (
	69,	'Biology',		'3',	8,	2,	NULL,	NULL,			NULL
), (
	70,	'Domestic Science',	'1',	2,	2,	8,	NULL,			4
), (
	70,	'Domestic Science',	'2',	4,	2,	8,	NULL,			4
), (
	70,	'Domestic Science',	'3',	6,	2,	8,	NULL,			4
), (
	71,	'Games',		'',	5,	5,	NULL,	NULL,			NULL
), (
	71,	'Physical Education',	'1',	2,	1,	NULL,	NULL,			NULL
), (
	71,	'Physical Education',	'2',	2,	1,	NULL,	NULL,			NULL
), (
	71,	'Physical Education',	'3',	2,	1,	NULL,	NULL,			NULL
), (
	71,	'Swimming',		'1',	4,	2,	NULL,	NULL,			NULL
), (
	71,	'Swimming',		'2',	4,	2,	NULL,	NULL,			NULL
), (
	71,	'Swimming',		'3',	4,	2,	NULL,	NULL,			NULL
), (
	72,	'Music',		'1',	1,	1,	NULL,	NULL,			NULL
), (
	72,	'Music',		'2',	2,	1,	NULL,	NULL,			NULL
), (
	73,	'Religious Education',	'1',	2,	1,	NULL,	NULL,			NULL
), (
	73,	'Religious Education',	'2',	3,	1,	NULL,	NULL,			NULL
), (
	74,	'History',		'1',	3,	1,	NULL,	NULL,			NULL
), (
	74,	'History',		'2',	4,	1,	NULL,	NULL,			NULL
), (
	74,	'History',		'3',	5,	1,	NULL,	'Geography/History 3',	NULL
), (
	75,	'Chinese',		'1',	3,	1,	NULL,	'Foreign Languages/1',	NULL
), (
	75,	'Chinese',		'2',	4,	1,	NULL,	'Foreign Languages/2',	NULL
), (
	75,	'Chinese',		'3',	5,	1,	NULL,	'Foreign Languages/3',	NULL
), (
	76,	'Woodwork',		'1',	2,	1,	NULL,	NULL,			NULL
), (
	76,	'Woodwork',		'2',	3,	1,	NULL,	NULL,			NULL
), (
	77,	'Technical Drawing',	'1',	2,	1,	NULL,	NULL,			NULL
), (
	77,	'Technical Drawing',	'2',	3,	1,	NULL,	NULL,			NULL
), (
	78,	'Art',			'1',	2,	1,	NULL,	NULL,			NULL
), (
	78,	'Art',			'2',	3,	1,	NULL,	NULL,			NULL
), (
	78,	'Art',			'3',	4,	1,	NULL,	NULL,			NULL
);
UNLOCK TABLES;

LOCK TABLES `tbl_specialtyTopic` WRITE;
INSERT INTO `tbl_specialtyTopic` (
	`teacherRegisterId`,
	`topic`
) VALUES (
	6,	'Music'
), (
	7,	'Geography'
), (
	8,	'Maths'
), (
	9,	'Physics'
), (
	10,	'Domestic Science'
), (
	11,	'Religious Education'
), (
	13,	'Geography'
), (
	14,	'Maths'
), (
	15,	'Chemistry'
), (
	16,	'Physics'
), (
	17,	'Domestic Science'
), (
	18,	'Physical Education'
), (
	19,	'Religious Education'
), (
	20,	'Music'
), (
	21,	'Domestic Science'
), (
	23,	'Music'
), (
	24,	'Religious Education'
), (
	25,	'Geography'
), (
	26,	'Maths'
), (
	27,	'Chemistry'
), (
	28,	'Music'
), (
	29,	'French'
), (
	30,	'Physics'
), (
	31,	'Domestic Science'
), (
	32,	'Games'
), (
	33,	'Physics'
), (
	34,	'Biology'
), (
	35,	'Physical Education'
), (
	36,	'Biology'
), (
	37,	'Religious Education'
), (
	39,	'Music'
), (
	40,	'Religious Education'
), (
	41,	'Geography'
), (
	42,	'Domestic Science'
), (
	43,	'Biology'
), (
	44,	'Maths'
), (
	45,	'French'
), (
	46,	'Chemistry'
), (
	47,	'Games'
), (
	48,	'Physics'
), (
	49,	'Physics'
), (
	50,	'Biology'
), (
	51,	'Domestic Science'
), (
	52,	'Physical Education'
), (
	53,	'Music'
), (
	54,	'Religious Education'
), (
	55,	'History'
), (
	57,	'Music'
), (
	58,	'German'
), (
	59,	'English'
), (
	60,	'Geography'
), (
	62,	'Maths'
), (
	63,	'French'
), (
	64,	'English'
), (
	65,	'Chemistry'
), (
	66,	'Games'
), (
	67,	'Physics'
), (
	68,	'Physics'
), (
	69,	'Biology'
), (
	70,	'Domestic Science'
), (
	71,	'Physical Education'
), (
	72,	'Music'
), (
	73,	'Religious Education'
), (
	74,	'History'
), (
	75,	'Chinese'
), (
	76,	'Woodwork'
), (
	77,	'Technical Drawing'
), (
	78,	'Art'
);
UNLOCK TABLES;

LOCK TABLES `tbl_specificTimeRequest` WRITE;
INSERT INTO `tbl_specificTimeRequest` (
	`teacherRegisterId`,
	`topic`,
	`level`,
	`day`,
	`timeslotId`
) VALUES (
	32,	'Games',	'1',	'Wednesday',	2
), (
	32,	'Games',	'1',	'Wednesday',	3
), (
	32,	'Games',	'1',	'Wednesday',	4
), (
	35,	'Games',	'1',	'Tuesday',	2
), (
	35,	'Games',	'1',	'Tuesday',	3
), (
	35,	'Games',	'1',	'Tuesday',	4
), (
	47,	'Games',	'',	'Wednesday',	5
), (
	47,	'Games',	'',	'Wednesday',	6
), (
	47,	'Games',	'',	'Wednesday',	7
), (
	52,	'Games',	'',	'Tuesday',	5
), (
	52,	'Games',	'',	'Tuesday',	6
), (
	52,	'Games',	'',	'Tuesday',	7
), (
	66,	'Games',	'',	'Wednesday',	8
), (
	66,	'Games',	'',	'Wednesday',	9
), (
	66,	'Games',	'',	'Wednesday',	10
), (
	66,	'Games',	'',	'Wednesday',	11
), (
	66,	'Games',	'',	'Wednesday',	12
), (
	71,	'Games',	'',	'Tuesday',	8
), (
	71,	'Games',	'',	'Tuesday',	9
), (
	71,	'Games',	'',	'Tuesday',	10
), (
	71,	'Games',	'',	'Tuesday',	11
), (
	71,	'Games',	'',	'Tuesday',	12
);
UNLOCK TABLES;

LOCK TABLES `tbl_requiredFacility` WRITE;
INSERT INTO `tbl_requiredFacility` (
	`teacherRegisterId`,
	`topic`,
	`level`,
	`facilityTypeId`
) VALUES (
	6,	'Music',		'2',	4	-- 'Musical instruments'
), (
	7,	'Geography',		'1',	1	-- 'Blackboard'
), (
	7,	'Geography',		'1',	2	-- 'Desks'
), (
	8,	'Maths',		'1',	1	-- 'Blackboard'
), (
	8,	'Maths',		'1',	2	-- 'Desks'
), (
	9,	'Physics',		'1',	1	-- 'Blackboard'
), (
	9,	'Physics',		'1',	2	-- 'Desks'
), (
	9,	'Physics',		'1',	5	-- 'Physics-lab'
), (
	10,	'Domestic Science',	'1',	3	-- 'Kitchen-equipment'
), (
	11,	'Religious Education',	'1',	2	-- 'Desks'
), (
	13,	'Geography',		'1',	6	-- 'Blackboard'
), (
	13,	'Geography',		'1',	8	-- 'Desks'
), (
	13,	'Geography',		'2',	6	-- 'Blackboard'
), (
	13,	'Geography',		'2',	8	-- 'Desks'
), (
	14,	'Maths',		'2',	6	-- 'Blackboard'
), (
	14,	'Maths',		'2',	8	-- 'Desks'
), (
	15,	'Chemistry',		'1',	6	-- 'Blackboard'
), (
	15,	'Chemistry',		'1',	7	-- 'Chemistry-lab'
), (
	15,	'Chemistry',		'1',	8	-- 'Desks'
), (
	16,	'Physics',		'2',	6	-- 'Blackboard'
), (
	16,	'Physics',		'2',	8	-- 'Desks'
), (
	16,	'Physics',		'2',	12	-- 'Physics-lab'
), (
	17,	'Domestic Science',	'1',	10	-- 'Kitchen-equipment'
), (
	18,	'Physical Education',	'1',	9	-- 'Gym-equipment'
), (
	19,	'Religious Education',	'1',	8	-- 'Desks'
), (
	20,	'Music',		'2',	11	-- 'Musical instruments'
), (
	21,	'Domestic Science',	'1',	19	-- 'Kitchen-equipment'
), (
	21,	'Domestic Science',	'2',	19	-- 'Kitchen-equipment'
), (
	23,	'Music',		'1',	21	-- 'Musical instruments'
), (
	23,	'Music',		'2',	21	-- 'Musical instruments'
), (
	24,	'Religious Education',	'1',	16	-- 'Desks'
), (
	25,	'Geography',		'2',	14	-- 'Blackboard'
), (
	25,	'Geography',		'2',	16	-- 'Desks'
), (
	25,	'Geography',		'3',	14	-- 'Blackboard'
), (
	25,	'Geography',		'3',	16	-- 'Desks'
), (
	26,	'Maths',		'1',	14	-- 'Blackboard'
), (
	26,	'Maths',		'1',	16	-- 'Desks'
), (
	26,	'Maths',		'2',	14	-- 'Blackboard'
), (
	26,	'Maths',		'2',	16	-- 'Desks'
), (
	27,	'Chemistry',		'1',	14	-- 'Blackboard'
), (
	27,	'Chemistry',		'1',	15	-- 'Chemistry-lab'
), (
	27,	'Chemistry',		'1',	16	-- 'Desks'
), (
	27,	'Chemistry',		'2',	14	-- 'Blackboard'
), (
	27,	'Chemistry',		'2',	15	-- 'Chemistry-lab'
), (
	27,	'Chemistry',		'2',	16	-- 'Desks'
), (
	28,	'French',		'1',	14	-- 'Blackboard'
), (
	28,	'French',		'1',	16	-- 'Desks'
), (
	28,	'Music',		'1',	21	-- 'Musical instruments'
), (
	29,	'French',		'1',	14	-- 'Blackboard'
), (
	29,	'French',		'1',	16	-- 'Desks'
), (
	29,	'French',		'2',	14	-- 'Blackboard'
), (
	29,	'French',		'2',	16	-- 'Desks'
), (
	29,	'French',		'3',	14	-- 'Blackboard'
), (
	29,	'French',		'3',	16	-- 'Desks'
), (
	30,	'Physics',		'1',	14	-- 'Blackboard'
), (
	30,	'Physics',		'1',	16	-- 'Desks'
), (
	30,	'Physics',		'1',	22	-- 'Physics-lab'
), (
	30,	'Physics',		'2',	14	-- 'Blackboard'
), (
	30,	'Physics',		'2',	16	-- 'Desks'
), (
	30,	'Physics',		'2',	22	-- 'Physics-lab'
), (
	30,	'Physics',		'3',	14	-- 'Blackboard'
), (
	30,	'Physics',		'3',	16	-- 'Desks'
), (
	30,	'Physics',		'3',	22	-- 'Physics-lab'
), (
	31,	'Domestic Science',	'1',	19	-- 'Kitchen-equipment'
), (
	31,	'Domestic Science',	'2',	19	-- 'Kitchen-equipment'
), (
	32,	'Games',		'1',	24	-- 'Running-track'
), (
	32,	'Physical Education',	'1',	18	-- 'Gym-equipment'
), (
	33,	'Physics',		'3',	14	-- 'Blackboard'
), (
	33,	'Physics',		'3',	16	-- 'Desks'
), (
	33,	'Physics',		'3',	22	-- 'Physics-lab'
), (
	34,	'Biology',		'2',	13	-- 'Biology-lab'
), (
	34,	'Biology',		'2',	14	-- 'Blackboard'
), (
	34,	'Biology',		'2',	16	-- 'Desks'
), (
	34,	'Biology',		'3',	13	-- 'Biology-lab'
), (
	34,	'Biology',		'3',	14	-- 'Blackboard'
), (
	34,	'Biology',		'3',	16	-- 'Desks'
), (
	35,	'Games',		'1',	17	-- 'Goal-posts'
), (
	35,	'Games',		'1',	26	-- 'Showers'
), (
	35,	'Physical Education',	'1',	18	-- 'Gym-equipment'
), (
	36,	'Biology',		'1',	13	-- 'Biology-lab'
), (
	36,	'Biology',		'1',	14	-- 'Blackboard'
), (
	36,	'Biology',		'1',	16	-- 'Desks'
), (
	37,	'Religious Education',	'1',	16	-- 'Desks'
), (
	39,	'Music',		'1',	35	-- 'Musical instruments'
), (
	39,	'Music',		'2',	35	-- 'Musical instruments'
), (
	39,	'Music',		'3',	35	-- 'Musical instruments'
), (
	40,	'Religious Education',	'1',	30	-- 'Desks'
), (
	41,	'Geography',		'3',	28	-- 'Blackboard'
), (
	41,	'Geography',		'3',	30	-- 'Desks'
), (
	41,	'Geography',		'4',	28	-- 'Blackboard'
), (
	41,	'Geography',		'4',	30	-- 'Desks'
), (
	42,	'Domestic Science',	'1',	33	-- 'Kitchen-equipment'
), (
	42,	'Domestic Science',	'2',	33	-- 'Kitchen-equipment'
), (
	42,	'Domestic Science',	'3',	33	-- 'Kitchen-equipment'
), (
	43,	'Biology',		'4',	27	-- 'Biology-lab'
), (
	43,	'Biology',		'4',	28	-- 'Blackboard'
), (
	43,	'Biology',		'4',	30	-- 'Desks'
), (
	44,	'Maths',		'2',	28	-- 'Blackboard'
), (
	44,	'Maths',		'2',	30	-- 'Desks'
), (
	44,	'Maths',		'3',	28	-- 'Blackboard'
), (
	44,	'Maths',		'3',	30	-- 'Desks'
), (
	44,	'Maths',		'4',	28	-- 'Blackboard'
), (
	44,	'Maths',		'4',	30	-- 'Desks'
), (
	45,	'French',		'3',	28	-- 'Blackboard'
), (
	45,	'French',		'3',	30	-- 'Desks'
), (
	45,	'French',		'4',	28	-- 'Blackboard'
), (
	45,	'French',		'4',	30	-- 'Desks'
), (
	46,	'Chemistry',		'2',	28	-- 'Blackboard'
), (
	46,	'Chemistry',		'2',	29	-- 'Chemistry-lab'
), (
	46,	'Chemistry',		'2',	30	-- 'Desks'
), (
	46,	'Chemistry',		'3',	28	-- 'Blackboard'
), (
	46,	'Chemistry',		'3',	29	-- 'Chemistry-lab'
), (
	46,	'Chemistry',		'3',	30	-- 'Desks'
), (
	46,	'Chemistry',		'4',	28	-- 'Blackboard'
), (
	46,	'Chemistry',		'4',	29	-- 'Chemistry-lab'
), (
	46,	'Chemistry',		'4',	30	-- 'Desks'
), (
	47,	'Games',		'',	38	-- 'Running-track'
), (
	47,	'Physical Education',	'1',	32	-- 'Gym-equipment'
), (
	48,	'Physics',		'3.A',	28	-- 'Blackboard'
), (
	48,	'Physics',		'3.A',	30	-- 'Desks'
), (
	48,	'Physics',		'3.A',	37	-- 'Physics-lab'
), (
	48,	'Physics',		'4',	28	-- 'Blackboard'
), (
	48,	'Physics',		'4',	30	-- 'Desks'
), (
	48,	'Physics',		'4',	37	-- 'Physics-lab'
), (
	49,	'Physics',		'3.B',	28	-- 'Blackboard'
), (
	49,	'Physics',		'3.B',	30	-- 'Desks'
), (
	49,	'Physics',		'3.B',	37	-- 'Physics-lab'
), (
	49,	'Physics',		'4',	28	-- 'Blackboard'
), (
	49,	'Physics',		'4',	30	-- 'Desks'
), (
	49,	'Physics',		'4',	37	-- 'Physics-lab'
), (
	50,	'Biology',		'3',	27	-- 'Biology-lab'
), (
	50,	'Biology',		'3',	28	-- 'Blackboard'
), (
	50,	'Biology',		'3',	30	-- 'Desks'
), (
	50,	'Biology',		'4',	27	-- 'Biology-lab'
), (
	50,	'Biology',		'4',	28	-- 'Blackboard'
), (
	50,	'Biology',		'4',	30	-- 'Desks'
), (
	51,	'Domestic Science',	'1',	33	-- 'Kitchen-equipment'
), (
	51,	'Domestic Science',	'2',	33	-- 'Kitchen-equipment'
), (
	51,	'Domestic Science',	'3',	33	-- 'Kitchen-equipment'
), (
	52,	'Games',		'',	31	-- 'Goal-posts'
), (
	52,	'Games',		'',	40	-- 'Showers'
), (
	52,	'Physical Education',	'1',	32	-- 'Gym-equipment'
), (
	53,	'Music',		'1',	35	-- 'Musical instruments'
), (
	54,	'Religious Education',	'1',	30	-- 'Desks'
), (
	55,	'History',		'3',	28	-- 'Blackboard'
), (
	55,	'History',		'3',	30	-- 'Desks'
), (
	55,	'History',		'4',	28	-- 'Blackboard'
), (
	55,	'History',		'4',	30	-- 'Desks'
), (
	57,	'Music',		'2',	53	-- 'Musical instruments'
), (
	57,	'Music',		'3',	53	-- 'Musical instruments'
), (
	58,	'German',		'1',	44	-- 'Blackboard'
), (
	58,	'German',		'1',	46	-- 'Desks'
), (
	58,	'German',		'2',	44	-- 'Blackboard'
), (
	58,	'German',		'2',	46	-- 'Desks'
), (
	58,	'German',		'3',	44	-- 'Blackboard'
), (
	58,	'German',		'3',	46	-- 'Desks'
), (
	59,	'English',		'1',	44	-- 'Blackboard'
), (
	59,	'English',		'1',	46	-- 'Desks'
), (
	59,	'English',		'2',	44	-- 'Blackboard'
), (
	59,	'English',		'2',	46	-- 'Desks'
), (
	60,	'Geography',		'1',	44	-- 'Blackboard'
), (
	60,	'Geography',		'1',	46	-- 'Desks'
), (
	60,	'Geography',		'2',	44	-- 'Blackboard'
), (
	60,	'Geography',		'2',	46	-- 'Desks'
), (
	60,	'Geography',		'3',	44	-- 'Blackboard'
), (
	60,	'Geography',		'3',	46	-- 'Desks'
), (
	61,	'Maths',		'1',	44	-- 'Blackboard'
), (
	61,	'Maths',		'1',	46	-- 'Desks'
), (
	61,	'Maths',		'2',	44	-- 'Blackboard'
), (
	61,	'Maths',		'2',	46	-- 'Desks'
), (
	61,	'Maths',		'3',	44	-- 'Blackboard'
), (
	61,	'Maths',		'3',	46	-- 'Desks'
), (
	62,	'Maths',		'1',	44	-- 'Blackboard'
), (
	62,	'Maths',		'1',	46	-- 'Desks'
), (
	62,	'Maths',		'2',	44	-- 'Blackboard'
), (
	62,	'Maths',		'2',	46	-- 'Desks'
), (
	62,	'Maths',		'3',	44	-- 'Blackboard'
), (
	62,	'Maths',		'3',	46	-- 'Desks'
), (
	63,	'French',		'1',	44	-- 'Blackboard'
), (
	63,	'French',		'1',	46	-- 'Desks'
), (
	63,	'French',		'2',	44	-- 'Blackboard'
), (
	63,	'French',		'2',	46	-- 'Desks'
), (
	64,	'English',		'2',	44	-- 'Blackboard'
), (
	64,	'English',		'2',	46	-- 'Desks'
), (
	64,	'English',		'3',	44	-- 'Blackboard'
), (
	64,	'English',		'3',	46	-- 'Desks'
), (
	65,	'Chemistry',		'1',	44	-- 'Blackboard'
), (
	65,	'Chemistry',		'1',	45	--'Chemistry-lab'
), (
	65,	'Chemistry',		'1',	46	-- 'Desks'
), (
	65,	'Chemistry',		'2',	44	-- 'Blackboard'
), (
	65,	'Chemistry',		'2',	45	--'Chemistry-lab'
), (
	65,	'Chemistry',		'2',	46	-- 'Desks'
), (
	65,	'Chemistry',		'3',	44	-- 'Blackboard'
), (
	65,	'Chemistry',		'3',	45	--'Chemistry-lab'
), (
	65,	'Chemistry',		'3',	46	-- 'Desks'
), (
	66,	'Games',		'',	56	-- 'Running-track'
), (
	66,	'Physical Education',	'1',	50	-- 'Gym-equipment'
), (
	66,	'Physical Education',	'2',	50	-- 'Gym-equipment'
), (
	66,	'Physical Education',	'3',	50	-- 'Gym-equipment'
), (
	66,	'Swimming',		'1',	60	-- 'Swimming-pool'
), (
	66,	'Swimming',		'2',	60	-- 'Swimming-pool'
), (
	66,	'Swimming',		'3',	60	-- 'Swimming-pool'
), (
	67,	'Physics',		'1',	44	-- 'Blackboard'
), (
	67,	'Physics',		'1',	46	-- 'Desks'
), (
	67,	'Physics',		'1',	54	-- 'Physics-lab'
), (
	67,	'Physics',		'2',	44	-- 'Blackboard'
), (
	67,	'Physics',		'2',	46	-- 'Desks'
), (
	67,	'Physics',		'2',	54	-- 'Physics-lab'
), (
	67,	'Physics',		'3.A',	44	-- 'Blackboard'
), (
	67,	'Physics',		'3.A',	46	-- 'Desks'
), (
	67,	'Physics',		'3.A',	54	-- 'Physics-lab'
), (
	68,	'Physics',		'1',	44	-- 'Blackboard'
), (
	68,	'Physics',		'1',	46	-- 'Desks'
), (
	68,	'Physics',		'1',	54	-- 'Physics-lab'
), (
	68,	'Physics',		'2',	44	-- 'Blackboard'
), (
	68,	'Physics',		'2',	46	-- 'Desks'
), (
	68,	'Physics',		'2',	54	-- 'Physics-lab'
), (
	68,	'Physics',		'3.B',	44	-- 'Blackboard'
), (
	68,	'Physics',		'3.B',	46	-- 'Desks'
), (
	68,	'Physics',		'3.B',	54	-- 'Physics-lab'
), (
	69,	'Biology',		'1',	43	-- 'Biology-lab'
), (
	69,	'Biology',		'1',	44	-- 'Blackboard'
), (
	69,	'Biology',		'1',	46	-- 'Desks'
), (
	69,	'Biology',		'2',	43	-- 'Biology-lab'
), (
	69,	'Biology',		'2',	44	-- 'Blackboard'
), (
	69,	'Biology',		'2',	46	-- 'Desks'
), (
	69,	'Biology',		'3',	43	-- 'Biology-lab'
), (
	69,	'Biology',		'3',	44	-- 'Blackboard'
), (
	69,	'Biology',		'3',	46	-- 'Desks'
), (
	70,	'Domestic Science',	'1',	51	-- 'Kitchen-equipment'
), (
	70,	'Domestic Science',	'2',	51	-- 'Kitchen-equipment'
), (
	70,	'Domestic Science',	'3',	51	-- 'Kitchen-equipment'
), (
	71,	'Games',		'',	49	-- 'Goal-posts'
), (
	71,	'Games',		'',	58	-- 'Showers'
), (
	71,	'Physical Education',	'1',	50	-- 'Gym-equipment'
), (
	71,	'Physical Education',	'2',	50	-- 'Gym-equipment'
), (
	71,	'Physical Education',	'3',	50	-- 'Gym-equipment'
), (
	71,	'Swimming',		'1',	60	-- 'Swimming-pool'
), (
	71,	'Swimming',		'2',	60	-- 'Swimming-pool'
), (
	71,	'Swimming',		'3',	60	-- 'Swimming-pool'
), (
	72,	'Music',		'1',	53	-- 'Musical instruments'
), (
	72,	'Music',		'2',	53	-- 'Musical instruments'
), (
	73,	'Religious Education',	'1',	46	-- 'Desks'
), (
	73,	'Religious Education',	'2',	46	-- 'Desks'
), (
	74,	'History',		'1',	44	-- 'Blackboard'
), (
	74,	'History',		'1',	46	-- 'Desks'
), (
	74,	'History',		'2',	44	-- 'Blackboard'
), (
	74,	'History',		'2',	46	-- 'Desks'
), (
	74,	'History',		'3',	44	-- 'Blackboard'
), (
	74,	'History',		'3',	46	-- 'Desks'
), (
	75,	'Chinese',		'1',	44	-- 'Blackboard'
), (
	75,	'Chinese',		'1',	46	-- 'Desks'
), (
	75,	'Chinese',		'2',	44	-- 'Blackboard'
), (
	75,	'Chinese',		'2',	46	-- 'Desks'
), (
	75,	'Chinese',		'3',	44	-- 'Blackboard'
), (
	75,	'Chinese',		'3',	46	-- 'Desks'
), (
	76,	'Woodwork',		'1',	62	-- 'Wood-tools'
), (
	76,	'Woodwork',		'1',	63	-- 'Work-benches'
), (
	76,	'Woodwork',		'2',	62	-- 'Wood-tools'
), (
	76,	'Woodwork',		'2',	63	-- 'Work-benches'
), (
	77,	'Technical Drawing',	'1',	47	-- 'Drawing-desks'
), (
	77,	'Technical Drawing',	'2',	47	-- 'Drawing-desks'
), (
	78,	'Art',			'1',	42	-- 'Art-tools'
), (
	78,	'Art',			'1',	48	-- 'Easels'
), (
	78,	'Art',			'1',	59	-- 'Sinks'
), (
	78,	'Art',			'2',	42	-- 'Art-tools'
), (
	78,	'Art',			'2',	48	-- 'Easels'
), (
	78,	'Art',			'2',	59	-- 'Sinks'
), (
	78,	'Art',			'3',	42	-- 'Art-tools'
), (
	78,	'Art',			'3',	48	-- 'Easels'
), (
	78,	'Art',			'3',	59	-- 'Sinks'
);
UNLOCK TABLES;

LOCK TABLES `tbl_knowledgeRequirement` WRITE;
INSERT INTO `tbl_knowledgeRequirement` (
	`studentBodyRegisterId`,
	`priority`,
	`topic`,
	`level`
) VALUES (
	1,	'core',		'Geography',		'1'
), (
	1,	'core',		'Maths',		'1'
), (
	1,	'core',		'Physics',		'1'
), (
	1,	'optional',	'Music',		'1'
), (
	1,	'optional',	'Religious Education',	'1'
), (
	2,	'core',		'Geography',		'1'
), (
	2,	'core',		'Maths',		'1'
), (
	2,	'core',		'Physics',		'1'
), (
	2,	'optional',	'Domestic Science',	'1'
), (
	2,	'optional',	'Religious Education',	'1'
), (
	3,	'core',		'History',		'1'
), (
	3,	'core',		'Maths',		'1'
), (
	3,	'core',		'Physics',		'1'
), (
	3,	'optional',	'Domestic Science',	'1'
), (
	3,	'optional',	'Music',		'1'
), (
	4,	'core',		'Geography',		'1'
), (
	4,	'core',		'Maths',		'1'
), (
	4,	'optional',	'Music',		'2'
), (
	4,	'core',		'Physics',		'1'
), (
	4,	'core',		'Religious Education',	'1'
), (
	5,	'optional',	'Domestic Science',	'1'
), (
	5,	'core',		'Geography',		'1'
), (
	5,	'core',		'Maths',		'1'
), (
	5,	'core',		'Physics',		'1'
), (
	5,	'core',		'Religious Education',	'1'
), (
	6,	'core',		'Geography',		'1'
), (
	6,	'core',		'Maths',		'2'
), (
	6,	'optional',	'Music',		'2'
), (
	6,	'core',		'Physical Education',	'1'
), (
	6,	'core',		'Physics',		'2'
), (
	6,	'core',		'Religious Education',	'1'
), (
	7,	'core',		'Chemistry',		'1'
), (
	7,	'optional',	'Domestic Science',	'1'
), (
	7,	'core',		'Geography',		'2'
), (
	7,	'core',		'Maths',		'2'
), (
	7,	'core',		'Physical Education',	'1'
), (
	7,	'core',		'Religious Education',	'1'
), (
	8,	'core',		'French',		'3'
), (
	8,	'core',		'Geography',		'3'
), (
	8,	'core',		'Maths',		'2'
), (
	8,	'optional',	'Music',		'1'
), (
	8,	'core',		'Physical Education',	'1'
), (
	8,	'core',		'Physics',		'3'
), (
	8,	'core',		'Religious Education',	'1'
), (
	9,	'core',		'Chemistry',		'2'
), (
	9,	'optional',	'Domestic Science',	'2'
), (
	9,	'core',		'French',		'2'
), (
	9,	'optional',	'Music',		'2'
), (
	9,	'core',		'Physical Education',	'1'
), (
	9,	'core',		'Physics',		'3'
), (
	9,	'core',		'Religious Education',	'1'
), (
	10,	'core',		'French',		'2'
), (
	10,	'core',		'Games',		'1'
), (
	10,	'core',		'Geography',		'2'
), (
	10,	'core',		'Maths',		'2'
), (
	10,	'optional',	'Music',		'1'
), (
	10,	'core',		'Physical Education',	'1'
), (
	10,	'core',		'Religious Education',	'1'
), (
	11,	'core',		'Biology',		'2'
), (
	11,	'core',		'Chemistry',		'2'
), (
	11,	'optional',	'Domestic Science',	'2'
), (
	11,	'core',		'French',		'1'
), (
	11,	'optional',	'Music',		'2'
), (
	11,	'core',		'Physical Education',	'1'
), (
	11,	'core',		'Physics',		'1'
), (
	11,	'core',		'Religious Education',	'1'
), (
	12,	'core',		'Biology',		'2'
), (
	12,	'optional',	'Domestic Science',	'1'
), (
	12,	'core',		'Games',		'1'
), (
	12,	'core',		'Geography',		'2'
), (
	12,	'optional',	'Music',		'1'
), (
	12,	'core',		'Physical Education',	'1'
), (
	12,	'core',		'Physics',		'2'
), (
	12,	'core',		'Religious Education',	'1'
), (
	13,	'core',		'Biology',		'1'
), (
	13,	'core',		'Chemistry',		'2'
), (
	13,	'optional',	'Domestic Science',	'2'
), (
	13,	'core',		'French',		'2'
), (
	13,	'core',		'Maths',		'2'
), (
	13,	'optional',	'Music',		'1'
), (
	13,	'core',		'Physical Education',	'1'
), (
	13,	'core',		'Religious Education',	'1'
), (
	14,	'core',		'Biology',		'1'
), (
	14,	'core',		'Chemistry',		'2'
), (
	14,	'optional',	'Domestic Science',	'2'
), (
	14,	'core',		'French',		'2'
), (
	14,	'core',		'Geography',		'2'
), (
	14,	'optional',	'Music',		'2'
), (
	14,	'core',		'Religious Education',	'1'
), (
	15,	'core',		'Biology',		'1'
), (
	15,	'core',		'Chemistry',		'2'
), (
	15,	'optional',	'Domestic Science',	'2'
), (
	15,	'core',		'French',		'2'
), (
	15,	'core',		'Maths',		'2'
), (
	15,	'optional',	'Music',		'2'
), (
	15,	'core',		'Religious Education',	'1'
), (
	16,	'core',		'Chemistry',		'1'
), (
	16,	'optional',	'Domestic Science',	'2'
), (
	16,	'core',		'Games',		'1'
), (
	16,	'core',		'Geography',		'2'
), (
	16,	'core',		'Maths',		'1'
), (
	16,	'optional',	'Music',		'1'
), (
	16,	'core',		'Physics',		'1'
), (
	16,	'core',		'Religious Education',	'1'
), (
	17,	'core',		'Biology',		'3'
), (
	17,	'core',		'Chemistry',		'2'
), (
	17,	'optional',	'Domestic Science',	'2'
), (
	17,	'core',		'Geography',		'2'
), (
	17,	'core',		'Maths',		'2'
), (
	17,	'optional',	'Music',		'2'
), (
	17,	'core',		'Physical Education',	'1'
), (
	18,	'core',		'Biology',		'3'
), (
	18,	'optional',	'Domestic Science',	'1'
), (
	18,	'core',		'Geography',		'2'
), (
	18,	'core',		'Maths',		'1'
), (
	18,	'optional',	'Music',		'1'
), (
	18,	'core',		'Physical Education',	'1'
), (
	18,	'core',		'Physics',		'3'
), (
	19,	'optional',	'Domestic Science',	'2'
), (
	19,	'core',		'French',		'3'
), (
	19,	'core',		'Geography',		'3'
), (
	19,	'core',		'Maths',		'4'
), (
	19,	'optional',	'Music',		'2'
), (
	19,	'core',		'Physical Education',	'1'
), (
	19,	'core',		'Physics',		'3.A'
), (
	19,	'core',		'Religious Education',	'1'
), (
	20,	'core',		'Chemistry',		'4'
), (
	20,	'optional',	'Domestic Science',	'3'
), (
	20,	'core',		'French',		'4'
), (
	20,	'optional',	'Music',		'3'
), (
	20,	'core',		'Physical Education',	'1'
), (
	20,	'core',		'Physics',		'4'
), (
	20,	'core',		'Religious Education',	'1'
), (
	21,	'optional',	'Domestic Science',	'1'
), (
	21,	'core',		'French',		'3'
), (
	21,	'core',		'Games',		''
), (
	21,	'core',		'Geography',		'3'
), (
	21,	'core',		'Maths',		'2'
), (
	21,	'optional',	'Music',		'1'
), (
	21,	'core',		'Physical Education',	'1'
), (
	21,	'core',		'Religious Education',	'1'
), (
	22,	'core',		'Biology',		'4'
), (
	22,	'optional',	'Domestic Science',	'2'
), (
	22,	'core',		'French',		'3'
), (
	22,	'core',		'Maths',		'3'
), (
	22,	'optional',	'Music',		'2'
), (
	22,	'core',		'Physical Education',	'1'
), (
	22,	'core',		'Physics',		'3.B'
), (
	22,	'core',		'Religious Education',	'1'
), (
	23,	'core',		'Biology',		'4'
), (
	23,	'optional',	'Domestic Science',	'2'
), (
	23,	'core',		'Games',		''
), (
	23,	'core',		'History',		'4'
), (
	23,	'optional',	'Music',		'1'
), (
	23,	'core',		'Physical Education',	'1'
), (
	23,	'core',		'Physics',		'4'
), (
	23,	'core',		'Religious Education',	'1'
), (
	24,	'core',		'Biology',		'4'
), (
	24,	'core',		'Chemistry',		'4'
), (
	24,	'optional',	'Domestic Science',	'3'
), (
	24,	'core',		'French',		'3'
), (
	24,	'core',		'Maths',		'4'
), (
	24,	'core',		'Physical Education',	'1'
), (
	24,	'core',		'Religious Education',	'1'
), (
	25,	'core',		'Biology',		'4'
), (
	25,	'core',		'French',		'3'
), (
	25,	'core',		'Games',		''
), (
	25,	'core',		'Geography',		'4'
), (
	25,	'optional',	'Music',		'1'
), (
	25,	'core',		'Physics',		'4'
), (
	25,	'core',		'Religious Education',	'1'
), (
	26,	'core',		'Biology',		'3'
), (
	26,	'core',		'Chemistry',		'3'
), (
	26,	'optional',	'Domestic Science',	'3'
), (
	26,	'core'	,	'Maths',		'3'
), (
	26,	'optional',	'Music',		'3'
), (
	26,	'core'	,	'Physics',		'4'
), (
	27,	'core'	,	'Chemistry',		'2'
), (
	27,	'optional',	'Domestic Science',	'3'
), (
	27,	'core'	,	'Games',		''
), (
	27,	'core'	,	'History',		'3'
), (
	27,	'core'	,	'Maths',		'3'
), (
	27,	'optional',	'Music',		'1'
), (
	27,	'core'	,	'Physics',		'3.A'
), (
	27,	'core'	,	'Religious Education',	'1'
), (
	28,	'core'	,	'Biology',		'3'
), (
	28,	'core'	,	'Chemistry',		'3'
), (
	28,	'optional',	'Domestic Science',	'3'
), (
	28,	'core'	,	'Geography',		'4'
), (
	28,	'core'	,	'Maths',		'3'
), (
	28,	'optional',	'Music',		'2'
), (
	28,	'core'	,	'Physical Education',	'1'
), (
	29,	'core'	,	'Biology',		'3'
), (
	29,	'optional',	'Domestic Science',	'3'
), (
	29,	'core'	,	'History',		'3'
), (
	29,	'core'	,	'Maths',		'3'
), (
	29,	'optional',	'Music',		'3'
), (
	29,	'core'	,	'Physical Education',	'1'
), (
	29,	'core'	,	'Physics',		'3.B'
), (
	30,	'optional',	'Art',			'1'
), (
	30,	'core'	,	'Chemistry',		'1'
), (
	30,	'optional',	'Domestic Science',	'1'
), (
	30,	'core'	,	'English',		'1'
), (
	30,	'core'	,	'French',		'1'
), (
	30,	'core'	,	'Games',		''
), (
	30,	'core'	,	'Geography',		'1'
), (
	30,	'core'	,	'History',		'1'
), (
	30,	'core'	,	'Maths',		'1'
), (
	30,	'optional',	'Music',		'1'
), (
	30,	'core'	,	'Physical Education',	'1'
), (
	30,	'core'	,	'Physics',		'1'
), (
	30,	'core'	,	'Swimming',		'1'
), (
	30,	'optional',	'Woodwork',		'1'
), (
	31,	'optional',	'Art',			'2'
), (
	31,	'core'	,	'Chemistry',		'1'
), (
	31,	'optional',	'Domestic Science',	'1'
), (
	31,	'core'	,	'English',		'1'
), (
	31,	'core'	,	'Games',		''
), (
	31,	'core'	,	'Geography',		'1'
), (
	31,	'core'	,	'German',		'1'
), (
	31,	'core'	,	'History',		'1'
), (
	31,	'core'	,	'Maths',		'1'
), (
	31,	'core'	,	'Physical Education',	'1'
), (
	31,	'core'	,	'Physics',		'1'
), (
	31,	'core'	,	'Religious Education',	'1'
), (
	31,	'core'	,	'Swimming',		'1'
), (
	32,	'optional',	'Art',			'1'
), (
	32,	'core'	,	'English',		'1'
), (
	32,	'core'	,	'Games',		''
), (
	32,	'core'	,	'Geography',		'1'
), (
	32,	'core'	,	'History',		'1'
), (
	32,	'core'	,	'Maths',		'1'
), (
	32,	'optional',	'Music',		'1'
), (
	32,	'core'	,	'Physical Education',	'1'
), (
	32,	'core'	,	'Physics',		'1'
), (
	32,	'core'	,	'Religious Education',	'1'
), (
	32,	'core'	,	'Swimming',		'1'
), (
	33,	'optional',	'Art',			'1'
), (
	33,	'core'	,	'Biology',		'1'
), (
	33,	'core'	,	'Chinese',		'1'
), (
	33,	'optional',	'Domestic Science',	'1'
), (
	33,	'core'	,	'English',		'1'
), (
	33,	'core'	,	'Games',		''
), (
	33,	'core'	,	'Geography',		'1'
), (
	33,	'core'	,	'History',		'1'
), (
	33,	'core'	,	'Maths',		'1'
), (
	33,	'optional',	'Music',		'1'
), (
	33,	'core'	,	'Physical Education',	'1'
), (
	33,	'core'	,	'Physics',		'1'
), (
	33,	'core'	,	'Religious Education',	'1'
), (
	33,	'core'	,	'Swimming',		'1'
), (
	34,	'core'	,	'Biology',		'2'
), (
	34,	'optional',	'Domestic Science',	'2'
), (
	34,	'core'	,	'English',		'2'
), (
	34,	'core'	,	'French',		'2'
), (
	34,	'core'	,	'Games',		''
), (
	34,	'core'	,	'History',		'2'
), (
	34,	'optional',	'Music',		'2'
), (
	34,	'core'	,	'Physical Education',	'2'
), (
	34,	'core'	,	'Physics',		'2'
), (
	34,	'core'	,	'Swimming',		'2'
), (
	34,	'optional',	'Technical Drawing',	'2'
), (
	35,	'optional',	'Art',			'2'
), (
	35,	'core'	,	'Chemistry',		'2'
), (
	35,	'optional',	'Domestic Science',	'2'
), (
	35,	'core'	,	'English',		'2'
), (
	35,	'core'	,	'Games',		''
), (
	35,	'core'	,	'Geography',		'2'
), (
	35,	'core'	,	'German',		'2'
), (
	35,	'core'	,	'Maths',		'2'
), (
	35,	'core'	,	'Physical Education',	'2'
), (
	35,	'core'	,	'Religious Education',	'2'
), (
	35,	'core'	,	'Swimming',		'2'
), (
	36,	'optional',	'Art',			'1'
), (
	36,	'core'	,	'Biology',		'2'
), (
	36,	'core'	,	'Chinese',		'2'
), (
	36,	'core'	,	'English',		'2'
), (
	36,	'core'	,	'Games',		''
), (
	36,	'core'	,	'Geography',		'2'
), (
	36,	'core'	,	'Maths',		'2'
), (
	36,	'optional',	'Music',		'2'
), (
	36,	'core'	,	'Physics',		'2'
), (
	36,	'core'	,	'Swimming',		'2'
), (
	36,	'optional',	'Technical Drawing',	'1'
), (
	37,	'core'	,	'Biology',		'2'
), (
	37,	'core'	,	'Chemistry',		'2'
), (
	37,	'core'	,	'English',		'2'
), (
	37,	'core'	,	'French',		'2'
), (
	37,	'core'	,	'History',		'2'
), (
	37,	'core'	,	'Maths',		'2'
), (
	37,	'optional',	'Music',		'2'
), (
	37,	'core'	,	'Physics',		'2'
), (
	37,	'core'	,	'Swimming',		'2'
), (
	37,	'optional',	'Woodwork',		'2'
), (
	38,	'core'	,	'Chinese',		'3'
), (
	38,	'optional',	'Domestic Science',	'3'
), (
	38,	'core'	,	'English',		'3'
), (
	38,	'core'	,	'Games',		''
), (
	38,	'core'	,	'History',		'3'
), (
	38,	'core'	,	'Maths',		'3'
), (
	38,	'core'	,	'Physics',		'3.A'
), (
	38,	'core'	,	'Swimming',		'3'
), (
	38,	'optional',	'Technical Drawing',	'1'
), (
	39,	'optional',	'Art',			'3'
), (
	39,	'core'	,	'Biology',		'3'
), (
	39,	'core'	,	'Chemistry',		'3'
), (
	39,	'optional',	'Domestic Science',	'2'
), (
	39,	'core'	,	'English',		'3'
), (
	39,	'core'	,	'German',		'3'
), (
	39,	'core'	,	'Maths',		'3'
), (
	39,	'core'	,	'Physical Education',	'3'
), (
	39,	'core'	,	'Swimming',		'3'
), (
	40,	'core'	,	'Biology',		'3'
), (
	40,	'core'	,	'English',		'3'
), (
	40,	'core'	,	'Geography',		'3'
), (
	40,	'core'	,	'Maths',		'3'
), (
	40,	'optional',	'Music',		'3'
), (
	40,	'core'	,	'Physical Education',	'3'
), (
	40,	'core'	,	'Physics',		'3.B'
), (
	40,	'core'	,	'Swimming',		'3'
), (
	40,	'optional',	'Technical Drawing',	'2'
), (
	40,	'optional',	'Woodwork',		'1'
);
UNLOCK TABLES;

