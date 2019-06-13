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

 [@DESCRIPTION@]	Defines the /resource/s required for a /lesson/, from the perspective of a /teacher/.
-}

module WeekDaze.TeacherView.LessonResourceIds(
-- * Types
-- ** Data-types
	LessonResourceIds(
--		MkLessonResourceIds,
		getLocationId,
		getStudentClass
	),
-- * Constants
--	tag,
-- * Functions
-- ** Constructor
	mkLessonResourceIds,
-- ** Translation
	fromStudentView,
	toStudentView
) where

import			Control.Arrow((&&&))
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import qualified	Text.XML.HXT.Arrow.Pickle		as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Aggregate.StudentClass		as Aggregate.StudentClass
import qualified	WeekDaze.Model.ResourceUser		as Model.ResourceUser
import qualified	WeekDaze.StudentView.LessonResourceIds	as StudentView.LessonResourceIds
import			Text.XHtml.Strict((+++), (<<))

-- | Used to qualify XML.
tag :: String
tag	= "teacherViewLessonResourceIds"

-- | Composed from both the 'locationId' & 'Aggregate.StudentClass.StudentClass'.
data LessonResourceIds locationId	= MkLessonResourceIds {
	getLocationId	:: locationId,				-- ^ The identifier for the /location/ at which the /class/ will be held.
	getStudentClass	:: Aggregate.StudentClass.StudentClass	-- ^ The /class/ of /student/s to be taught.
} deriving (Eq, Ord, Show)

instance Text.XHtml.Strict.HTML locationId => Text.XHtml.Strict.HTML (LessonResourceIds locationId) where
	toHtml	= (
		\(locationId, studentClass) -> Text.XHtml.Strict.li << (locationId +++ '.') +++ Text.XHtml.Strict.li << (studentClass +++ '.')
	 ) . (
		getLocationId &&& Aggregate.StudentClass.toHtml . getStudentClass
	 )

instance HXT.XmlPickler locationId => HXT.XmlPickler (LessonResourceIds locationId) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		uncurry MkLessonResourceIds,		-- Construct from a Pair.
		getLocationId &&& getStudentClass	-- Deconstruct to a Pair.
	 ) $ HXT.xpickle `HXT.xpPair` (
		HXT.xpElem Aggregate.StudentClass.tag . HXT.xpWrap (
			Data.Set.fromList,	-- Construct from a List.
			Data.Set.toList		-- Deconstruct to a List.
		) $ HXT.xpList1 {-can't be null-} HXT.xpickle {-StudentBody-}
	 )

instance Eq locationId => Model.ResourceUser.ResourceUser (LessonResourceIds locationId) where
	areIndependent l r	= getLocationId l /= getLocationId r && Model.ResourceUser.areIndependent (getStudentClass l) (getStudentClass r)

instance Show locationId => ToolShed.SelfValidate.SelfValidator (LessonResourceIds locationId) where
	getErrors lessonResourceIds	= ToolShed.SelfValidate.extractErrors [
		(Data.Set.null $ getStudentClass lessonResourceIds,	"a class should be composed from one or more student-bodies; " ++ show lessonResourceIds)
	 ]

-- | Smart constructor.
mkLessonResourceIds
	:: Show locationId
	=> locationId				-- ^ The identifier of the /location/ at which the /student-class/ is to be taught.
	-> Aggregate.StudentClass.StudentClass	-- ^ The class of /student/s to be instructed.
	-> LessonResourceIds locationId
mkLessonResourceIds locationId studentClass
	| ToolShed.SelfValidate.isValid lessonResourceIds	= lessonResourceIds
	| otherwise						= error $ "WeekDaze.TeacherView.LessonResourceIds.mkLessonResourceIds:\t" ++ ToolShed.SelfValidate.getFirstError lessonResourceIds ++ "."
	where
		lessonResourceIds	= MkLessonResourceIds locationId studentClass

-- | Convert from the /student/-view to a /teacher/-view.
fromStudentView :: Show locationId => StudentView.LessonResourceIds.LessonResourceIds locationId teacherId -> Aggregate.StudentClass.StudentClass -> LessonResourceIds locationId
fromStudentView resourceIds	= mkLessonResourceIds (StudentView.LessonResourceIds.getLocationId resourceIds)

-- | Convert from the /teacher/-view to a /student/-view.
toStudentView :: teacherId -> LessonResourceIds locationId -> StudentView.LessonResourceIds.LessonResourceIds locationId teacherId
toStudentView teacherId resourceIds	= StudentView.LessonResourceIds.MkLessonResourceIds {
	StudentView.LessonResourceIds.getLocationId	= getLocationId resourceIds,
	StudentView.LessonResourceIds.getTeacherId	= teacherId
}

