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

 [@DESCRIPTION@]	Defines the /resource/s required for a /lesson/, from the perspective of a /location/.
-}

module WeekDaze.LocationView.LessonResourceIds(
-- * Types
-- ** Data-types
	LessonResourceIds(
--		MkLessonResourceIds,
		getStudentClass,
		getTeacherId
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
tag	= "locationViewLessonResourceIds"

-- | Composed from both the 'Aggregate.StudentClass.StudentClass' & 'teacherId'.
data LessonResourceIds teacherId	= MkLessonResourceIds {
	getStudentClass	:: Aggregate.StudentClass.StudentClass,	-- ^ The class of /student/s to be taught.
	getTeacherId	:: teacherId				-- ^ The identifier for the /teacher/ instructing this /class/.
} deriving (Eq, Show)

instance Text.XHtml.Strict.HTML	teacherId => Text.XHtml.Strict.HTML (LessonResourceIds teacherId) where
	toHtml	= (
		\(studentClass, teacherId) -> Text.XHtml.Strict.li << (studentClass +++ '.') +++ Text.XHtml.Strict.li << (teacherId +++ '.')
	 ) . (
		Aggregate.StudentClass.toHtml . getStudentClass &&& getTeacherId
	 )

instance HXT.XmlPickler teacherId => HXT.XmlPickler (LessonResourceIds teacherId) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		uncurry MkLessonResourceIds,		-- Construct from a Pair.
		getStudentClass &&& getTeacherId	-- Deconstruct to a Pair.
	 ) $ (
		HXT.xpElem Aggregate.StudentClass.tag . HXT.xpWrap (
			Data.Set.fromList,	-- Construct from a List.
			Data.Set.toList		-- Deconstruct to a List.
		) $ HXT.xpList1 {-can't be null-} HXT.xpickle {-StudentBody-}
	 ) `HXT.xpPair` HXT.xpickle

instance Eq teacherId => Model.ResourceUser.ResourceUser (LessonResourceIds teacherId) where
	areIndependent l r	= Model.ResourceUser.areIndependent (getStudentClass l) (getStudentClass r) && getTeacherId l /= getTeacherId r

instance Show teacherId => ToolShed.SelfValidate.SelfValidator (LessonResourceIds teacherId) where
	getErrors lessonResourceIds	= ToolShed.SelfValidate.extractErrors [
		(
			Data.Set.null $ getStudentClass lessonResourceIds,
			"a class should be composed from one or more student-bodies; " ++ show lessonResourceIds
		) -- Pair.
	 ]

-- | Smart constructor.
mkLessonResourceIds
	:: Show teacherId
	=> Aggregate.StudentClass.StudentClass	-- ^ The class of /student/s to be instructed.
	-> teacherId				-- ^ The identifier of the /teacher/ taking the class.
	-> LessonResourceIds teacherId
mkLessonResourceIds studentClass teacherId
	| ToolShed.SelfValidate.isValid lessonResourceIds	= lessonResourceIds
	| otherwise						= error $ "WeekDaze.LocationView.LessonResourceIds.mkLessonResourceIds:\t" ++ ToolShed.SelfValidate.getFirstError lessonResourceIds ++ "."
	where
		lessonResourceIds	= MkLessonResourceIds studentClass teacherId

-- | Convert from the /student/-view to a /location/-view.
fromStudentView :: Show teacherId => Aggregate.StudentClass.StudentClass -> StudentView.LessonResourceIds.LessonResourceIds locationId teacherId -> LessonResourceIds teacherId
fromStudentView studentClass	= mkLessonResourceIds studentClass . StudentView.LessonResourceIds.getTeacherId

-- | Convert from the /location/-view to a /student/-view.
toStudentView :: locationId -> LessonResourceIds teacherId -> StudentView.LessonResourceIds.LessonResourceIds locationId teacherId
toStudentView locationId resourceIds	= StudentView.LessonResourceIds.MkLessonResourceIds {
	StudentView.LessonResourceIds.getLocationId	= locationId,
	StudentView.LessonResourceIds.getTeacherId	= getTeacherId resourceIds
}

