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

	* Individual /students/ can be grouped into a /body/ composed from those with identical /profiles/.

	* This reduces the number of personal timetables that must ultimately be constructed.

	* This is a relationship persists for the duration of a configuration-file, & in this respect differs from a /class/ of /student/s,
	which is a transient relationship formed for the duration of a single booking in the timetable, or perhaps for the bookings in a single /course/.

 [@TODO@]

	* The requirement to specify all the /student-Ids/, could be replaced with merely an anonymous head-count,
	since the application make very little use of this data.
-}

module WeekDaze.Aggregate.StudentBody(
-- * Types
-- ** Type-synonyms
	Mnemonic,
--	Members,
	StudentBody(
--		MkStudentBody,
		getMnemonic,
		getStudentIds
	),
-- * Constants
--	tag,
	mnemonicTag,
--	studentTag,
	studentIdTag,
-- * Functions
	getSize,
	toPair,
-- ** Constructor
	mkStudentBody
) where

import qualified	Control.Arrow
import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Data.List
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import			Text.XHtml.Strict((<<), (!))
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Data.Student		as Data.Student
import qualified	WeekDaze.Size			as Size

-- | Used to qualify CSS & XML.
tag :: String
tag		= "studentBody"

-- | Used to qualify SQL & XML.
mnemonicTag :: String
mnemonicTag	= "mnemonic"

-- | Used to qualify XML.
studentTag :: String
studentTag	= "student"

-- | Used to qualify SQL & XML.
studentIdTag :: String
studentIdTag	= "studentId"

-- | The type of the mnemonic by which a /student-body/ is known.
type Mnemonic	= String

-- | The type the member-/student/s.
type Members	= Data.Set.Set Data.Student.Id

{- |
	* A group of /student/s with identical /profile/s; i.e. with identical requirements.

	* Because this data-type forms part of the results for the application, & can become excessively verbose, a concise /mnemonic/ is provided.
-}
data StudentBody	= MkStudentBody {
	getMnemonic	:: Mnemonic,	-- ^ A mnemonic for the whole /student-body/; e.g. "2/B" or "Remedial".
	getStudentIds	:: Members	-- ^ The identifiers of the member-/student/s.
}

-- 'compare' is a performance-hotspot, & therefore has a specialised implementation.
instance Ord StudentBody where
	MkStudentBody mnemonicL _ `compare` MkStudentBody mnemonicR _	= mnemonicL `compare` mnemonicR

-- CAVEAT: because of the specialised 'Ord'-implementation, a compatible specialised 'Eq'-implementation is required; otherwise two unequal StudentBodies may be neither '<' nor '>'.
instance Eq StudentBody where
	MkStudentBody mnemonicL _ == MkStudentBody mnemonicR _	= mnemonicL == mnemonicR

instance Read StudentBody where
	readsPrec _	= map (Control.Arrow.first $ uncurry mkStudentBody . Control.Arrow.second Data.Set.fromList) . reads

-- This instance-declaration hides the internal structure.
instance Show StudentBody where
	showsPrec _	= shows . Control.Arrow.second Data.Set.toList . toPair

instance Text.XHtml.Strict.HTML StudentBody where
	toHtml studentBody	= Text.XHtml.Strict.thespan ! [
		Text.XHtml.Strict.theclass tag,
		Text.XHtml.Strict.title . Data.List.intercalate ", " . map show {-display double-quotes-} . Data.Set.toList $ getStudentIds studentBody
	 ] << getMnemonic studentBody

instance ToolShed.SelfValidate.SelfValidator StudentBody where
	getErrors studentBody	= ToolShed.SelfValidate.extractErrors [
		(null $ getMnemonic studentBody,		"null mnemonic for student-ids; " ++ show (getStudentIds studentBody)),
		(Data.Set.null $ getStudentIds studentBody,	"zero student-ids are members of the student-body of mnemonic " ++ show (getMnemonic studentBody))
	 ]

instance HXT.XmlPickler StudentBody where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		uncurry mkStudentBody . Control.Arrow.second Data.Set.fromList,	-- Construct from a Pair.
		Control.Arrow.second Data.Set.toList . toPair			-- Deconstruct to a Pair.
	 ) $ HXT.xpTextAttr mnemonicTag {-can't be null-} `HXT.xpPair` (
		HXT.xpList1 {-can't be null-} . HXT.xpElem studentTag $ HXT.xpTextAttr studentIdTag {-can't be null-}
	 )

instance Control.DeepSeq.NFData StudentBody where
	rnf	= Control.DeepSeq.rnf . toPair

-- | Smart constructor.
mkStudentBody :: Mnemonic -> Members -> StudentBody
mkStudentBody mnemonic students
	| ToolShed.SelfValidate.isValid studentBody	= studentBody
	| otherwise					= error $ "WeekDaze.Aggregate.StudentBody.mkStudentBody:\t" ++ ToolShed.SelfValidate.getFirstError studentBody ++ "."
	where
		studentBody	= MkStudentBody	mnemonic students

-- | Deconstruct.
toPair :: StudentBody -> (Mnemonic, Members)
toPair	= getMnemonic &&& getStudentIds

-- | The number of /student/s in the body.
getSize :: StudentBody -> Size.NStudents
getSize	= Data.Set.size . getStudentIds

