{-
	Copyright (C) 2013-2014 Dr. Alistair Ward

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

	* This is a group of /student-bodies/, merged for the duration of a single booking in the timetable, or the bookings required for a single /course/.

	* In contrast to a /student-body/, the members of a /student-class/ don't have identical 'Data.Student.Profile's,
	but their requirements are sufficiently similar to permit them to study a /course/ together.

	* Temporarily merging /student-bodies/, better utilises the available /teacher/s & /location/s,
	by increasing the pool of /student/s available to be taught simultaneously.
-}

module WeekDaze.Aggregate.StudentClass(
-- * Types
-- ** Type-synonyms
	StudentClass,
	MnemonicSeparator,
-- * Constants
	tag,
-- * Functions
	merge,
	getSize,
-- ** Translation
	toHtml
) where

import			Control.Arrow((***))
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import			Text.XHtml.Strict((<<))
import qualified	WeekDaze.Aggregate.StudentBody	as Aggregate.StudentBody
import qualified	WeekDaze.Size			as Size
import qualified	WeekDaze.Text.XHTML		as Text.XHTML

-- | Used to qualify XML & CSS.
tag :: String
tag	= "studentClass"

{- |
	Whilst a /student-body/ represents a group with identical requirements,
	when one or more /student-bodies/ have similar requirements for a single /subject/,
	they can be temporarily merged to form a /class/.
-}
type StudentClass	= Data.Set.Set Aggregate.StudentBody.StudentBody

-- | Used to separate /mnemonic/s, when merging a /student-class/ into a /student-body/.
type MnemonicSeparator	= String

{- |
	* Remove the boundaries separating the groups of /student/s within a class, to form a single /student-body/.

	* The mnemonics for the constituent /student-bodies/ are concatenated to form a composite mnemonic for the new /student-body/.

	* CAVEAT: any /student-Ids/ common to more than one /student-body/ within the /student-class/, will be merged.

	* CAVEAT: a null set, will result in a null mnemonic, which will be rejected by 'Aggregate.StudentBody.mkStudentBody'.
-}
merge :: MnemonicSeparator -> StudentClass -> Aggregate.StudentBody.StudentBody
merge mnemonicSeparator	= uncurry Aggregate.StudentBody.mkStudentBody . (
	Data.List.intercalate mnemonicSeparator *** Data.Set.unions {-members-}
 ) . unzip . map Aggregate.StudentBody.toPair . Data.Set.toList

{- |
	* Counts the number of /student/s in a class.

	* CAVEAT: assumes that the /student-bodies/, from which the class is composed, are distinct.
-}
getSize :: Data.Foldable.Foldable foldable => foldable Aggregate.StudentBody.StudentBody -> Size.NStudents
getSize	= Data.Foldable.foldr ((+) . Aggregate.StudentBody.getSize) 0

-- | Convert to XHTML.
toHtml :: StudentClass -> Text.XHtml.Strict.Html
toHtml	= (Text.XHTML.mkXHTMLSpan tag <<) . Data.List.intersperse (Text.XHtml.Strict.stringToHtml ", ") . map Text.XHtml.Strict.toHtml . Data.Set.toList

