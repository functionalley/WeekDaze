{-# LANGUAGE CPP, FlexibleContexts #-}
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

	* Describes a /subject/, in terms of the /topic/, & the /level/ at which it is being taught.

	* The /level/ may correspond to an academic year, but may be a finer-grain value as required to model /topic/-specific streaming.

	* A non-academic /subject/ like cross-country running, may not have a concept of multiple /level/s,
	but since it doesn't have zero but rather one /level/, the concept exists for all /subject/s.
-}

module WeekDaze.Data.Subject(
-- * Types
-- ** Type-synonyms
	Topic,
	Knowledge,
-- ** Data-types
	Subject(
--		MkSubject,
		getTopic,
		getLevel
	),
-- * Constants
	tag,
	levelTag,
	topicTag,
-- * Functions
-- ** Constructors
#ifdef USE_HDBC
	mkSubjectFromSql,
#endif
	mkSubject
) where

import qualified	Control.Arrow
import			Control.Arrow((&&&))
import qualified	Control.DeepSeq
import qualified	Data.Set
import qualified	Text.XHtml.Strict
import			Text.XHtml.Strict((+++), (<<))
import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import qualified	ToolShed.SelfValidate
import qualified	WeekDaze.Text.XHTML		as Text.XHTML

#ifdef USE_HDBC
import qualified	Data.Convertible
import qualified	Data.Maybe
import qualified	Database.HDBC

-- | Construct from two database-values.
mkSubjectFromSql
	:: (Data.Convertible.Convertible Database.HDBC.SqlValue level, Show level)	-- Flexible context.
	=> Database.HDBC.SqlValue	-- ^ Topic.
	-> Database.HDBC.SqlValue	-- ^ Level.
	-> Subject level
mkSubjectFromSql topicSql	= mkSubject (
	Data.Maybe.fromMaybe (
		error $ "WeekDaze.Data.Subject.mkSubjectFromSql:\tnull " ++ show topicTag ++ "."
	) $ Database.HDBC.fromSql topicSql
 ) . Data.Maybe.fromMaybe (
	error $ "WeekDaze.Data.Subject.mkSubjectFromSql:\tnull " ++ show levelTag ++ "."
 ) . Database.HDBC.fromSql
#endif /* USE_HDBC */

-- | Used to qualify CSS & XML.
tag :: String
tag		= "subject"

-- | Used to qualify CSS, SQL & XML.
levelTag :: String
levelTag	= "level"

-- | Used to qualify CSS, SQL & XML.
topicTag :: String
topicTag	= "topic"

-- | The type of the /topic/ of study.
type Topic	= String

-- | The subject which is being either taught or learned.
data Subject level	= MkSubject {
	getTopic	:: Topic,	-- ^ The /topic/ of study, which would typically be something like /Maths/ or /English/, but it could be something non-academic, like /Games/; but it shouldn't be either /Assembly/ or /Lunch/, since these aren't required to have exactly one /teacher/ in attendance.
	getLevel	:: level	-- ^ The /level/ at which this /topic/ is being taught; which may be merely the academic year, or a finer-grain concept representing a /topic/-specific stream.
} deriving (Eq, Ord)

instance (Read level, Show level) => Read (Subject level) where
	readsPrec _	= map (Control.Arrow.first $ uncurry mkSubject) . reads

instance Show level => Show (Subject level) where
	showsPrec _	= shows . (getTopic &&& getLevel)	-- Hides the constructor & accessors.

instance Text.XHtml.Strict.HTML level => Text.XHtml.Strict.HTML (Subject level) where
	toHtml subject	= Text.XHTML.mkXHTMLSpan tag << (
		Text.XHTML.mkXHTMLSpan topicTag << getTopic subject +++ Text.XHtml.Strict.spaceHtml {-level may be null, so make separator invisible-} +++ getLevel subject
	 )

instance (HXT.XmlPickler level, Show level) => HXT.XmlPickler (Subject level) where
	xpickle	= HXT.xpElem tag . HXT.xpWrap (
		uncurry mkSubject,	-- Construct from a Pair.
		getTopic &&& getLevel	-- Deconstruct to a Pair.
	 ) $ HXT.xpTextAttr topicTag {-can't be null-} `HXT.xpPair` HXT.xpickle {-Level-}

instance Control.DeepSeq.NFData level => Control.DeepSeq.NFData (Subject level) where
	rnf	= Control.DeepSeq.rnf . (getTopic &&& getLevel)

instance Show level => ToolShed.SelfValidate.SelfValidator (Subject level) where
	getErrors subject	= ToolShed.SelfValidate.extractErrors [(null $ getTopic subject, "null " ++ show topicTag ++ "; " ++ show subject)]

-- | Smart constructor.
mkSubject :: Show level => Topic -> level -> Subject level
mkSubject topic level
	| ToolShed.SelfValidate.isValid subject	= subject
	| otherwise				= error $ "WeekDaze.Data.Subject.mkSubject:\t" ++ ToolShed.SelfValidate.getFirstError subject ++ "."
	where
		subject	= MkSubject topic level

-- | A collection of distinct 'Subject's.
type Knowledge level	= Data.Set.Set (Subject level)

