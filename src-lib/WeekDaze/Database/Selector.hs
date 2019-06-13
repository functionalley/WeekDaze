{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

	* Provides an interface for data-types which can construct themselves from a database.

	* Provides a convenient function to run a simple SQL-query.

	* Some common column-names are also exported.
-}

module WeekDaze.Database.Selector(
-- * Types
-- ** Type-synonyms
	ColumnName,
	TableName,
-- * Constants
	locationIdColumnName,
--	mySqlSetSeparator,
	projectIdColumnName,
	synchronisationIdColumnName,
	tablePrefix,
	teacherIdColumnName,
	timeslotIdColumnName,
	campusColumnName,
-- * Type-classes
	Selector(..),
-- * Functions
	queryConcurrently,
--	fromDatabaseConcurrently',
	round',
	fromMySqlSet,
	toMySqlSet,
	fromSqlFractional,
--	mkParameterisedSelection,
	prepare,
	select
) where

import qualified	Control.Concurrent
import qualified	Control.DeepSeq
import			Control.DeepSeq(($!!))
import qualified	Control.Monad
import qualified	Database.HDBC
import qualified	Data.Convertible
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Ratio
import qualified	Data.Set
import qualified	Data.Typeable

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

#ifdef PRINT_DB_QUERIES
import qualified	System.IO
#endif

-- | Synonym.
type ColumnName	= String

-- | Synonym.
type TableName	= String

-- | Database column-name.
locationIdColumnName :: ColumnName
locationIdColumnName		= "locationId"

-- | Database column-name.
projectIdColumnName :: ColumnName
projectIdColumnName		= "projectId"

-- | Database column-name.
synchronisationIdColumnName :: ColumnName
synchronisationIdColumnName	= "synchronisationId"

{- |
	* Database table-identifier.

	* Consistent use of this for all table-names, prevents a clash with SQL-keywords, without requiring table-names to be quoted.
-}
tablePrefix :: TableName
tablePrefix			= "tbl_"

-- | Database column-name.
teacherIdColumnName :: ColumnName
teacherIdColumnName		= "teacherId"

-- | Database column-name.
timeslotIdColumnName :: ColumnName
timeslotIdColumnName		= "timeslotId"

-- | Database column-name.
campusColumnName :: ColumnName
campusColumnName		= "campus"

{- |
	An interface to which data-types capable of constructing themselves from a database may conform.
	Since the database can contain many projects, & only one is required, the identity of the specific project must be defined.
-}
class Selector selector where
	-- | Reads an instance from the connected database.
	fromDatabase
		:: Database.HDBC.IConnection	connection
		=> connection			-- ^ Abstract database-connection.
		-> Database.HDBC.SqlValue	-- ^ The integral projectId.
		-> IO selector
	-- | Reads an instance from the connected database, using concurrent queries.
	fromDatabaseConcurrently
		:: Database.HDBC.IConnection	connection
		=> connection			-- ^ Abstract database-connection.
		-> Database.HDBC.SqlValue	-- ^ The integral projectId.
		-> IO selector
	fromDatabaseConcurrently	= fromDatabase	-- Default to serial query.

instance (
	Control.DeepSeq.NFData	a,
	Control.DeepSeq.NFData	b,
	Selector		a,
	Selector		b
 ) => Selector (a, b) where
	fromDatabase connection projectIdSql			= (,) <$> fromDatabase connection projectIdSql <*> fromDatabase connection projectIdSql
	fromDatabaseConcurrently connection projectIdSql	= do
		x	<- Control.Concurrent.newEmptyMVar
		y	<- Control.Concurrent.newEmptyMVar

		fromDatabaseConcurrently' connection projectIdSql x
		fromDatabaseConcurrently' connection projectIdSql y

		(,) <$> Control.Concurrent.takeMVar x <*> Control.Concurrent.takeMVar y

instance (
	Control.DeepSeq.NFData	a,
	Control.DeepSeq.NFData	b,
	Control.DeepSeq.NFData	c,
	Selector		a,
	Selector		b,
	Selector		c
 ) => Selector (a, b, c) where
	fromDatabase connection projectIdSql			= (,,) <$> fromDatabase connection projectIdSql <*> fromDatabase connection projectIdSql <*> fromDatabase connection projectIdSql
	fromDatabaseConcurrently connection projectIdSql	= do
		x	<- Control.Concurrent.newEmptyMVar
		y	<- Control.Concurrent.newEmptyMVar
		z	<- Control.Concurrent.newEmptyMVar

		fromDatabaseConcurrently' connection projectIdSql x
		fromDatabaseConcurrently' connection projectIdSql y
		fromDatabaseConcurrently' connection projectIdSql z

		(,,) <$> Control.Concurrent.takeMVar x <*> Control.Concurrent.takeMVar y <*> Control.Concurrent.takeMVar z

instance (
	Control.DeepSeq.NFData	a,
	Control.DeepSeq.NFData	b,
	Control.DeepSeq.NFData	c,
	Control.DeepSeq.NFData	d,
	Selector		a,
	Selector		b,
	Selector		c,
	Selector		d
 ) => Selector (a, b, c, d) where
	fromDatabase connection projectIdSql			= (,,,) <$> fromDatabase connection projectIdSql <*> fromDatabase connection projectIdSql <*> fromDatabase connection projectIdSql <*> fromDatabase connection projectIdSql
	fromDatabaseConcurrently connection projectIdSql	= do
		w	<- Control.Concurrent.newEmptyMVar
		x	<- Control.Concurrent.newEmptyMVar
		y	<- Control.Concurrent.newEmptyMVar
		z	<- Control.Concurrent.newEmptyMVar

		fromDatabaseConcurrently' connection projectIdSql w
		fromDatabaseConcurrently' connection projectIdSql x
		fromDatabaseConcurrently' connection projectIdSql y
		fromDatabaseConcurrently' connection projectIdSql z

		(,,,) <$> Control.Concurrent.takeMVar w <*> Control.Concurrent.takeMVar x <*> Control.Concurrent.takeMVar y <*> Control.Concurrent.takeMVar z

-- | Issue the specified database-query on a new thread.
queryConcurrently :: (
	Control.DeepSeq.NFData		selection,
	Database.HDBC.IConnection	connection
 )
	=> (connection -> Database.HDBC.SqlValue -> IO selection)	-- ^ The query.
	-> connection							-- ^ Abstract database-connection.
	-> Database.HDBC.SqlValue					-- ^ The integral projectId.
	-> Control.Concurrent.MVar selection				-- ^ Where to store the result.
	-> IO ()
queryConcurrently query connection projectIdSql mVar	= do
	connection'	<- Database.HDBC.clone connection	-- CAVEAT: from the HDBC-documentation; not all databases support more than one active statement for a single connection, therefore for maximum portability, use a different connection to the database for each simultaneous query you wish to use.

	Control.Monad.void . Control.Concurrent.forkIO $ query connection' projectIdSql >>= (Control.Concurrent.putMVar mVar $!!)	-- CAVEAT: force the thread to work by strictly evaluating the structure.

-- | Construct an arbitrary datum from a database-query issued on a new thread.
fromDatabaseConcurrently' :: (
	Control.DeepSeq.NFData		selector,
	Database.HDBC.IConnection	connection,
	Selector			selector
 )
	=> connection				-- ^ Abstract database-connection.
	-> Database.HDBC.SqlValue		-- ^ The integral projectId.
	-> Control.Concurrent.MVar selector	-- ^ Where to store the result.
	-> IO ()
fromDatabaseConcurrently'	= queryConcurrently fromDatabase

-- | Round any error introduced by conversion to a rational number.
round' :: RealFrac r => r -> Rational
round'	= (`Data.Ratio.approxRational` doublePrecisionEpsilon) where
	doublePrecisionEpsilon	= recip 2 ^ floatDigits (undefined :: Double)

{- |
	* Converts a /SqlValue/ to the specified fractional type.

	* CAVEAT: required to avoid any rounding-error in the floating-point number read from the database resulting in an inefficiently large 'Rational' number.
-}
fromSqlFractional :: (
	Data.Convertible.Convertible	Database.HDBC.SqlValue a,	-- Flexible context.
	Data.Typeable.Typeable		a,
	RealFrac			a
 )
	=> a		-- ^ Default value to use if the SQL-value is null.
	-> Database.HDBC.SqlValue
	-> a
fromSqlFractional defaultValue	= Data.Maybe.maybe defaultValue (
	\x -> if Data.Typeable.typeOf x == Data.Typeable.typeOf (undefined :: Data.Ratio.Rational)
		then realToFrac $ round' x
		else x
 ) . either (
	error . showString "WeekDaze.Database.Selector.fromSqlFractional:\tfailed to parse a value read from the database; " . show
 ) id . Database.HDBC.safeFromSql

-- | The separator used by MySql, when representing the members of a set.
mySqlSetSeparator :: Char
mySqlSetSeparator	= ','

-- | Constructs a list from a MySql set.
fromMySqlSet :: Read a => String -> [a]
fromMySqlSet s
	| null s		= []	-- Terminate.
	| null remainder	= [day]	-- Terminate.
	| otherwise		= day : fromMySqlSet (tail {-drop the separator-} remainder) {-recurse-}
	where
		(s', remainder)	= break (== mySqlSetSeparator) s
		day			= case reads s' of
			[(value, [])]	-> value
			_		-> error $ "WeekDaze.Database.Selector.fromMySqlSet:\tfailed to parse " ++ show s'

-- | Constructs a MySql set from a list.
toMySqlSet :: Show a => [a] -> String
toMySqlSet	= Data.List.intercalate [mySqlSetSeparator] . map show

-- N.B.: orphan instance.
instance (Ord a, Read a) => Data.Convertible.Convertible Database.HDBC.SqlValue (Data.Set.Set a {-MultiParamTypeClasses-}) where
	safeConvert	= fmap (Data.Set.fromList . fromMySqlSet) . Data.Convertible.safeConvert

{- |
	* Constructs a parameterised SQL-select query for the specified columns from the specified tables,
	from those rows whose columns exactly match the currently unspecified actual parameters.

	* CAVEAT: identifiers aren't delimited, since MySQL uses back-ticks rather than the standard double-quote;
	so identifiers can contain any white space or be SQL-keywords.
	If this presents a problem, then they can be pre-quoted before the call.

	* CAVEAT: this is an ad-hoc solution to the general problem of constructing an SQL-query,
	in that it only caters for the specific type of query required by this application.
-}
mkParameterisedSelection
	:: [ColumnName]	-- ^ Select columns from the matching rows.
	-> [TableName]	-- ^ Typically only a single table is referenced, but the facility exists to implicitly specify a /join/.
	-> [ColumnName]	-- ^ Select rows matching (by equality) values (currently represented by place-holders) of the specified (case-insensitive) column-names.
	-> String
mkParameterisedSelection columnNames tableNames rowConditions
	| null rowConditions	= showString selectFrom ";"
	| otherwise		= showString selectFrom . showString " WHERE " $ showString (
		Data.List.intercalate " AND " {-arbitrarily-} $ map (
			++ "=?"	-- Lacking any requirement for more sophistication, all parameters are matched using equality; sorry.
		) rowConditions
	) ";"
	where
		selectFrom :: String
		selectFrom	= showString "SELECT " . showString (separateIdentifiers columnNames) . showString " FROM " $ separateIdentifiers tableNames where
			separateIdentifiers :: [String] -> String
			separateIdentifiers	= Data.List.intercalate ","

-- | Composes a parameterised SQL-select query.
prepare
	:: Database.HDBC.IConnection connection
	=> connection	-- ^ Abstract database-connection.
	-> [ColumnName]	-- ^ Select the specified columns from matching rows.
	-> [TableName]	-- ^ Typically only a single table is referenced, but the facility exists to implicitly specify a /join/.
	-> [ColumnName]	-- ^ The formal parameters which will be subsequently be required to match corresponding actual parameters to filter rows.
	-> IO Database.HDBC.Statement
prepare connection columnNames tableNames formalParameters	= do
	statement	<- Database.HDBC.prepare connection $ mkParameterisedSelection columnNames tableNames formalParameters

#ifdef PRINT_DB_QUERIES
	System.IO.hPutStrLn System.IO.stderr . showString "INFO:\tDB-query=" $ shows (Database.HDBC.originalQuery statement) "."
#endif
	return {-to IO-monad-} statement

{- |
	* Prepares a parameterised SQL-select query, then executes it using the specified database-connection and actual parameters.

	* Because the SQL is parsed before replacing the formal parameters with actual parameters, it shouldn't be vulnerable to SQL-injection.
-}
select
	:: Database.HDBC.IConnection	connection
	=> connection					-- ^ Abstract database-connection.
	-> [ColumnName]					-- ^ Select the specified columns from matching rows.
	-> [TableName]					-- ^ Typically only a single table is referenced, but the facility exists to implicitly specify a /join/.
	-> [(ColumnName, Database.HDBC.SqlValue)]	-- ^ Select those rows matching the specified parameters.
	-> IO [[Database.HDBC.SqlValue]]
select connection columnNames tableNames parameters	= let
	(formalParameters, actualParameters)	= unzip parameters
 in do
	statement	<- prepare connection columnNames tableNames formalParameters
	0		<- Database.HDBC.execute statement actualParameters	-- For 'SELECT' queries, the number of rows modified is always zero.
	rows		<- Database.HDBC.fetchAllRows' statement		-- CAVEAT: the strict version of this call is necessary to ensure that a new query isn't executed before these rows have been read; else SQL-error "Commands out of sync; you can't run this command now".

#ifdef PRINT_DB_ROWS
	System.IO.hPutStrLn System.IO.stderr . showString "INFO:\tDB-rows=" $ shows rows "."
#endif
	return {-to IO-monad-} rows

