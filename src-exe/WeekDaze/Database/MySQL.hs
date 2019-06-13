{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2014-2015 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Encapsulates the module "MySQL".
-}

module WeekDaze.Database.MySQL(
	MySQL.withRTSSignalsBlocked,	-- Re-export to shield the caller from the source module.
-- * Constants
	defaultHost,
	defaultPort,
-- * Functions
	connect
) where

import qualified	Database.HDBC.MySQL	as MySQL	-- CAVEAT: there may be issues compiling the required package in Windows , where typically one would use "HDBC.ODBC".
import qualified	Data.List
--import qualified	System.IO

-- | The default host-name.
defaultHost :: String
defaultHost	= MySQL.mysqlHost MySQL.defaultMySQLConnectInfo

-- | The default port on which to connect.
defaultPort :: Int
defaultPort	= MySQL.mysqlPort MySQL.defaultMySQLConnectInfo

instance Show MySQL.MySQLConnectInfo where
	showsPrec _ MySQL.MySQLConnectInfo {
		MySQL.mysqlHost		= host,
		MySQL.mysqlPort		= port,
		MySQL.mysqlUser		= user,
--		MySQL.mysqlPassword	= password,
		MySQL.mysqlDatabase	= database
	} = foldr (.) (
		showChar ')'	-- Initial value.
	 ) . Data.List.intersperse (
		showString ", "	-- Separator.
	 ) $ map (
		\(k, v)	-> showString k . v
	 ) [
		(
			"(host=",	shows host
		), (
			"port=",	shows port
		), (
			"user=",	shows user
{-
		), (
			"password=",	shows password
-}
		), (
			"database=",	shows database
		)
	 ]

-- | Connects to the specified MySQL-database.
connect
	:: String	-- ^ Data-server's Host-name.
	-> Int		-- ^ Port.
	-> String	-- ^ User-name on Data-server.
	-> String	-- ^ Password for Data-server.
	-> String	-- ^ Database-name.
	-> IO MySQL.Connection
connect hostName port dataServerUserName dataServerPassword databaseName	= do
	let mySQLConnectInfo	= MySQL.defaultMySQLConnectInfo {
		MySQL.mysqlHost		= hostName,
		MySQL.mysqlPort		= port,
		MySQL.mysqlUser		= dataServerUserName,
		MySQL.mysqlPassword	= dataServerPassword,
		MySQL.mysqlDatabase	= databaseName
	}

--	System.IO.hPutStrLn System.IO.stderr $ "INFO:\tattempting connection to the data-server; " ++ show mySQLConnectInfo ++ "."	-- CAVEAT: the executable may be running on a web-server, so don't reveal connection-details to web-clients.

	MySQL.connectMySQL mySQLConnectInfo

