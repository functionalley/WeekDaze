{-
	Copyright (C) 2015 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Supports ODBC-connection to a DBMS.
-}
module WeekDaze.Database.ODBC(
-- * Functions
	buildConnectionString
) where

import qualified	Data.List

{- |
	* Constructs an ODBC connection-string according to the syntax defined in <http://www.unixodbc.org/odbcinst.html> or <https://dev.mysql.com/doc/connector-odbc/en/>.

	* CAVEAT: despite ODBC's aim of abstracting the connection-process, the field-names defined here are MySQL-specific.
-}
buildConnectionString
	:: Maybe String	-- ^ The optional host-name on which the data-server is running.
	-> Maybe Int	-- ^ The optional port on which the data-server is listening.
	-> Maybe String	-- ^ The optional user-name on the above data-server.
	-> Maybe String	-- ^ The optional password by which to authenticate with the above data-server.
	-> Maybe String	-- ^ The optional database-name.
	-> String	-- ^ A DSN by which to select the remaining connection-parameters from one's previously filed data-sources; e.g. from "~/.odbc.ini".
	-> String
buildConnectionString maybeDataServerName maybeDataServerPort maybeDataServerUserName maybeDataServerPassword maybeDatabaseName dsn	= Data.List.intercalate ";" [
	showString key $ showChar '=' value | (key, Just value) <- [
		(
			"SERVER",	maybeDataServerName	-- PostgreSQL uses 'Servername' here.
		), (
			"PORT",		fmap show maybeDataServerPort
		), (
			"UID",		maybeDataServerUserName	-- AKA 'User'. PostgreSQL uses 'UserName' here.
		), (
			"PASSWORD",	maybeDataServerPassword
		), (
			"DATABASE",	maybeDatabaseName
		), (
			"DSN",		Just dsn
		)
	]
 ] -- List-comprehension.

