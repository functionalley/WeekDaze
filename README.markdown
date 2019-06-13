# **WeekDaze**

[![Build Status](https://travis-ci.org/functionalley/WeekDaze.svg?branch=master)](https://travis-ci.org/functionalley/WeekDaze)
[![Hackage](https://img.shields.io/hackage/v/weekdaze.svg)](https://hackage.haskell.org/package/weekdaze)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Haskell](https://b.repl.ca/v1/language-haskell-yellow.png)](https://haskell.org)

This is "**WeekDaze**", an application which searches for solutions to the [school-timetable](https://en.wikipedia.org/wiki/School_timetable) problem.

It can read problem-specifications either from a relational database or from an **XML**-file.

## Installation

* It can be built and installed using [Cabal](https://www.haskell.org/cabal/users-guide/installing-packages.html).
* Configuration can be read by either from a local [XML](https://en.wikipedia.org/wiki/XML)-file using the Haskell package [hxt](https://hackage.haskell.org/package/hxt),
	or by establishing database-connectivity using one or more of the database-backend Haskell packages "[HDBC-odbc](https://hackage.haskell.org/package/HDBC-odbc)" & "[HDBC-mysql](https://hackage.haskell.org/package/HDBC-mysql)", & this affects how the product should be built.
* In order to connect to an arbitrary type of [RDBMS](https://en.wikipedia.org/wiki/Relational_database_management_system), **HDBC-odbc** should be installed,
	but when connecting specifically to [MySQL](https://www.mysql.com/), one can alternatively install **HDBC-mysql** (which is slower).
* Before building these database-backend Haskell packages on GNU/Linux the following prerequisites must be observed:
	+ **HDBC-odbc**:	requires the development version of the package "[unixODBC](https://www.unixodbc.org/)".
	+ **HDBC-mysql**:	requires either "**libmariadb-devel**" or the development version of the package "**libmysqlclient**".
		CAVEAT: **HDBC-mysql-0.6.6.1** is rather picky about the versions of other Haskell packages.
* Before using ODBC to establish database-connectivity:
	+ An appropriate driver for the type of RDBMS, e.g. [MyODBC-unixODBC](https://en.wikipedia.org/wiki/MySQL_Connector/ODBC) for **MySQL**, must be installed.
	+ A system-wide configuration-file "**odbcinst.ini**" must be created to define the above database-driver,
		& a configuration-file "**.odbc.ini**" must be created to define the available [DSN](https://en.wikipedia.org/wiki/Data_source_name)s.
		These files are described for a various types of DBMS in [odbcinst](https://www.unixodbc.org/odbcinst.html) & for **MySQL** in [MySQL Connector/ODBC](https://dev.mysql.com/doc/connector-odbc/en/) (examples for the latter case are included in this package).
* An initial **MySQL**-database can be constructed using the packaged SQL-files.

## Documentation

The documentation is in "**man/**".

## Examples

The directory "**xml/**" contains example-configurations and the directory "**xhtml/**" contains the corresponding solutions.
The packaged SQL-files contain the same example-configurations.

## License

For information on copying and distributing this package, see the file "**LICENSE**" in this directory.

## Bug-reporting

Bug-reports should be emailed to <weekdaze@functionalley.com>.

## Testing

The test-suite can be run using:

    cabal configure --enable-tests;
    cabal build;
    cabal test --show-details=always;

## Author

This application is written and maintained by Dr. Alistair Ward.

