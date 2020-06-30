# Copyright (C) 2015 Dr. Alistair Ward
#
# This file is part of WeekDaze.
#
# WeekDaze is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# WeekDaze is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with WeekDaze.  If not, see <https://www.gnu.org/licenses/>.

%define package		%name-%version
%define tarBall		%package.tar.gz
%define _sharedir	%prefix/share
%define _datadir	%_sharedir/%name
%define _docdir		%_sharedir/doc/%name
%define _mandir		%_sharedir/man

Summary:	Attempts to find a timetable satisfying the configured requirements.
Name:		weekdaze
Version:	0.0.0.2
Release:	1
License:	GPLv3
# From '/usr/share/doc/packages/rpm/GROUPS'.
Group:		Applications/Productivity
# Source0:	https://functionalley.com/Downloads/sdist/%tarBall
URL:		https://functionalley.com/WeekDaze/weekdaze.html
Prefix:		/usr
BuildRequires:	haskell-platform

%description
A command-line application, which searches for a school-timetable matching the requirements defined in either an XML configuration-file or a SQL-database. The process is non-interactive; if the final result is inappropriate, then the configuration must be amended and the process re-started (regrettably a solution for all the requirements can neither be guaranteed to be found nor even to exist).
The solution is evolved randomly, and so the quality of the solution depends on the available time.
The result is presented from different perspectives; the students, the teachers, and the locations; and rendered in XHTML.

%prep
# N.B.: CWD has changed to %_builddir
echo 'package="%package", prefix="%prefix", _builddir="%_builddir", buildroot="%buildroot"'
(cd $OLDPWD && cabal sdist) && tar -zxf $OLDPWD/dist/%tarBall	# Make a source-distribution & unpack it into the build-directory.
cd '%package/' && cabal configure --user --prefix='%prefix' --datadir='%_datadir' --datasubdir='' --docdir='%_docdir'	# Tell cabal to use the user's personal package-database, to generate an appropriate "Paths" module, & where to place the documentation.

%build
cd '%package/' && cabal build	# Descend into the unpacked source-distribution and build according to the previously established configuration.

%install
cd '%package/'	# Descend into the build-directory.
cabal copy --destdir=%buildroot	# Install the built package in the target-directory.
mkdir -p -- '%buildroot%_docdir' && mv 'changelog.markdown' 'copyright' 'README.markdown' '%buildroot%_docdir/'	# 'LICENSE' has already been copied by cabal.
mkdir -p -- '%buildroot%_mandir' && mv man/man[15] '%buildroot%_mandir/'
rm -rf -- '%buildroot%prefix/lib/'	# The library isn't a deliverable.

%clean
rm -rf -- '%_builddir/%package/' '%buildroot/'	# Only the '.rpm' is required.

%files
%attr(0755, root, root)		%prefix/bin/%name
%attr(0644, root, root)		%_datadir/css/%name.css
%attr(0644, root, root)		%_datadir/dtd/%name.dtd
%attr(0644, root, root)		%_datadir/dtd/locationViewTimetable.dtd
%attr(0644, root, root)		%_datadir/dtd/studentViewTimetable.dtd
%attr(0644, root, root)		%_datadir/dtd/teacherViewTimetable.dtd
%attr(0644, root, root)		%_datadir/images/*.pdf
%attr(0644, root, root)		%_datadir/images/*.png
%attr(0644, root, root)		%_datadir/sql/MySQL/%{name}Create.sql
%attr(0644, root, root)		%_datadir/sql/MySQL/%{name}Examples.sql
%attr(0644, root, root)		%_datadir/sql/MySQL/%{name}Triggers.sql
%attr(0644, root, root)		%_datadir/sql/MySQL/odbc.ini
%attr(0644, root, root)		%_datadir/sql/MySQL/odbcinst.ini
%attr(0644, root, root)		%_datadir/xhtml/*.xhtml
%attr(0644, root, root)		%_datadir/xml/*.xml
%attr(0644, root, root) %doc	%_docdir/changelog.markdown
%attr(0644, root, root)	%doc	%_docdir/copyright
%attr(0644, root, root)	%doc	%_docdir/LICENSE
%attr(0644, root, root)	%doc	%_docdir/README.markdown
%attr(0644, root, root) %doc	%_mandir/man1/%name.1.gz
%attr(0644, root, root) %doc	%_mandir/man5/%name.5.gz

%changelog
* Thu Jul 04 2013	Alistair Ward	<weekdaze@functionalley.com>	0.0.0.1-1
First cut.

