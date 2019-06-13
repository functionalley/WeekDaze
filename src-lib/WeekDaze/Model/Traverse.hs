{-# LANGUAGE ScopedTypeVariables #-}
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

	* Provides the sequence of coordinates, required to traverse the conceptually /3-D/-/timetable/, in the required order.

	* Defines the type of an unspecified function, which can be folded over these coordinates, to sequentially receive any /lesson/-definitions from the /timetable/.
-}

module WeekDaze.Model.Traverse(
-- * Functions
	generateRasterCoordinates
) where

import			Control.Arrow((&&&))
import qualified	Data.Array.IArray
import			Data.Array.IArray((!))
import qualified	Data.Maybe
import qualified	WeekDaze.Model.TimetableAxis		as Model.TimetableAxis
import qualified	WeekDaze.Model.TimetableAxisTraversal	as Model.TimetableAxisTraversal
import qualified	WeekDaze.Model.TimetableAxisTriple	as Model.TimetableAxisTriple
import qualified	WeekDaze.Model.TimetableCoordinates	as Model.TimetableCoordinates
import qualified	WeekDaze.Temporal.Day			as Temporal.Day
import qualified	WeekDaze.Temporal.Time			as Temporal.Time

{- |
	* Generate the 'Model.TimetableCoordinates.Vector' defining a raster-scan through the /time-slot/'s in a /timetable/.

	* The first axis specified, changes the least frequently during the raster-scan,
	& the last specified, changes the most frequently.
-}
generateRasterCoordinates :: forall observerId timeslotId.
	Model.TimetableAxisTriple.Axes	-- ^ Defines the order & sense, in which the axes of a /timetable/ are traversed.
	-> [observerId]				-- ^ Defines the keys into the outer /map/ of a /timetable/.
	-> [timeslotId]				-- ^ Defines the keys in the enclosed /array/.
	-> Model.TimetableCoordinates.Vector observerId timeslotId
generateRasterCoordinates timetableAxes observerIds timeslotIdRange
	| Model.TimetableAxisTriple.hasWildSense timetableAxes	= error $ "WeekDaze.Model.Traverse.generateRasterCoordinates:\twild sense-specification received; " ++ show timetableAxes
	| otherwise						= case (axisX, axisY, axisZ) of
		(Model.TimetableAxis.ObserverId, Model.TimetableAxis.Day, Model.TimetableAxis.TimeslotId)	-> [
			(observerId, Temporal.Time.mkTime day timeslotId) |
				observerId	<- observerIdsBySense ! senseX,
				day		<- dayRangeBySense ! senseY,
				timeslotId	<- timeslotIdRangeBySense ! senseZ
		 ] -- List-comprehension.
		(Model.TimetableAxis.ObserverId, Model.TimetableAxis.TimeslotId, Model.TimetableAxis.Day)	-> [
			(observerId, Temporal.Time.mkTime day timeslotId) |
				observerId	<- observerIdsBySense ! senseX,
				timeslotId	<- timeslotIdRangeBySense ! senseY,
				day		<- dayRangeBySense ! senseZ
		 ] -- List-comprehension.
		(Model.TimetableAxis.Day, Model.TimetableAxis.ObserverId, Model.TimetableAxis.TimeslotId)	-> [
			(observerId, Temporal.Time.mkTime day timeslotId) |
				day		<- dayRangeBySense ! senseX,
				observerId	<- observerIdsBySense ! senseY,
				timeslotId	<- timeslotIdRangeBySense ! senseZ
		 ] -- List-comprehension.
		(Model.TimetableAxis.Day, Model.TimetableAxis.TimeslotId, Model.TimetableAxis.ObserverId)	-> [
			(observerId, Temporal.Time.mkTime day timeslotId) |
				day		<- dayRangeBySense ! senseX,
				timeslotId	<- timeslotIdRangeBySense ! senseY,
				observerId	<- observerIdsBySense ! senseZ
		 ] -- List-comprehension.
		(Model.TimetableAxis.TimeslotId, Model.TimetableAxis.ObserverId, Model.TimetableAxis.Day)	-> [
			(observerId, Temporal.Time.mkTime day timeslotId) |
				timeslotId	<- timeslotIdRangeBySense ! senseX,
				observerId	<- observerIdsBySense ! senseY,
				day		<- dayRangeBySense ! senseZ
		 ] -- List-comprehension.
		(Model.TimetableAxis.TimeslotId, Model.TimetableAxis.Day, Model.TimetableAxis.ObserverId)	-> [
			(observerId, Temporal.Time.mkTime day timeslotId) |
				timeslotId	<- timeslotIdRangeBySense ! senseX,
				day		<- dayRangeBySense ! senseY,
				observerId	<- observerIdsBySense ! senseZ
		 ] -- List-comprehension.
		_	-> error $ "WeekDaze.Model.Traverse.generateRasterCoordinates:\tunsupported axis-order; " ++ show timetableAxes ++ "."
	where
		[(senseX, axisX), (senseY, axisY), (senseZ, axisZ)]	= map (Data.Maybe.fromJust . Model.TimetableAxisTraversal.getMaybeSense &&& Model.TimetableAxisTraversal.getAxis) $ Model.TimetableAxisTriple.toList timetableAxes

		observerIdsBySense :: Data.Array.IArray.Array Model.TimetableAxisTraversal.Sense [observerId]
		observerIdsBySense	= Data.Array.IArray.array (minBound, maxBound) $ zip [minBound .. maxBound] [reverse observerIds, observerIds]

		dayRangeBySense	:: Data.Array.IArray.Array Model.TimetableAxisTraversal.Sense [Temporal.Day.Day]
		dayRangeBySense		= Data.Array.IArray.array (minBound, maxBound) $ zip [minBound .. maxBound] [reverse Temporal.Day.range, Temporal.Day.range]

		timeslotIdRangeBySense :: Data.Array.IArray.Array Model.TimetableAxisTraversal.Sense [timeslotId]
		timeslotIdRangeBySense	= Data.Array.IArray.array (minBound, maxBound) $ zip [minBound .. maxBound] [reverse timeslotIdRange, timeslotIdRange]

