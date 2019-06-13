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

 [@DESCRIPTION@]	Defines a /pickler/ for a specific tuple, to augment the ones packaged in "Text.XML.HXT.Arrow.Pickle".
-}

module WeekDaze.Enhanced.EnhancedTuple(
-- * Functions
	xp57Tuple
) where

import qualified	Text.XML.HXT.Arrow.Pickle	as HXT
import			Text.XML.HXT.Arrow.Pickle(PU)

-- | An extreme addition to the picklers packaged in "Text.XML.HXT.Arrow.Pickle".
xp57Tuple :: PU a0 -> PU a1 -> PU a2 -> PU a3 -> PU a4 -> PU a5 -> PU a6 -> PU a7 -> PU a8 -> PU a9 -> PU a10 -> PU a11 -> PU a12 -> PU a13 -> PU a14 -> PU a15 -> PU a16 -> PU a17 -> PU a18 -> PU a19 -> PU a20 -> PU a21 -> PU a22 -> PU a23 -> PU a24 -> PU a25 -> PU a26 -> PU a27 -> PU a28 -> PU a29 -> PU a30 -> PU a31 -> PU a32 -> PU a33 -> PU a34 -> PU a35 -> PU a36 -> PU a37 -> PU a38 -> PU a39 -> PU a40 -> PU a41 -> PU a42 -> PU a43 -> PU a44 -> PU a45 -> PU a46 -> PU a47 -> PU a48 -> PU a49 -> PU a50 -> PU a51 -> PU a52 -> PU a53 -> PU a54 -> PU a55 -> PU a56 -> PU (
	a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56
 )
xp57Tuple a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47 a48 a49 a50 a51 a52 a53 a54 a55 a56	= HXT.xpWrap (
	\(
		(
			x0,	x1,	x2,	x3,	x4,	x5,	x6,	x7,	x8,	x9,	x10,	x11,	x12,	x13,	x14,	x15,	x16,	x17,	x18,	x19
		), (
			x20,	x21,	x22,	x23,	x24,	x25,	x26,	x27,	x28,	x29,	x30,	x31,	x32,	x33,	x34,	x35,	x36,	x37,	x38,	x39
		), (
			x40,	x41,	x42,	x43,	x44,	x45,	x46,	x47,	x48,	x49,	x50,	x51,	x52,	x53,	x54,	x55,	x56
		)
	) -> (
		x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56
	), -- Construct from a Triple.
	\(
		x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56
	) -> (
		(
			x0,	x1,	x2,	x3,	x4,	x5,	x6,	x7,	x8,	x9,	x10,	x11,	x12,	x13,	x14,	x15,	x16,	x17,	x18,	x19
		), (
			x20,	x21,	x22,	x23,	x24,	x25,	x26,	x27,	x28,	x29,	x30,	x31,	x32,	x33,	x34,	x35,	x36,	x37,	x38,	x39
		), (
			x40,	x41,	x42,	x43,	x44,	x45,	x46,	x47,	x48,	x49,	x50,	x51,	x52,	x53,	x54,	x55,	x56
		)
	) -- Deconstruct into a Triple.
 ) $ HXT.xpTriple (
	HXT.xp20Tuple	a0	a1	a2	a3	a4	a5	a6	a7	a8	a9	a10	a11	a12	a13	a14	a15	a16	a17	a18	a19
	) (
	HXT.xp20Tuple	a20	a21	a22	a23	a24	a25	a26	a27	a28	a29	a30	a31	a32	a33	a34	a35	a36	a37	a38	a39
	) (
	HXT.xp17Tuple	a40	a41	a42	a43	a44	a45	a46	a47	a48	a49	a50	a51	a52	a53	a54	a55	a56
 )

