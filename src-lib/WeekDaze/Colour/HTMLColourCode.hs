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

 [@DESCRIPTION@] Exports some colour-manipulation functions for HTML.
-}

module WeekDaze.Colour.HTMLColourCode(
-- * Types
-- ** Type-synonyms
	HTMLColourCode,
-- * Constants
--	htmlColourCodeHexDigits,
	htmlColourCodePrefix,
-- * Functions
	deriveComplementaryHTMLColourCode,
	generateHTMLColourCodeFrom,
--	hash,
	readHex
) where

import qualified	Crypto.Hash
import qualified	Data.Bits
import			Data.Bits((.&.))
import qualified	Data.ByteString.Char8
import qualified	Numeric
import qualified	Text.Printf

-- | The representation in HTML of 24-bit colour.
type HTMLColourCode	= String

-- | The number of hex-digits required to represent 24-bit colour in HTML.
htmlColourCodeHexDigits :: Int
htmlColourCodeHexDigits	= 6

-- | The prefix used when specifying an HTML colour-code.
htmlColourCodePrefix :: Char
htmlColourCodePrefix	= '#'

-- | Hash the specified string.
hash :: String -> String
hash	= (show :: Crypto.Hash.Digest Crypto.Hash.MD5 {-arbitrarily-} -> String) . Crypto.Hash.hash . Data.ByteString.Char8.pack

-- | Generate an arbitrary HTML colour-code using a hash of the specified string.
generateHTMLColourCodeFrom :: String -> HTMLColourCode
generateHTMLColourCodeFrom	= (htmlColourCodePrefix :) . take htmlColourCodeHexDigits . hash

-- | Interpret the specified string as hexadecimal.
readHex :: (Eq i, Num i) => String -> i
readHex s	= case Numeric.readHex s of
	[(hex, [])]	-> hex
	_		-> error $ "WeekDaze.Colour.HTMLColourCode.readHex:\tfailed to interpret '" ++ s ++ "' as hexadecimal."

-- | Return the complement of the specified HTML colour-code.
deriveComplementaryHTMLColourCode :: HTMLColourCode -> HTMLColourCode
deriveComplementaryHTMLColourCode htmlColourCode	= Text.Printf.printf (htmlColourCodePrefix : "%0*x") htmlColourCodeHexDigits $ bitMask .&. Data.Bits.complement (readHex $ tail htmlColourCode) where
	bitMask :: Int
	bitMask	= 0xffffff	-- 24-bit colour.

