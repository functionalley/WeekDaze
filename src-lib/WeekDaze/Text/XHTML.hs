{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
	Copyright (C) 2014 Dr. Alistair Ward

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

	* Declares an 'Int'-instance for XHTML.
	* Exports some convenience-functions for XHTML.
-}

module WeekDaze.Text.XHTML(
-- * Constants
--	showHideButtonCSSIdentifier,
-- * Functions
	mkShowHideButton,
	mkXHTMLDiv,
	mkXHTMLSpan
) where

import qualified	Text.XHtml.Strict
import			Text.XHtml.Strict((!), (<<))
import qualified	WeekDaze.Text.CSS	as Text.CSS

-- | A CSS class-label, for an HTML /button/.
showHideButtonCSSIdentifier :: Text.CSS.CSSIdentifier
showHideButtonCSSIdentifier	= "showHideButton"

instance Text.XHtml.Strict.HTML Int where
	toHtml	= Text.XHtml.Strict.toHtml . show

-- | Create a button to show/hide the specified element.
mkShowHideButton
	:: Bool				-- ^ Whether the element is initially displayed; if 'False', the caller should insert 'Text.XHtml.Strict.thestyle "display: none"' into their element.
	-> Text.CSS.CSSIdentifier	-- ^ The name of the element to show or hide.
	-> Text.XHtml.Strict.Html
mkShowHideButton displayState elementIdentifier	= Text.XHtml.Strict.button ! [
	Text.XHtml.Strict.theclass showHideButtonCSSIdentifier,
	Text.XHtml.Strict.strAttr "onclick" . showString "this.innerHTML = ((this.innerHTML !== '" . showString hideLabel . showString "') ? '" . showString hideLabel . showString "' : '" . showString showLabel . showString "'); var element = document.getElementById('" $ showString elementIdentifier "'); element.style.display = ((element.style.display !== 'none') ? 'none' : 'block');"
 ] << (
	if displayState
		then hideLabel
		else showLabel
 ) where
	hideLabel, showLabel :: String
	hideLabel	= "Hide"
	showLabel	= "Show"

-- | Make an XHTML /div/, which labels & classifies the contents.
mkXHTMLDiv :: String -> Text.XHtml.Strict.Html -> Text.XHtml.Strict.Html
mkXHTMLDiv s	= Text.XHtml.Strict.thediv ! [Text.XHtml.Strict.theclass $ Text.CSS.mkIdentifier s, Text.XHtml.Strict.title s]

-- | Make an XHTML /span/, which labels & classifies the contents.
mkXHTMLSpan :: String -> Text.XHtml.Strict.Html -> Text.XHtml.Strict.Html
mkXHTMLSpan s	= Text.XHtml.Strict.thespan ! [Text.XHtml.Strict.theclass $ Text.CSS.mkIdentifier s, Text.XHtml.Strict.title s]

