// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke (kennke@aicas.com)

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.text.html.CSS;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.html.CSS;

/**
 * Tests the values of the constants in the inner class Attribute
 * of javax.swing.text.html.CSS.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class Attribute implements Testlet
{

  public void test(TestHarness harness)
  {
    testAttribute(harness, CSS.Attribute.BACKGROUND, "background", false, null);
    testAttribute(harness, CSS.Attribute.BACKGROUND_ATTACHMENT, "background-attachment", false, "scroll");
    testAttribute(harness, CSS.Attribute.BACKGROUND_COLOR, "background-color", false, "transparent");
    testAttribute(harness, CSS.Attribute.BACKGROUND_IMAGE, "background-image", false, "none");
    testAttribute(harness, CSS.Attribute.BACKGROUND_POSITION, "background-position", false, null);
    testAttribute(harness, CSS.Attribute.BACKGROUND_REPEAT, "background-repeat", false, "repeat");
    testAttribute(harness, CSS.Attribute.BORDER, "border", false, null);
    testAttribute(harness, CSS.Attribute.BORDER_BOTTOM, "border-bottom", false, null);
    testAttribute(harness, CSS.Attribute.BORDER_BOTTOM_WIDTH, "border-bottom-width", false, "medium");
    testAttribute(harness, CSS.Attribute.BORDER_COLOR, "border-color", false, "black");
    testAttribute(harness, CSS.Attribute.BORDER_LEFT, "border-left", false, null);
    testAttribute(harness, CSS.Attribute.BORDER_LEFT_WIDTH, "border-left-width", false, "medium");
    testAttribute(harness, CSS.Attribute.BORDER_RIGHT, "border-right", false, null);
    testAttribute(harness, CSS.Attribute.BORDER_RIGHT_WIDTH, "border-right-width", false, "medium");
    testAttribute(harness, CSS.Attribute.BORDER_STYLE, "border-style", false, "none");
    testAttribute(harness, CSS.Attribute.BORDER_TOP, "border-top", false, null);
    testAttribute(harness, CSS.Attribute.BORDER_TOP_WIDTH, "border-top-width", false, "medium");
    testAttribute(harness, CSS.Attribute.BORDER_WIDTH, "border-width", false, "medium");
    testAttribute(harness, CSS.Attribute.CLEAR, "clear", false, "none");
    testAttribute(harness, CSS.Attribute.COLOR, "color", true, "black");
    testAttribute(harness, CSS.Attribute.DISPLAY, "display", false, "block");
    testAttribute(harness, CSS.Attribute.FLOAT, "float", false, "none");
    testAttribute(harness, CSS.Attribute.FONT, "font", true, null);
    testAttribute(harness, CSS.Attribute.FONT_FAMILY, "font-family", true, null);
    testAttribute(harness, CSS.Attribute.FONT_SIZE, "font-size", true, "medium");
    testAttribute(harness, CSS.Attribute.FONT_STYLE, "font-style", true, "normal");
    testAttribute(harness, CSS.Attribute.FONT_VARIANT, "font-variant", true, "normal");
    testAttribute(harness, CSS.Attribute.FONT_WEIGHT, "font-weight", true, "normal");
    testAttribute(harness, CSS.Attribute.HEIGHT, "height", false, "auto");
    testAttribute(harness, CSS.Attribute.LETTER_SPACING, "letter-spacing", true, "normal");
    testAttribute(harness, CSS.Attribute.LINE_HEIGHT, "line-height", true, "normal");
    testAttribute(harness, CSS.Attribute.LIST_STYLE, "list-style", true, null);
    testAttribute(harness, CSS.Attribute.LIST_STYLE_IMAGE, "list-style-image", true, "none");
    testAttribute(harness, CSS.Attribute.LIST_STYLE_POSITION, "list-style-position", true, "outside");
    testAttribute(harness, CSS.Attribute.LIST_STYLE_TYPE, "list-style-type", true, "disc");
    testAttribute(harness, CSS.Attribute.MARGIN, "margin", false, null);
    testAttribute(harness, CSS.Attribute.MARGIN_BOTTOM, "margin-bottom", false, "0");
    testAttribute(harness, CSS.Attribute.MARGIN_LEFT, "margin-left", false, "0");
    testAttribute(harness, CSS.Attribute.MARGIN_RIGHT, "margin-right", false, "0");
    testAttribute(harness, CSS.Attribute.MARGIN_TOP, "margin-top", false, "0");
    testAttribute(harness, CSS.Attribute.PADDING, "padding", false, null);
    testAttribute(harness, CSS.Attribute.PADDING_BOTTOM, "padding-bottom", false, "0");
    testAttribute(harness, CSS.Attribute.PADDING_LEFT, "padding-left", false, "0");
    testAttribute(harness, CSS.Attribute.PADDING_RIGHT, "padding-right", false, "0");
    testAttribute(harness, CSS.Attribute.PADDING_TOP, "padding-top", false, "0");
    testAttribute(harness, CSS.Attribute.TEXT_ALIGN, "text-align", true, null);
    testAttribute(harness, CSS.Attribute.TEXT_DECORATION, "text-decoration", true, "none");
    testAttribute(harness, CSS.Attribute.TEXT_INDENT, "text-indent", true, "0");
    testAttribute(harness, CSS.Attribute.TEXT_TRANSFORM, "text-transform", true, "none");
    testAttribute(harness, CSS.Attribute.VERTICAL_ALIGN, "vertical-align", false, "baseline");
    testAttribute(harness, CSS.Attribute.WHITE_SPACE, "white-space", true, "normal");
    testAttribute(harness, CSS.Attribute.WIDTH, "width", false, "auto");
    testAttribute(harness, CSS.Attribute.WORD_SPACING, "word-spacing", true, "normal");
  }

  void testAttribute(TestHarness harness, CSS.Attribute att, String attStr,
                     boolean inherited, String defaultValue)
  {
    harness.check(att.toString(), attStr);
    harness.check(att.isInherited(), inherited);
    harness.check(att.getDefaultValue(), defaultValue);
  }
}
