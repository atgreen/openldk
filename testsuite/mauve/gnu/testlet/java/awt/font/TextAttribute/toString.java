// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.font.TextAttribute;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.font.TextAttribute;

/**
 * Some tests for the toString() method in the {@link TextAttribute} class.
 */
public class toString implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.check(TextAttribute.BACKGROUND.toString(), "java.awt.font.TextAttribute(background)");
    harness.check(TextAttribute.BIDI_EMBEDDING.toString(), "java.awt.font.TextAttribute(bidi_embedding)");
    harness.check(TextAttribute.CHAR_REPLACEMENT.toString(), "java.awt.font.TextAttribute(char_replacement)");
    harness.check(TextAttribute.FAMILY.toString(), "java.awt.font.TextAttribute(family)");
    harness.check(TextAttribute.FONT.toString(), "java.awt.font.TextAttribute(font)");
    harness.check(TextAttribute.FOREGROUND.toString(), "java.awt.font.TextAttribute(foreground)");
    harness.check(TextAttribute.INPUT_METHOD_HIGHLIGHT.toString(), "java.awt.font.TextAttribute(input method highlight)");
    harness.check(TextAttribute.JUSTIFICATION.toString(), "java.awt.font.TextAttribute(justification)");
    harness.check(TextAttribute.NUMERIC_SHAPING.toString(), "java.awt.font.TextAttribute(numeric_shaping)");
    harness.check(TextAttribute.POSTURE.toString(), "java.awt.font.TextAttribute(posture)");
    harness.check(TextAttribute.RUN_DIRECTION.toString(), "java.awt.font.TextAttribute(run_direction)");
    harness.check(TextAttribute.SIZE.toString(), "java.awt.font.TextAttribute(size)");
    harness.check(TextAttribute.STRIKETHROUGH.toString(), "java.awt.font.TextAttribute(strikethrough)");
    harness.check(TextAttribute.WEIGHT.toString(), "java.awt.font.TextAttribute(weight)");
    harness.check(TextAttribute.WIDTH.toString(), "java.awt.font.TextAttribute(width)");
  } 

}