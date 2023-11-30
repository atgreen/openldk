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

import java.awt.font.TextAttribute;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some tests for the constants in the {@link TextAttribute} class.
 */
public class constants implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("JUSTIFICATION");
    harness.check(TextAttribute.JUSTIFICATION_FULL, new Float(1.0));
    harness.check(TextAttribute.JUSTIFICATION_NONE, new Float(0.0));
    
    harness.checkPoint("POSTURE");
    harness.check(TextAttribute.POSTURE_OBLIQUE, new Float(0.2));
    harness.check(TextAttribute.POSTURE_REGULAR, new Float(0.0));
    
    harness.checkPoint("RUN_DIRECTION");
    harness.check(TextAttribute.RUN_DIRECTION_LTR, Boolean.FALSE);
    harness.check(TextAttribute.RUN_DIRECTION_RTL, Boolean.TRUE);
    
    harness.checkPoint("STRIKETHROUGH");
    harness.check(TextAttribute.STRIKETHROUGH_ON, Boolean.TRUE);
    
    harness.checkPoint("SUPERSCRIPT");
    harness.check(TextAttribute.SUPERSCRIPT_SUB, new Integer(-1));
    harness.check(TextAttribute.SUPERSCRIPT_SUPER, new Integer(1));
    
    harness.checkPoint("SWAP_COLORS");
    harness.check(TextAttribute.SWAP_COLORS_ON, Boolean.TRUE);
    
    harness.checkPoint("WEIGHT");
    harness.check(TextAttribute.WEIGHT_BOLD, new Float(2.0));
    harness.check(TextAttribute.WEIGHT_DEMIBOLD, new Float(1.75));
    harness.check(TextAttribute.WEIGHT_DEMILIGHT, new Float(0.875));
    harness.check(TextAttribute.WEIGHT_EXTRA_LIGHT, new Float(0.5));
    harness.check(TextAttribute.WEIGHT_EXTRABOLD, new Float(2.5));
    harness.check(TextAttribute.WEIGHT_HEAVY, new Float(2.25));
    harness.check(TextAttribute.WEIGHT_LIGHT, new Float(0.75));
    harness.check(TextAttribute.WEIGHT_MEDIUM, new Float(1.5));
    harness.check(TextAttribute.WEIGHT_REGULAR, new Float(1.0));
    harness.check(TextAttribute.WEIGHT_SEMIBOLD, new Float(1.25));
    harness.check(TextAttribute.WEIGHT_ULTRABOLD, new Float(2.75));
    
    harness.checkPoint("WIDTH");
    harness.check(TextAttribute.WIDTH_CONDENSED, new Float(0.75));
    harness.check(TextAttribute.WIDTH_EXTENDED, new Float(1.5));
    harness.check(TextAttribute.WIDTH_REGULAR, new Float(1.0));
    harness.check(TextAttribute.WIDTH_SEMI_CONDENSED, new Float(0.875));
    harness.check(TextAttribute.WIDTH_SEMI_EXTENDED, new Float(1.25));
  }

}
