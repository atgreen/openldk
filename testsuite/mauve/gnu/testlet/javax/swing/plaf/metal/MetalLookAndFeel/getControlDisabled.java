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

package gnu.testlet.javax.swing.plaf.metal.MetalLookAndFeel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Color;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the getControlDisabled() method in the MetalLookAndFeel class.
 */
public class getControlDisabled implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) {
	MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
    ColorUIResource c = MetalLookAndFeel.getControlDisabled();
    harness.check(c, new ColorUIResource(new Color(153, 153, 153)));

    MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme() {
      public ColorUIResource getControlDisabled() {
        return new ColorUIResource(Color.red); 
      }
    });

    c = MetalLookAndFeel.getControlDisabled();
    harness.check(c, new ColorUIResource(Color.red));
    
    // reset the theme so that other tests won't be affected
    MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());

  }

}
