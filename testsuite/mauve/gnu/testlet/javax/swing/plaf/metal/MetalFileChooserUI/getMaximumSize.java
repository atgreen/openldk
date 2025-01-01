// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.plaf.metal.MetalFileChooserUI;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;

import javax.swing.JFileChooser;
import javax.swing.plaf.metal.MetalFileChooserUI;

/**
 * Some checks for the getMaximumSize() method in the 
 * {@link MetalFileChooserUI} class.  
 */
public class getMaximumSize implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    JFileChooser fc = new JFileChooser();
    MetalFileChooserUI ui = new MetalFileChooserUI(fc);
    Dimension expected = new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
    Dimension d1 = ui.getMaximumSize(fc);
    harness.check(d1, expected);
    
    // check that the method returns a new instance every time?
    Dimension d2 = ui.getMaximumSize(fc);
    harness.check(d1 != d2);
    
    // does the method look at the filechooser passed in?
    Dimension d3 = ui.getMaximumSize(null);
    harness.check(d3, expected);
  }

}
