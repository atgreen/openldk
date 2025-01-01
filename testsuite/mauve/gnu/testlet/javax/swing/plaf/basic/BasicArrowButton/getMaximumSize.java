// Tags: JDK1.2

// Copyright (C) 2005, 2006, David Gilbert <david.gilbert@object-refinery.com>

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
// along with GNU Classpath; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.javax.swing.plaf.basic.BasicArrowButton;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;

import javax.swing.SwingConstants;
import javax.swing.plaf.basic.BasicArrowButton;

/**
 * Some checks for the getMaximumSize() method in the 
 * {@link BasicArrowButton} class.  
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
    BasicArrowButton b = new BasicArrowButton(SwingConstants.NORTH);
    harness.check(b.getMaximumSize(), new Dimension(Integer.MAX_VALUE, 
            Integer.MAX_VALUE));  
    
    // setting the maximum size explicitly has no effect
    b.setMaximumSize(new Dimension(12, 34));
    harness.check(b.getMaximumSize(), new Dimension(Integer.MAX_VALUE, 
            Integer.MAX_VALUE));
    
    // modifying the returned value should not affect the button
    Dimension m = b.getMaximumSize();
    m.setSize(1, 2);
    harness.check(b.getMaximumSize(), new Dimension(Integer.MAX_VALUE, 
            Integer.MAX_VALUE));    
  }

}
