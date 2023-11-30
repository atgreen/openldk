/* EMPTY.java -- Tests for the EMPTY field
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
   
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.2

package gnu.testlet.javax.swing.text.SimpleAttributeSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.SimpleAttributeSet;

/**
 * Some checks for the {@link EMPTY} field in the {@link SimpleAttributeSet} 
 * class.
 */
public class EMPTY implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("EMPTY");
    
    // check that it is empty
    harness.check(SimpleAttributeSet.EMPTY.getAttributeCount(), 0);
    harness.check(SimpleAttributeSet.EMPTY.getResolveParent(), null);
    
    // EMPTY should not be an instance of SimpleAttributeSet, since then 
    // you could cast it and add attributes to it...
    harness.check(SimpleAttributeSet.EMPTY instanceof SimpleAttributeSet, 
            false);
    
  }

}