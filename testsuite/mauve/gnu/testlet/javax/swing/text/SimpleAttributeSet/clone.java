/* clone.java -- Tests for the clone() method in the SimpleAttributeSet class.
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
 * Some checks for the clone() method in the {@link SimpleAttributeSet} 
 * class.
 */
public class clone implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("clone()");
    
    // clone an empty set
    SimpleAttributeSet s1 = new SimpleAttributeSet();
    SimpleAttributeSet s2 = (SimpleAttributeSet) s1.clone();
    harness.check(s1.equals(s2));
    s1.addAttribute("X1", "Y1");
    harness.check(!s1.equals(s2));
    
    // clone a set with no resolve parent
    s2 = (SimpleAttributeSet) s1.clone();
    harness.check(s1.equals(s2));
    s1.addAttribute("X2", "Y2");
    harness.check(!s1.equals(s2));
    
    // clone a set with a resolve parent
    SimpleAttributeSet s3 = new SimpleAttributeSet();
    s3.addAttribute("A1", "B1");
    s1.setResolveParent(s3);
    s2 = (SimpleAttributeSet) s1.clone();
    harness.check(s1.equals(s2));
    s3.addAttribute("A1", "BB");
    harness.check(s1.equals(s2));
    s1.addAttribute("Y1", "-");    
    harness.check(!s1.equals(s2));    
    
  }

}