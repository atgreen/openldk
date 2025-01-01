/* constructors.java -- Tests for the constructors
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
 * Some checks for the constructors in the {@link SimpleAttributeSet} class.
 */
public class constructors implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }
  
  public void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("SimpleAttributeSet()");
    SimpleAttributeSet s = new SimpleAttributeSet();
    harness.check(s.isEmpty(), true);
    harness.check(s.getResolveParent(), null);
  }

  public void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("SimpleAttributeSet(AttributeSet)");
    
    SimpleAttributeSet s = new SimpleAttributeSet();
    SimpleAttributeSet s1 = new SimpleAttributeSet(s);
    harness.check(s1.isEmpty(), true);
    harness.check(s1.getResolveParent(), null);
    
    // adding to original set doesn't update new set
    s.addAttribute("X1", "Y1");
    harness.check(s1.isEmpty());
    harness.check(s1.getResolveParent(), null);

    SimpleAttributeSet s2 = new SimpleAttributeSet(s);
    harness.check(s2.isEmpty(), false);
    harness.check(s2.getResolveParent(), null);
    harness.check(s2.getAttribute("X1"), "Y1");
    
    SimpleAttributeSet ss = new SimpleAttributeSet();
    ss.setResolveParent(s);
    ss.addAttribute("X2", "Y2");
    
    SimpleAttributeSet s3 = new SimpleAttributeSet(ss);
    harness.check(s3.isEmpty(), false);
    harness.check(s3.getResolveParent(), s);
    harness.check(s3.getAttribute("X1"), "Y1");
    harness.check(s3.getAttribute("X2"), "Y2");
    
    // try null argument
    boolean pass = false;
    try
      {
        /*SimpleAttributeSet ss =*/ new SimpleAttributeSet(null);
      }
    catch (NullPointerException e)
      {
        pass = true;
      }
    harness.check(pass);
  }

}