/* getInputMap.java -- some checks for the getInputMap() methods in the
       JComponent class.
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

// Tags: JDK1.3

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.plaf.InputMapUIResource;

public class getInputMap implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("()");
    JButton button = new JButton("X");
    InputMap m1 = button.getInputMap();
    InputMap m2 = button.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1 == m2);
  }
  
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    JButton button = new JButton("X");
    InputMap m1 = button.getInputMap(JComponent.WHEN_FOCUSED);
    InputMap m2 = button.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    InputMap m3 = button.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    // the following are just sanity checks. Real testing for the maps should
    // be in the JButton test code...
    harness.check(m1.keys(), null);
    harness.check(m1.getParent() instanceof InputMapUIResource);
    harness.check(m2.keys(), null);
    harness.check(m2.getParent(), null);
    harness.check(m3.keys(), null);
    harness.check(m3.getParent(), null);
    boolean pass = false;
    try
      {
        m1 = button.getInputMap(3);
      }
    catch (IllegalArgumentException e)
      {
        pass = true;
      }
    harness.check(pass);
    
    pass = false;
    try
      {
        m1 = button.getInputMap(JComponent.UNDEFINED_CONDITION);  // -1
      }
    catch (IllegalArgumentException e)
      {
        pass = true;
      }
    harness.check(pass);
  }
  
}
