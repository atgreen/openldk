/* testGroup.java -- 
   Copyright (C) 2006 Red Hat
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

// Tags: JDK1.4

package gnu.testlet.java.awt.CheckboxGroup;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.Frame;
import java.awt.Robot;

public class testGroup
implements Testlet
{
  
  /**
   * This tests a checkbox in a group.
   * The checkbox turns to a radio button when it is
   * put into a group. If the group is set to null, then
   * the checkbox turns into a regular checkbox.
   * 
   * This tests a dynamically changing group.
   */
  public void test(TestHarness harness)
  {
    Robot r = harness.createRobot ();
    Frame frame = new Frame();
    Checkbox checkbox = new Checkbox("Checkbox");
    frame.add(checkbox);
    frame.setBounds(0, 0, 100, 100);
    
    harness.check(checkbox.getCheckboxGroup(), null);
    CheckboxGroup group = new CheckboxGroup();
    checkbox.setCheckboxGroup(group);
    harness.check(group, checkbox.getCheckboxGroup());
    frame.setVisible(true);
    
    r.waitForIdle ();
    r.delay(1000);
    
    checkbox.setCheckboxGroup(null);
    
    r.delay(1000);
    
    harness.check(checkbox.getCheckboxGroup(), null);
  }
}
