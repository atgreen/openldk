/* getAccessibleContext.java -- some checks for the getAccessibleContext()
       method in the JToolTip class.
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

package gnu.testlet.javax.swing.JToolTip;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.JToolTip;

public class getAccessibleContext 
  implements Testlet
{
  public void test(TestHarness harness)
  {
    JToolTip tt = new JToolTip();
    AccessibleContext att = tt.getAccessibleContext();
    
    // check the default values
    harness.check(att.getAccessibleName(), null);
    harness.check(att.getAccessibleDescription(), null);
    harness.check(att.getAccessibleComponent(), att);    
    harness.check(att.getAccessibleRole(), AccessibleRole.TOOL_TIP);

    // setting the tip text on the tool tip updates the accessible description
    tt.setTipText("XYZ");
    harness.check(att.getAccessibleDescription(), "XYZ");
    tt.setTipText(null);
    harness.check(att.getAccessibleDescription(), null);
    
    // setting the accessible description doesn't update the tip text
    att.setAccessibleDescription("ABC");
    harness.check(att.getAccessibleDescription(), "ABC");
    harness.check(tt.getTipText(), null);
    
    // ...and once an explicit description is set, it isn't changed by setting
    // the tip text
    tt.setTipText("DEF");
    harness.check(att.getAccessibleDescription(), "ABC");
    
    // TODO: the following should fire a property change event
    att.setAccessibleName("X");
    harness.check(att.getAccessibleName(), "X");

    // does getAccessibleContext() always return the same instance?
    AccessibleContext att2 = tt.getAccessibleContext();
    harness.check(att == att2);
  }
}
