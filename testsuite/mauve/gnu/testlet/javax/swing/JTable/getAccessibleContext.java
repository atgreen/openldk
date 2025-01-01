/* getAccessibleContext.java -- some checks for the getAccessibleContext() 
       method in the JTable class.
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

package gnu.testlet.javax.swing.JTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.JTable;

public class getAccessibleContext implements Testlet
{
  public void test(TestHarness harness)
  {
    JTable t = new JTable();
    AccessibleContext ac = t.getAccessibleContext();
    harness.check(ac.getAccessibleName(), null);
    harness.check(ac.getAccessibleRole(), AccessibleRole.TABLE);
    harness.check(ac.getAccessibleAction(), null);
    harness.check(ac.getAccessibleComponent(), ac);
    harness.check(ac.getAccessibleDescription(), null);
    harness.check(ac.getAccessibleEditableText(), null);
    harness.check(ac.getAccessibleIcon(), null);
    harness.check(ac.getAccessibleTable(), ac);
    harness.check(ac.getAccessibleText(), null);

  }
}
