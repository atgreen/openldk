/* getAccessibleName.java -- some checks for the getAccessibleName() method
       in the AccessibleJLabel class.
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

package gnu.testlet.javax.swing.JLabel.AccessibleJLabel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.accessibility.AccessibleContext;
import javax.swing.JLabel;

public class getAccessibleName implements Testlet
{
  public void test(TestHarness harness)
  {
    JLabel label = new JLabel("ABC");
    AccessibleContext ac = label.getAccessibleContext();
    harness.check(ac.getAccessibleName(), "ABC");
    JLabel label2 = new JLabel("JLabel2");
    label2.setLabelFor(label);
    harness.check(ac.getAccessibleName(), "ABC");
    label.setText(null);
    harness.check(ac.getAccessibleName(), "JLabel2");
    
    ac.setAccessibleName("XYZ");
    harness.check(ac.getAccessibleName(), "XYZ");
    label.setText("ABC");
    harness.check(ac.getAccessibleName(), "XYZ");
  }
}
