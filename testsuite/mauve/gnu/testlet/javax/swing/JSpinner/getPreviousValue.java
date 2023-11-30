/* getPreviousValue.java -- some checks for the getPreviousValue() method in the
                            JSpinner class.
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.JSpinner;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

public class getPreviousValue implements Testlet {

  
  public void test(TestHarness harness) 
  {
    harness.checkPoint("()");
    SpinnerNumberModel m = new SpinnerNumberModel(2.0, 1.0, 3.0, 0.5);
    JSpinner s = new JSpinner(m);
    harness.check(s.getValue(), new Double(2.0));
    harness.check(s.getPreviousValue(), new Double(1.5));
    // accessing the previous value doesn't update the current value
    harness.check(s.getValue(), new Double(2.0));
    s.setValue(new Double(1.5));
    harness.check(s.getPreviousValue(), new Double(1.0));
    s.setValue(new Double(1.0));
    harness.check(s.getPreviousValue(), null);
  }
}
