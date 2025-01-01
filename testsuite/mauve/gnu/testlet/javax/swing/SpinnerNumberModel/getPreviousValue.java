/* getPreviousValue.java -- some checks for the getPreviousValue() method in the
                            SpinnerNumberModel class.
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

package gnu.testlet.javax.swing.SpinnerNumberModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.SpinnerNumberModel;

public class getPreviousValue implements Testlet {


  public void test(TestHarness harness) 
  {
    SpinnerNumberModel m = new SpinnerNumberModel(2.0, 1.0, 3.0, 0.5);
    harness.check(m.getValue(), new Double(2.0));
    harness.check(m.getPreviousValue(), new Double(1.5));
    // accessing the previous value doesn't update the current value
    harness.check(m.getValue(), new Double(2.0));
    m.setValue(new Double(1.5));
    harness.check(m.getPreviousValue(), new Double(1.0));
    m.setValue(new Double(1.0));
    harness.check(m.getPreviousValue(), null);

    // repeat for model without bounds
    m = new SpinnerNumberModel(new Integer(0), null, null, new Integer(1));
    harness.check(m.getValue(), new Integer(0));
    harness.check(m.getPreviousValue(), new Integer(-1));

    // accessing the next value doesn't update the current value
    harness.check(m.getValue(), new Integer(0));
    
    m.setValue(new Integer(-99));
    harness.check(m.getPreviousValue(), new Integer(-100));
    
    // what happens for min integer
    m.setValue(new Integer(Integer.MIN_VALUE));
    harness.check(m.getPreviousValue(), new Integer(Integer.MAX_VALUE));  
  }
}
