/* getPreviousValue.java -- some checks for the getPreviousValue() method in 
                            the SpinnerDateModel class.
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

package gnu.testlet.javax.swing.SpinnerDateModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Calendar;
import java.util.Date;

import javax.swing.SpinnerDateModel;
import javax.swing.SpinnerNumberModel;

public class getPreviousValue implements Testlet {

  public void test(TestHarness harness) 
  {
    Date now = new Date();
    Date prev = new Date(now.getTime() - 1L);  
    Date start = new Date(now.getTime() - 2L);
    Date end = new Date(now.getTime() + 2L);
      
    SpinnerDateModel m = new SpinnerDateModel(now, start, end, 
              Calendar.MILLISECOND);
    harness.check(m.getValue(), now);
    harness.check(m.getPreviousValue(), prev);
    
    // accessing the previous value doesn't update the current value
    harness.check(m.getValue(), now);
    m.setValue(prev);
    harness.check(m.getPreviousValue(), start);
    m.setValue(start);
    harness.check(m.getPreviousValue(), null);
    
    // repeat for model without bounds
    m = new SpinnerDateModel(now, null, null, Calendar.MILLISECOND);
    harness.check(m.getValue(), now);
    harness.check(m.getPreviousValue(), prev);

    // accessing the previous value doesn't update the current value
    harness.check(m.getValue(), now);    
  }
}
