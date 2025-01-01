/* testSetUnitIncrement.java 
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

// Tags: 1.4

package gnu.testlet.java.awt.Scrollbar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Scrollbar;

public class testSetUnitIncrement implements Testlet
{

  public void test(TestHarness harness)
  {
    Scrollbar bar = new Scrollbar();
    
    // This check shows that the default value of lineIncrement is 1.
    harness.check(bar.getUnitIncrement(), 1);
    
    // These checks show that it is not possible to set lineIncrement
    // to 0.  Instead, it is set to 1.
    bar.setUnitIncrement(0);
    harness.check(bar.getUnitIncrement(), 1);
    bar.setUnitIncrement(5);
    harness.check(bar.getUnitIncrement(), 5);
    bar.setUnitIncrement(0);
    harness.check(bar.getUnitIncrement(), 1);
    
    // These checks show that there was unnecessary code in the method.
    // The unnecessary code would produce a value of 9 as the lineIncrement,
    // when in fact it should be set to 30.
    // NOTE: It is no longer necessary to check if 
    // (maximum - minimum) == 0 because maximum will never equal minimum. 
    bar.setMaximum(10);
    bar.setMinimum(1);
    harness.check(bar.getMaximum(), 10);
    harness.check(bar.getMinimum(), 1);
    harness.check(bar.getUnitIncrement(), 1);
    bar.setUnitIncrement(30);
    harness.check(bar.getUnitIncrement(), 30);
    harness.check(bar.getUnitIncrement() != 9);    
    
    // This test ensures that pageIncrement is not effected
    // if it is greater than the range.
    bar.setValues(1,1,1,2);
    harness.check(bar.getValue(), 1);
    harness.check(bar.getVisibleAmount(), 1);
    harness.check(bar.getMinimum(), 1);
    harness.check(bar.getMaximum(), 2);
    bar.setBlockIncrement(4);
    harness.check(bar.getBlockIncrement() > 
                    (bar.getMaximum() - bar.getMinimum()));
    harness.check(bar.getBlockIncrement() == 4);
    harness.check(bar.getBlockIncrement() != 
                    (bar.getMaximum() - bar.getMinimum()));
  }
  
}
