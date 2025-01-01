/* testSetValues.java
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

import java.awt.Scrollbar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class testSetValues implements Testlet
{

  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
    test4(harness);
    test5(harness);
    test6(harness);
    test7(harness);
    test8(harness);
    test9(harness);
    test10(harness);
    test11(harness);
  }

  public void test1(TestHarness harness)
  {
    // This test ensures that if value < minimum, then
    // value is set to minimum.
    Scrollbar bar = new Scrollbar();
    bar.setValues(0, 1, 1, 2);
    harness.check(bar.getValue(), 1);
    harness.check(bar.getVisibleAmount(), 1);
    harness.check(bar.getMinimum(), 1);
    harness.check(bar.getMaximum(), 2);
  }
  
  public void test2(TestHarness harness)
  {
    // This test ensures that if visibleAmount < 0, then
    // it is set to 1.
    Scrollbar bar = new Scrollbar();
    bar.setValues(4, -5, 4, 8);
    harness.check(bar.getValue(), 4);
    harness.check(bar.getVisibleAmount(), 1);
    harness.check(bar.getMinimum(), 4);
    harness.check(bar.getMaximum(), 8);
  }
  
  public void test3(TestHarness harness)
  {
    // This test ensures that if visibleAmount = 0, then
    // it is set to 1.
    Scrollbar bar = new Scrollbar();
    bar.setValues(0, 0, 5, 10);
    harness.check(bar.getValue(), 5);
    harness.check(bar.getVisibleAmount(), 1);
    harness.check(bar.getMinimum(), 5);
    harness.check(bar.getMaximum(), 10);
  }
  public void test4(TestHarness harness)
  {
    // This test ensures that if maximum < minimum, then
    // maximum is set to minimum + 1.
    Scrollbar bar = new Scrollbar();
    bar.setValues(10, 1, 10, 5);
    harness.check(bar.getValue(), 10);
    harness.check(bar.getVisibleAmount(), 1);
    harness.check(bar.getMinimum(), 10);
    harness.check(bar.getMaximum(), 11);
  }
  
  public void test5(TestHarness harness)
  {
    // This test ensures that if value > maximum - visibleAmount,
    // then value is set to maximum - visibleAmount.
    Scrollbar bar = new Scrollbar();
    bar.setValues(10, 1, 5, 10);
    harness.check(bar.getValue(), 9);
    harness.check(bar.getVisibleAmount(), 1);
    harness.check(bar.getMinimum(), 5);
    harness.check(bar.getMaximum(), 10);
  }
  
  public void test6(TestHarness harness)
  {
    // This test ensures that if visibleAmount > maximum - minimum,
    // then visibleAmount is set to maximum - minimum.
    Scrollbar bar = new Scrollbar();
    bar.setValues(5, 30, 5, 20);
    harness.check(bar.getValue(), 5);
    harness.check(bar.getVisibleAmount(), 15);
    harness.check(bar.getMinimum(), 5);
    harness.check(bar.getMaximum(), 20);
  }
  
  public void test7(TestHarness harness)
  {
    // This test ensures that if maximum = minimum, 
    // then maximum is set to maximum + 1.
    Scrollbar bar = new Scrollbar();
    bar.setValues(5, 10, 20, 20);
    harness.check(bar.getValue(), 20);
    harness.check(bar.getVisibleAmount(), 1);
    harness.check(bar.getMinimum(), 20);
    harness.check(bar.getMaximum(), 21);
  }
  
  public void test8(TestHarness harness)
  {
    // This test ensures that if negative values are passed
    // as the value, minimum and maximum values, then
    // there is no effect.  That is, they are not rejected or
    // made positive.  
    Scrollbar bar = new Scrollbar();
    bar.setValues(-3, -2, -4, -8);
    harness.check(bar.getValue(), -4);
    harness.check(bar.getVisibleAmount(), 1);
    harness.check(bar.getMinimum(), -4);
    harness.check(bar.getMaximum(), -3);
  }
  
  public void test9(TestHarness harness)
  {
    // This test is taken from Intel's test suite
    // (test.java.awt.ScrollbarTest).  It passes on Sun
    // but fails on Classpath.
    Scrollbar bar = new Scrollbar();
    bar.setValues(0, 10, -100, 100);
    harness.check(bar.getValue(), 0);
    harness.check(bar.getVisibleAmount(), 10);
    harness.check(bar.getMinimum(), -100);
    harness.check(bar.getMaximum(), 100);
    bar.setMinimum(Integer.MIN_VALUE);
    harness.check(bar.getValue(), -11);
    harness.check(bar.getVisibleAmount(), 10);
    harness.check(bar.getMinimum(), Integer.MIN_VALUE);
    harness.check(bar.getMaximum(), -1);
  }

  public void test10(TestHarness harness)
  {
    // This test ensures that lineIncrement is not effected
    // if it is greater than the range.
    Scrollbar bar = new Scrollbar();
    bar.setValues(1, 1, 1, 2);
    harness.check(bar.getValue(), 1);
    harness.check(bar.getVisibleAmount(), 1);
    harness.check(bar.getMinimum(), 1);
    harness.check(bar.getMaximum(), 2);
    bar.setUnitIncrement(4);
    harness.check(bar.getUnitIncrement() > 
                    (bar.getMaximum() - bar.getMinimum()));
    harness.check(bar.getUnitIncrement() == 4);
    harness.check(bar.getUnitIncrement() != 
                    (bar.getMaximum() - bar.getMinimum()));
  }
  
  public void test11(TestHarness harness)
  {
    // This test ensures that pageIncrement is not effected
    // if it is greater than the range.
    Scrollbar bar = new Scrollbar();
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
