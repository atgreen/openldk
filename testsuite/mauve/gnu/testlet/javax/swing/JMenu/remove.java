/* remove.java -- FIXME: describe
 Copyright (C) 2006 Red Hat, Tania Bento <tbento@redhat.com>
 
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


package gnu.testlet.javax.swing.JMenu;

import javax.swing.JMenu;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class remove implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
  }
  
  public void test1(TestHarness harness)
  {
    JMenu menu = new JMenu();
    boolean fail = false;
    try
      {
        menu.remove(-1);
      }
    catch (IllegalArgumentException e)
      { 
        fail = true;
      }
    harness.check(fail);
  }
  
  public void test2(TestHarness harness)
  {
    JMenu menu = new JMenu();
    boolean pass = false;
    try
      {
        menu.remove(0);
        pass = true;
      }
    catch (Exception e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }
  
  public void test3(TestHarness harness)
  {
    JMenu menu = new JMenu();
    boolean fail = false;
    try
      {
        menu.remove(1);
      }
    catch (Exception e)
      {
        fail = true;
      }
    harness.check(fail);
  }
  
}
