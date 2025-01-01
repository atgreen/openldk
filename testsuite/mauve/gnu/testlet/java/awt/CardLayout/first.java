//Tags: JDK1.0

//Copyright (C) 2006 Free Software Foundation, Inc.
//Written by Wolfgang Baer (WBaer@gmx.de)

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 51 Franklin Street, Fifth Floor,
//Boston, MA, 02110-1301 USA.

package gnu.testlet.java.awt.CardLayout;

import java.awt.CardLayout;
import java.awt.Frame;
import java.awt.Panel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the first method for various argument types.
 * As first, next, previous, last are all implemented by
 * the same internal method in GNU classpath this tests
 * all these methods for the argument types.
 */
public class first implements Testlet
{

  public void test(TestHarness harness)
  {
    CardLayout layout = new CardLayout();
    Frame containerWithLayout = new Frame();
    Panel panel = new Panel();
    containerWithLayout.setLayout(layout);

    // test without a panel added - no exception should be thrown
    try
      {
        layout.first(containerWithLayout);
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false);
      }

    // test correct usage - with an added panel
    containerWithLayout.add(panel, "Panel");
    try
      {
        layout.first(containerWithLayout);
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false);
      }

    // test null behaviour - triggers NPE
    try
      {
        layout.first(null);
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }

    // test usage with container without setLayout called
    Frame container = new Frame();
    container.add(panel);
    try
      {
        layout.first(container);
        harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
        harness.check(true);
      }
  }

}
