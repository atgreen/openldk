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
 * Tests the show method for various argument types.
 */
public class show implements Testlet
{

  public void test(TestHarness harness)
  {
    CardLayout layout = new CardLayout();
    Frame containerWithLayout = new Frame();
    Panel panel = new Panel();
    containerWithLayout.setLayout(layout);
    containerWithLayout.add(panel, "Panel");

    // test correct usage
    try
      {
        layout.show(containerWithLayout, "Panel");
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false);
      }

    // test with string == null
    // nothing should happen no exception
    try
      {
        layout.show(containerWithLayout, null);
        harness.check(true);
      }
    catch (Exception e)
      {
        e.printStackTrace();
        harness.check(false);
      }

    // same with unknown names
    try
      {
        layout.show(containerWithLayout, "XXXXX");
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false);
      }

    // same with empty strings
    try
      {
        layout.show(containerWithLayout, "");
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.check(false);
      }

    // test usage with container without setLayout called
    Frame container = new Frame();
    container.add(panel);
    try
      {
        layout.show(container, "Panel");
        harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
        harness.check(true);
      }
    
    // test usage with container with another CardLayout instance called 
    CardLayout layout2 = new CardLayout();
    container.setLayout(layout2);
    try
      {
        layout.show(container, "Panel");
        harness.check(false);
      }
    catch (IllegalArgumentException e)
      {
        harness.check(true);
      }
 
    // test usage with container == null
    // NPE must be thrown
    try
      {
        layout.show(null, "Panel");
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }
  
  }

}
