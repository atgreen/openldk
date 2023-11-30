/* getPopUpMenu.java -- FIXME: describe
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

import java.awt.Component;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getPopUpMenu
    implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    testAdd1(harness);
    testAdd2(harness);
    testAdd3(harness);
    testAdd4(harness);
    testAdd5(harness);
    testGetMenuComponents(harness);
    testInsert(harness);
    testIsPopUpMenuVisible(harness);
    testRemove1(harness);
    testRemove2(harness);
    testPopUpMenuVisible(harness);
    testSetSelectedHelper(harness);
  }

  public void testAdd1(TestHarness harness)
  {
    JMenu menu = new JMenu();
    // Creating an <code>Action</code> with no properties.
    Action myAction = new AbstractAction()
    {
      public void actionPerformed(ActionEvent evt)
      {
        System.out.println("MyAction");
      }
    };
    boolean pass = false;
    try
      {
        menu.add(myAction);
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testAdd2(TestHarness harness)
  {
    JMenu menu = new JMenu();
    Component comp = new MyClass();
    boolean pass = false;
    try
      {
        menu.add(comp);
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testAdd3(TestHarness harness)
  {
    JMenu menu = new JMenu();
    Component comp = new MyClass();
    boolean pass = false;
    try
      {
        menu.add(comp, 0);
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testAdd4(TestHarness harness)
  {
    JMenu menu = new JMenu();
    JMenuItem item = new JMenuItem();
    boolean pass = false;
    try
      {
        menu.add(item);
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testAdd5(TestHarness harness)
  {
    JMenu menu = new JMenu();
    boolean pass = false;
    try
      {
        menu.add("String");
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testGetMenuComponents(TestHarness harness)
  {
    JMenu menu = new JMenu();
    boolean pass = true;
    try
      {
        menu.getMenuComponents();
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testInsert(TestHarness harness)
  {
    JMenu menu = new JMenu();
    JMenuItem item = new JMenuItem();
    boolean pass = false;
    try
      {
        menu.insert(item, 0);
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testIsPopUpMenuVisible(TestHarness harness)
  {
    JMenu menu = new JMenu();
    boolean pass = false;
    try
      {
        menu.isPopupMenuVisible();
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testRemove1(TestHarness harness)
  {
    JMenu menu = new JMenu();
    Component c = new MyClass();
    boolean pass = false;
    try
      {
        menu.remove(c);
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testRemove2(TestHarness harness)
  {
    JMenu menu = new JMenu();
    JMenuItem item = new JMenuItem();
    boolean pass = false;
    try
      {
        menu.remove(item);
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testPopUpMenuVisible(TestHarness harness)
  {
    JMenu menu = new JMenu();
    boolean pass = false;
    try
      {
        menu.setPopupMenuVisible(true);
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public void testSetSelectedHelper(TestHarness harness)
  {
    JMenu menu = new JMenu();
    boolean pass = false;
    try
      {
        menu.setSelected(false);
        pass = true;
      }
    catch (NullPointerException e)
      {
        // Do nothing.
      }
    harness.check(pass);
  }

  public class MyClass
      extends Component
  {
  }

}
