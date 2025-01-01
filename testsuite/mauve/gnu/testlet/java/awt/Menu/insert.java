/* insert.java -- some checks for the insert() method in the Menu class.
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

// Tags: GUI JDK1.5

package gnu.testlet.java.awt.Menu;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Button;
import java.awt.Frame;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;

public class insert implements Testlet
{
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    testPR28699(harness);
  }
  
  public void test1(TestHarness harness)
  {
    harness.checkPoint("(MenuItem, int)");
    Menu menu = new Menu("Menu 1");
    MenuItem itemA = new MenuItem("Menu Item A");
    menu.insert(itemA, 0);
    harness.check(menu.getItem(0), itemA);
    MenuItem itemB = new MenuItem("Menu Item B");
    menu.insert(itemB, 1);
    harness.check(menu.getItem(1), itemB);
    MenuItem itemC = new MenuItem("Menu Item C");
    menu.insert(itemC, 0);
    harness.check(menu.getItem(0), itemC);
    harness.check(menu.getItem(1), itemA);
    harness.check(menu.getItem(2), itemB);
    
    // try negative index
    boolean pass = false;
    try
    {
      MenuItem itemD = new MenuItem("Menu Item D");
      menu.insert(itemD, -1);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try index too large - item gets inserted at end of menu
    MenuItem itemD = new MenuItem("Menu Item D");
    menu.insert(itemD, 4);
    harness.check(menu.getItem(0), itemC);
    harness.check(menu.getItem(1), itemA);
    harness.check(menu.getItem(2), itemB);
    harness.check(menu.getItem(3), itemD);
    
    // try null item
    pass = false;
    try
    {
      menu.insert((MenuItem) null, 0);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
  
  public void test2(TestHarness harness)
  {
    harness.checkPoint("(String, int)");
    Menu menu = new Menu("Menu 1");
    menu.insert("Menu Item A", 0);
    harness.check(menu.getItem(0).getLabel(), "Menu Item A");
    menu.insert("Menu Item B", 1);
    harness.check(menu.getItem(1).getLabel(), "Menu Item B");
    menu.insert("Menu Item C", 0);
    harness.check(menu.getItem(0).getLabel(), "Menu Item C");
    harness.check(menu.getItem(1).getLabel(), "Menu Item A");
    harness.check(menu.getItem(2).getLabel(), "Menu Item B");
    
    // try negative index
    boolean pass = false;
    try
    {
      menu.insert("Menu Item D", -1);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try index too large - item gets inserted at end of menu
    menu.insert("Menu Item D", 4);
    harness.check(menu.getItem(0).getLabel(), "Menu Item C");
    harness.check(menu.getItem(1).getLabel(), "Menu Item A");
    harness.check(menu.getItem(2).getLabel(), "Menu Item B");
    harness.check(menu.getItem(3).getLabel(), "Menu Item D");
    
    // try null item
    menu.insert((String) null, 0);
    harness.check(menu.getItem(0).getLabel(), null);
  }
  
  public void testPR28699(TestHarness harness)
  {
    harness.checkPoint("PR29699");
    Frame f = new Frame("AWT Menu Test");
    MenuBar mb = new MenuBar();
    Menu m = new Menu("Menu 1");
    mb.add(m);
    f.setMenuBar(mb);
    f.add(new Button("Button"));
    f.pack();
    MenuItem itemA = new MenuItem("Item A");
    m.insert(itemA, 0);
    MenuItem itemB = new MenuItem("Item B");
    m.insert(itemB, 0);
    MenuItem itemC = new MenuItem("Item C");
    m.insert(itemC, 1);
    MenuItem itemD = new MenuItem("Item D");
    m.insert(itemD, 1);
    harness.check(m.getItem(0), itemB);
    harness.check(m.getItem(1), itemD);
    harness.check(m.getItem(2), itemC);
    harness.check(m.getItem(3), itemA);
    f.dispose();
  }
}
