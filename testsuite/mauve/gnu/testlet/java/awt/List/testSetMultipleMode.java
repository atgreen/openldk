/* testSetMultipleMode.java -- FIXME: describe
   Copyright (C) 2006 FIXME: your info here
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

// Tags: FIXME
// Uses: ../../../javax/swing/ButtonGroup/isSelected

package gnu.testlet.java.awt.List;

import java.awt.Frame;
import java.awt.List;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.javax.swing.ButtonGroup.isSelected;

public class testSetMultipleMode implements Testlet
{

  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
  }
  
  public void test1(TestHarness harness)
  {
    List list = new List();
    list.setMultipleMode(true);
    harness.check(list.isMultipleMode());
    list.add("item");
    list.add("item1");
    list.select(1);
    list.select(0);
    Frame f = new Frame("");
    f.add(list);
    f.pack();
    harness.check(list.getSelectedIndex(), -1);
    list.setMultipleMode(false);
    harness.check(list.getSelectedIndex(), 0);
    harness.check(list.isMultipleMode(), false);
    harness.check(list.getSelectedIndex(), 0);
    f.dispose();
  }
  
  public void test2(TestHarness harness)
  {
    List list = new List();
    list.setMultipleMode(true);
    harness.check(list.isMultipleMode());
    list.add("item1");
    list.add("item2");
    list.add("item3");
    harness.check(list.getItemCount(), 3);
    list.select(1);
    list.select(0);
    list.select(2);
    harness.check(list.isSelected(0));
    harness.check(list.isSelected(1));
    harness.check(list.isSelected(2));
    list.setMultipleMode(false);
    harness.check(list.isMultipleMode(), false);
    harness.check(list.getItemCount(), 3);
    harness.check(list.getSelectedIndex(), -1);
    harness.check(list.isSelected(0));
    harness.check(list.isSelected(1), true);
    harness.check(list.isSelected(2), true);
  }

  public void test3(TestHarness harness)
  {
    List list = new List();
    list.add("item1");
    list.add("item2");
    list.add("item3");
    harness.check(list.isMultipleMode(), false);
    list.select(2);
    list.select(0);
    list.select(1);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), true);
    harness.check(list.isSelected(2), false);
    list.setMultipleMode(true);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), true);
    harness.check(list.isSelected(2), false);
  }
}
