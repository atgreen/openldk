/* testSelected.java -- 
 Copyright (C) 2006 RedHat
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

// Tags: JDK 1.4E


package gnu.testlet.java.awt.List;

import java.awt.List;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class testSelected
    implements Testlet
{

  private List list;

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
    test12(harness);
    test13(harness);
    test14(harness);
    test15(harness);
    test16(harness);
  }

  public void test1(TestHarness harness)
  {
    // Testing that only one item can be selected when 
    // multipleMode is set to false.
    list = new List();
    harness.check (list.getSelectedIndex() == -1);
    list.setMultipleMode(false);
    list.add("item1");
    list.add("item2");
    list.select(0);
    list.select(1);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), true);
  }

  public void test2(TestHarness harness)
  {
    // Testing that more than one item can be selected when
    // multipleMode is set to true.
    list = new List();
    list.setMultipleMode(true);
    list.add("item1");
    list.add("item2");
    list.select(0);
    list.select(1);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), true);
  }

  public void test3(TestHarness harness)
  {
    // Testing that only one item can be selected when
    // multipleMode is set to false, regardless of the order
    // that the selection is made.
    list = new List();
    list.setMultipleMode(false);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.select(0);
    list.select(2);
    list.select(1);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), true);
    harness.check(list.isSelected(2), false);
  }

  public void test4(TestHarness harness)
  {
    // Testing that more than one item can be selected when
    // multipleMode is set to true, regardless of the order
    // that the selection is made.
    list = new List();
    list.setMultipleMode(true);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.select(0);
    list.select(2);
    list.select(1);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), true);
    harness.check(list.isSelected(2), true);
  }

  public void test5(TestHarness harness)
  {
    // Testing that deselect works on one item when 
    // multipleMode is set to false.
    list = new List();
    list.setMultipleMode(false);
    list.add("item1");
    list.add("item2");
    list.select(0);
    list.select(1);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), true);
    list.deselect(1);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), false);
  }

  public void test6(TestHarness harness)
  {
    // Testing that deslect works on one item when 
    // multipleMode is set to true.
    list = new List();
    list.setMultipleMode(true);
    list.add("item1");
    list.add("item2");
    list.select(0);
    list.select(1);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), true);
    list.deselect(1);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), false);
  }

  public void test7(TestHarness harness)
  {
    // Testing that deselect works on more than one item when
    // multipleMode is set to true.
    list = new List();
    list.setMultipleMode(true);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.add("item4");
    list.select(0);
    list.select(1);
    list.select(2);
    list.select(3);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), true);
    harness.check(list.isSelected(2), true);
    harness.check(list.isSelected(3), true);
    list.deselect(1);
    list.deselect(3);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), false);
    harness.check(list.isSelected(2), true);
    harness.check(list.isSelected(3), false);
  }

  public void test8(TestHarness harness)
  {
    // Again, testing that deselect works on more than one
    // item when Multimode is set to true.
    list = new List();
    list.setMultipleMode(true);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.select(0);
    list.select(1);
    list.select(2);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), true);
    harness.check(list.isSelected(2), true);
    list.deselect(0);
    list.deselect(1);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), false);
    harness.check(list.isSelected(2), true);
  }

  public void test9(TestHarness harness)
  {
    // Testing clear.
    list = new List();
    list.setMultipleMode(false);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.add("item4");
    list.select(0);
    list.select(1);
    list.select(2);
    list.select(3);
    harness.check(list.getItemCount(), 4);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), false);
    harness.check(list.isSelected(2), false);
    harness.check(list.isSelected(3), true);
    list.clear();
    harness.check(list.getItemCount(), 0);
  }

  public void test10(TestHarness harness)
  {
    // Again, testing clear.
    list = new List();
    list.setMultipleMode(true);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.add("item4");
    list.select(0);
    list.select(2);
    harness.check(list.getItemCount(), 4);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), false);
    harness.check(list.isSelected(2), true);
    harness.check(list.isSelected(3), false);
    list.clear();
    harness.check(list.getItemCount(), 0);
  }

  public void test11(TestHarness harness)
  {
    // Testing delItem on one item and when 
    // multipleMode is set to false.
    list = new List();
    list.setMultipleMode(false);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.select(2);
    harness.check(list.getItemCount(), 3);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), false);
    harness.check(list.isSelected(2), true);
    list.delItem(2);
    harness.check(list.getItemCount(), 2);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), false);
    harness.check(list.isSelected(2), true);
    boolean exceptionCaught = false;
    try
    {
    	// This will throw an exception, you cannot call getItem(2)
    	// despite the fact that isSelected(2) returns true.
    	list.getItem(2);
    }
    catch (ArrayIndexOutOfBoundsException ex)
    {
    	exceptionCaught = true;
    }
    harness.check(exceptionCaught);
  }

  public void test12(TestHarness harness)
  {
    // Testing delItem on two items and when 
    // multipleMode is set to true.
    list = new List();
    list.setMultipleMode(true);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.select(0);
    list.select(1);
    harness.check(list.getItemCount(), 3);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), true);
    harness.check(list.isSelected(2), false);
    list.delItem(0);
    harness.check(list.getItemCount(), 2);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), true);
    list.delItem(1);
    harness.check(list.getItemCount(), 1);
    harness.check(list.isSelected(0), true);
  }

  public void test13(TestHarness harness)
  {
    // Testing delItems when multipleMode is set to false.
    list = new List();
    list.setMultipleMode(false);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.select(2);
    list.select(1);
    harness.check(list.getItemCount(), 3);
    harness.check(list.isSelected(0), false);
    harness.check(list.isSelected(1), true);
    harness.check(list.isSelected(2), false);
    list.delItems(1, 2);
    harness.check(list.getItemCount(), 1);
    harness.check(list.isSelected(0), false);
  }

  public void test14(TestHarness harness)
  {
    //Testing delItems when mulitpleMode is set to true.
    list = new List();
    list.setMultipleMode(true);
    list.add("item1");
    list.add("item2");
    list.add("item3");
    list.add("item4");
    list.select(0);
    list.select(3);
    harness.check(list.getItemCount(), 4);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), false);
    harness.check(list.isSelected(2), false);
    harness.check(list.isSelected(3), true);
    list.delItems(0, 1);
    harness.check(list.getItemCount(), 2);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), false);
  }

  public void test15(TestHarness harness)
  {
    // Testing behaviour when index passed is invalid.

    // Testing deselect.
    list = new List();
    list.add("item1");
    list.select(0);
    list.deselect(1);
    harness.check(list.isSelected(0), true);

    // Testing delItem.
    boolean fail = false;
    harness.check(list.getItemCount(), 1);
    try
      {
        list.delItem(1);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        fail = true;
      }
    harness.check(fail);
    harness.check(list.getItemCount(), 1);
    harness.check(list.isSelected(0), true);

    // Testing delItems.
    fail = false;
    list.add("item2");
    list.add("item3");
    harness.check(list.getItemCount(), 3);
    try
      {
        list.delItems(1, 3);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        fail = true;
      }
    harness.check(fail);
    harness.check(list.getItemCount(), 3);

    // Again, testing delItems.
    fail = false;
    harness.check(list.getItemCount(), 3);
    try
      {
        list.delItems(- 4, 1);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        fail = true;
      }
    harness.check(fail);
    harness.check(list.getItemCount(), 1);

    // Again, testing delItems.
    fail = false;
    harness.check(list.getItemCount(), 1);
    try
      {
        list.delItems(- 1, 5);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        fail = true;
      }
    harness.check(fail);
    harness.check(list.getItemCount(), 1);

    // Again, testing delItems
    harness.check(list.getItemCount(), 1);
    list.delItems(4, 1);
    harness.check(list.getItemCount(), 1);
  }

  public void test16(TestHarness harness)
  {
    // Testing what happens when a selected item
    // is selected again.
    list = new List();
    list.setMultipleMode(false);
    list.add("item1");
    list.select(0);
    list.select(0);
    harness.check(list.isSelected(0), true);

    // Testing what happens when a deselected item
    // is deselected again.
    list.add("item2");
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), false);
    list.deselect(1);
    harness.check(list.isSelected(0), true);
    harness.check(list.isSelected(1), false);

    // Testing what happens when a replacedItem
    // was selected.
    harness.check(list.isSelected(0), true);
    list.replaceItem("newItem1", 0);
    harness.check(list.isSelected(0), true);

    // Testing that happens when a replacedItem 
    // was deselected.
    harness.check(list.isSelected(1), false);
    list.replaceItem("newItem2", 1);
    harness.check(list.isSelected(1), false);
  }
}
