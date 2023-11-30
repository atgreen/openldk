/* remove.java 
   Copyright (C) 2006 Tania Bento <tbento@redhat.com>
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

package gnu.testlet.java.awt.Choice;

import java.awt.Choice;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class remove implements Testlet
{

  public void test(TestHarness harness)
  {
    Choice c = new Choice();
    
    // testing remove with only one item
    harness.check(c.getSelectedIndex(), -1);
    harness.check(c.getItemCount(), 0);
    c.add("item1");
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getItemCount(), 1);
    c.select(0);
    harness.check(c.getSelectedIndex(), 0);
    c.remove(0);
    harness.check(c.getSelectedIndex(), -1);
    harness.check(c.getItemCount(), 0);
    
    // testing remove with two items
    c.add("item1");
    c.add("item2");
    harness.check(c.getItemCount(), 2);
    c.select(0);
    harness.check(c.getSelectedIndex(), 0);
    c.select(1);
    harness.check(c.getSelectedIndex(), 1);
    c.remove(1);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getItemCount(), 1);
    c.remove(0);
    harness.check(c.getSelectedIndex(), -1);
    harness.check(c.getItemCount(), 0);
    
    // testing remove with more than two items
    harness.check(c.getSelectedIndex(), -1);
    c.add("item1");
    harness.check(c.getSelectedIndex(), 0);
    c.add("item2");
    harness.check(c.getSelectedIndex(), 0);
    c.add("item3");
    harness.check(c.getSelectedIndex(), 0);
    c.add("item4");
    harness.check(c.getSelectedIndex(), 0);    
    harness.check(c.getItemCount(), 4);
    c.select(2);
    harness.check(c.getSelectedIndex(), 2);
    c.select(1);
    harness.check(c.getSelectedIndex(), 1);
    c.remove(1);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getItemCount(), 3);
    c.remove(0);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getItemCount(), 2);
    c.remove(0);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getItemCount(), 1);
    c.remove(0);
    harness.check(c.getSelectedIndex(), -1);
    harness.check(c.getItemCount(), 0);
    
    // testing remove with invalid indexes.
    boolean fail = false;
    try
      {
        c.remove(2);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        fail = true;
      } 
    harness.check(fail);
    
    // testing remove on last index
    c.add("item1");
    c.add("item2");
    c.add("item3");
    harness.check(c.getSelectedIndex(), 0);
    c.select(2);
    harness.check(c.getSelectedIndex(), 2);
    c.remove(2);
    harness.check(c.getSelectedIndex(), 0);
  }

}
