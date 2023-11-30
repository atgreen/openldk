/* getNextMatch.java -- some checks for the getNextMatch() method in the JList
       class.
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

// Tags: 1.4

package gnu.testlet.javax.swing.JList;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JList;
import javax.swing.text.Position;

public class getNextMatch implements Testlet
{
  public void test(TestHarness harness)
  {
    JList list = new JList(new Object[] {"A", "B", "C", "a1", "b1", "c1", "AA", "BB", "CC"});
    harness.check(list.getNextMatch("A", 0, Position.Bias.Forward), 0);
    harness.check(list.getNextMatch("A", 1, Position.Bias.Forward), 3);
    harness.check(list.getNextMatch("A", 2, Position.Bias.Forward), 3);
    harness.check(list.getNextMatch("A", 3, Position.Bias.Forward), 3);
    harness.check(list.getNextMatch("A", 4, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("A", 5, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("A", 6, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("A", 7, Position.Bias.Forward), 0);
    harness.check(list.getNextMatch("A", 8, Position.Bias.Forward), 0);

    harness.check(list.getNextMatch("a", 0, Position.Bias.Backward), 0);
    harness.check(list.getNextMatch("a", 1, Position.Bias.Backward), 0);
    harness.check(list.getNextMatch("a", 2, Position.Bias.Backward), 0);
    harness.check(list.getNextMatch("a", 3, Position.Bias.Backward), 3);
    harness.check(list.getNextMatch("a", 4, Position.Bias.Backward), 3);
    harness.check(list.getNextMatch("a", 5, Position.Bias.Backward), 3);
    harness.check(list.getNextMatch("a", 6, Position.Bias.Backward), 6);
    harness.check(list.getNextMatch("a", 7, Position.Bias.Backward), 6);
    harness.check(list.getNextMatch("a", 8, Position.Bias.Backward), 6);

    harness.check(list.getNextMatch("A", 0, null), 0);
    harness.check(list.getNextMatch("A", 1, null), 0);
    harness.check(list.getNextMatch("A", 2, null), 0);
    harness.check(list.getNextMatch("A", 3, null), 3);
    harness.check(list.getNextMatch("A", 4, null), 3);
    harness.check(list.getNextMatch("A", 5, null), 3);
    harness.check(list.getNextMatch("A", 6, null), 6);
    harness.check(list.getNextMatch("A", 7, null), 6);
    harness.check(list.getNextMatch("A", 8, null), 6);

    harness.check(list.getNextMatch("Aa", 0, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("Aa", 1, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("Aa", 2, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("Aa", 3, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("Aa", 4, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("Aa", 5, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("Aa", 6, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("Aa", 7, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("Aa", 8, Position.Bias.Forward), 6);

    harness.check(list.getNextMatch("", 0, Position.Bias.Forward), 0);
    harness.check(list.getNextMatch("", 1, Position.Bias.Forward), 1);
    harness.check(list.getNextMatch("", 2, Position.Bias.Forward), 2);
    harness.check(list.getNextMatch("", 3, Position.Bias.Forward), 3);
    harness.check(list.getNextMatch("", 4, Position.Bias.Forward), 4);
    harness.check(list.getNextMatch("", 5, Position.Bias.Forward), 5);
    harness.check(list.getNextMatch("", 6, Position.Bias.Forward), 6);
    harness.check(list.getNextMatch("", 7, Position.Bias.Forward), 7);
    
    harness.check(list.getNextMatch("D", 0, Position.Bias.Forward), -1);

    // try null string
    boolean pass = false;
    try
    {
      list.getNextMatch(null, 0, Position.Bias.Forward);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);

    // try negative index
    pass = false;
    try
    {
      list.getNextMatch("A", -1, Position.Bias.Forward);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try index == list size
    pass = false;
    try
    {
      list.getNextMatch("A", 9, Position.Bias.Forward);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
  }
}
