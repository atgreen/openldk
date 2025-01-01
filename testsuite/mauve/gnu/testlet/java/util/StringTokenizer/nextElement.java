// Tags: JDK1.0

// Copyright (C) 2005 David Gilbert (david.gilbert@object-refinery.com)

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.util.StringTokenizer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.NoSuchElementException;
import java.util.StringTokenizer;

/**
 * Some checks for the nextElement() method.
 */
public class nextElement implements Testlet
{

  /**
   * Runs the tests.
   * 
   * @param harness  the test harness.
   */
  public void test(TestHarness harness)
  {
    StringTokenizer t = new StringTokenizer("one two three");
    harness.check(t.nextElement(), "one");
    harness.check(t.nextElement(), "two");
    harness.check(t.nextElement(), "three");
 
    boolean pass = false;
    try
    {
      t.nextElement();   
    }
    catch (NoSuchElementException e) 
    {
      pass = true;   
    }
    harness.check(pass);
 
    // try with multiple delimiters
    t = new StringTokenizer("one two-three", "- ");
    harness.check(t.nextElement(), "one");
    harness.check(t.nextElement(), "two");
    harness.check(t.nextElement(), "three");
    pass = false;
    try
    {
      t.nextToken();   
    }
    catch (NoSuchElementException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

}