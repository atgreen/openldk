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
 * Some checks for the nextToken() method.
*/
public class nextToken implements Testlet
{

  /**
   * Runs the test.
   * 
   * @param harness  the test harness.
   */
  public void test(TestHarness harness) 
  {
    test1(harness);
    test2(harness);
  }
  
  private void test1(TestHarness harness)
  {
    harness.checkPoint("nextToken()");
    StringTokenizer t = new StringTokenizer("one two three");
    harness.check(t.nextToken(), "one");
    harness.check(t.nextToken(), "two");
    harness.check(t.nextToken(), "three");
    
    boolean pass = false;
    try
    {
      t.nextToken();   
    }
    catch (NoSuchElementException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try with multiple delimiters
    t = new StringTokenizer("one two-three", "- ");
    harness.check(t.nextToken(), "one");
    harness.check(t.nextToken(), "two");
    harness.check(t.nextToken(), "three");
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
  
  private void test2(TestHarness harness)
  {
    harness.checkPoint("nextToken(String)");
    StringTokenizer t = new StringTokenizer("A BC-DEF GHI-JKL", " ");
    harness.check(t.nextToken(), "A");
    harness.check(t.nextToken("-"), " BC");
    harness.check(t.nextToken(), "DEF GHI");
    
    boolean pass = false;
    try
    {
      t.nextToken(null);   
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
  }

}