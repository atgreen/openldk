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
 * Some checks for the constructors in the {@link StringTokenizer} class.
 */
public class constructors implements Testlet
{

  /**
   * Runs the checks.
   * 
   * @param harness  the test harness.
   */
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
  }
  
  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("StringTokenizer(String)");
    StringTokenizer st = new StringTokenizer("one two\tthree\nfour\rfive\fsix");
    harness.check(st.nextToken(), "one");
    harness.check(st.nextToken(), "two");
    harness.check(st.nextToken(), "three");
    harness.check(st.nextToken(), "four");
    harness.check(st.nextToken(), "five");
    harness.check(st.nextToken(), "six");
    
    boolean pass = false;
    try
    {
      st.nextToken();
    }
    catch (NoSuchElementException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // null argument - see 1.5 specification
    pass = false;
    try
    {
      st = new StringTokenizer(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);
    
    // try empty string
    st = new StringTokenizer("");
    harness.check(!st.hasMoreElements());
    
  }
  
  private void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("StringTokenizer(String, String)");
    StringTokenizer st = new StringTokenizer("one twoXthreeYfour", " XY");
    harness.check(st.nextToken(), "one");
    harness.check(st.nextToken(), "two");
    harness.check(st.nextToken(), "three");
    harness.check(st.nextToken(), "four");
    
    boolean pass = false;
    try
    {
      st.nextToken();
    }
    catch (NoSuchElementException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // null first argument
    pass = false;
    try
    {
      st = new StringTokenizer(null, " ");
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

    // null second argument - here is a case of the spec being written
    // to match the implementation, it says (in 1.5) NO exception should be 
    // thrown, but NullPointerException may follow on other operations.
    pass = false;
    try
    {
      st = new StringTokenizer("ABC DEFG", null);
      try
      {
        /* String s = */ st.nextToken();
      }
      catch (NullPointerException e)
      {
        pass = true;   
      }
    }
    catch (NullPointerException e)
    {
      // failed - even though this makes sense
    }
    harness.check(pass);
  }
  
  private void testConstructor3(TestHarness harness) 
  {
    harness.checkPoint("StringTokenizer(String, String, boolean)");
    
    // try with flag = true
    StringTokenizer st = new StringTokenizer("A BCXDEFYYGHI", " XY", true);
    harness.check(st.nextToken(), "A");
    harness.check(st.nextToken(), " ");
    harness.check(st.nextToken(), "BC");
    harness.check(st.nextToken(), "X");
    harness.check(st.nextToken(), "DEF");
    harness.check(st.nextToken(), "Y");
    harness.check(st.nextToken(), "Y");
    harness.check(st.nextToken(), "GHI");
    harness.check(!st.hasMoreElements());

    // try with flag = false
    st = new StringTokenizer("A BCXDEFYYGHI", " XY", false);
    harness.check(st.nextToken(), "A");
    harness.check(st.nextToken(), "BC");
    harness.check(st.nextToken(), "DEF");
    harness.check(st.nextToken(), "GHI");
    harness.check(!st.hasMoreElements());
  
    // null first argument
    boolean pass = false;
    try
    {
      st = new StringTokenizer(null, " ", true);
    }
    catch (NullPointerException e) 
    {
      pass = true;   
    }
    harness.check(pass);

  }
  

}