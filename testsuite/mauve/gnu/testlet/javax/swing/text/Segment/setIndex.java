/* setIndex.java -- Some checks for the setIndex() method in the Segment class.
   Copyright (C) 2006  David Gilbert <david.gilbert@object-refinery.com>
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

// Tags: 1.2

package gnu.testlet.javax.swing.text.Segment;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.Segment;

public class setIndex implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    char[] ch = new char[] {'A', 'B', 'C', 'D', 'E', 'F', 'G'};
    Segment s = new Segment(ch, 0, 7);
    s.setIndex(2);
    harness.check(s.current(), 'C');
    s.setIndex(6);
    harness.check(s.current(), 'G');
    
    // try bad indices
    boolean pass = false;
    try
    {
      s.setIndex(-1);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      s.setIndex(99);
    }
    catch (IllegalArgumentException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    
    // try results for a subset of the characters
    s = new Segment(ch, 3, 3);
    s.setIndex(4);
    harness.check(s.current(), 'E');
    s.setIndex(5);
    harness.check(s.current(), 'F');
    
    pass = false;
    try
    {
      s.setIndex(-1);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      s.setIndex(1);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      s.setIndex(7);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try
    {
      s.setIndex(99);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }

}