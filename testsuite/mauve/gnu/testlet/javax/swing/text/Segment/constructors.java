/* constructors.java -- Some checks for the constructors in the Segment
                        class.
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

public class constructors implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }
  
  public void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("()");
    Segment s = new Segment();
    harness.check(s.offset, 0);
    harness.check(s.count, 0);
    harness.check(s.array, null);
    harness.check(s.toString(), "");
  }
  
  public void testConstructor2(TestHarness harness) 
  {
    harness.checkPoint("(char[], int, int)");
    char[] ch = new char[] {'A', 'B', 'C'};
    Segment s = new Segment(ch, 1, 2);
    harness.check(s.offset, 1);
    harness.check(s.count, 2);
    harness.check(s.array, ch);
    harness.check(s.toString(), "BC");
    harness.check(s.getIndex(), 0);
    harness.check(s.getBeginIndex(), 1);
    harness.check(s.getEndIndex(), 3);
    
    // try offset out of range - this creates an instance with a bad state
    s = new Segment(ch, 4, 1);
    harness.check(s.offset, 4);
    harness.check(s.count, 1);
    harness.check(s.array, ch);
    
    // null array
    s = new Segment(null, 0, 1);
    harness.check(s.offset, 0);
    harness.check(s.count, 1);
    harness.check(s.array, null);
    
    // negative offsets
    s = new Segment(ch, -4, 1);
    harness.check(s.offset, -4);
    harness.check(s.count, 1);
    harness.check(s.array, ch);
    
  }

}