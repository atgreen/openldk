/* clone.java -- Some checks for the clone() method in the Segment class.
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

public class clone implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    char[] ch = new char[] {'A', 'B', 'C'};
    Segment s1 = new Segment(ch, 0, 3);
    Segment s2 = (Segment) s1.clone();
    harness.check(!s1.equals(s2));
    harness.check(s2.offset, 0);
    harness.check(s2.count, 3);
    harness.check(s2.array, ch);
    
    // offset is independent
    s1.offset = 1;
    harness.check(s2.offset, 0);
    s2.offset = 1;
    harness.check(s2.offset, 1);
    
    // count is independent
    s1.count = 2;
    harness.check(s2.count, 3);
    s2.count = 2;
    harness.check(s2.count, 2);
    
    // array is a shallow copy and not independent
    s1.array[1] = 'X';
    harness.check(s2.array[1], 'X');
    
    char[] ch2 = new char[] {'X', 'Y', 'Z'};
    s1.array = ch2;
    
    // s2 still points to ch
    harness.check(s2.array, ch);
  }

}