/* current.java -- Some checks for the current() method in the Segment class.
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

import java.text.CharacterIterator;

import javax.swing.text.Segment;

public class current implements Testlet
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
    harness.check(s.current(), 'A');
    s.last();
    harness.check(s.current(), 'G');
    s.next();
    harness.check(s.current(), CharacterIterator.DONE);
    s.first();
    harness.check(s.current(), 'A');
    s.previous();
    harness.check(s.current(), 'A');
    
    // try results for a subset of the characters
    s = new Segment(ch, 3, 3);
    harness.check(s.current(), 'A');
    s.last();
    harness.check(s.current(), 'F');
    s.next();
    harness.check(s.current(), CharacterIterator.DONE);
    s.first();
    harness.check(s.current(), 'D');
    s.previous();
    harness.check(s.current(), 'D');
  }

}