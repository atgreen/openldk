/* getIndex.java -- some checks for the getIndex() method in the SizeSequence
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.SizeSequence;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.SizeSequence;

public class getIndex implements Testlet
{
  public void test(TestHarness harness)
  {
    SizeSequence s = new SizeSequence(new int[] {1, 2, 3});
    harness.check(s.getIndex(0), 0);
    harness.check(s.getIndex(1), 1);
    harness.check(s.getIndex(2), 1);
    harness.check(s.getIndex(3), 2);
    harness.check(s.getIndex(4), 2);
    harness.check(s.getIndex(5), 2);
    harness.check(s.getIndex(6), 3);
    harness.check(s.getIndex(7), 3);
    harness.check(s.getIndex(99), 3);
    harness.check(s.getIndex(-1), 0);
    
    s = new SizeSequence();
    harness.check(s.getIndex(-1), 0);
    harness.check(s.getIndex(0), 0);
    harness.check(s.getIndex(1), 0);
  }
}
