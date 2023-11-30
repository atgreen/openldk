/* paramString.java -- some checks for the paramString() method in the 
       JScrollBar class.
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
// Uses: MyJScrollBar

package gnu.testlet.javax.swing.JScrollBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * The return value from the paramString() method is allowed to vary by
 * implementation, but this checks for a match against the reference
 * implementation anyway.
 */
public class paramString implements Testlet
{
  public void test(TestHarness harness)
  {
    MyJScrollBar scrollBar = new MyJScrollBar();
    harness.check(scrollBar.paramString().endsWith(
            ",blockIncrement=10,orientation=VERTICAL,unitIncrement=1"));
  }
}