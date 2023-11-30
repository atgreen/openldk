// Tags: JDK1.4

// Copyright (C) 2011 Pavel Tisnovsky <ptisnovs@redhat.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.GridLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.GridLayout;

/**
 * Test of method {@link GridLayout#toString()}
 */
public class toString
  implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    // test constructor without any parameter
    GridLayout gridLayout1 = new GridLayout();
    gridLayout1.toString();
    harness.check(gridLayout1.toString(), "java.awt.GridLayout[hgap=0,vgap=0,rows=1,cols=0]");

    // test constructor with two parameters
    GridLayout gridLayout2 = new GridLayout(50, 50);
    gridLayout2.toString();
    harness.check(gridLayout2.toString(), "java.awt.GridLayout[hgap=0,vgap=0,rows=50,cols=50]");

    // test constructor with four parameters
    GridLayout gridLayout3 = new GridLayout(50, 50, 10, 20);
    gridLayout3.toString();
    harness.check(gridLayout3.toString(), "java.awt.GridLayout[hgap=10,vgap=20,rows=50,cols=50]");
  }
}
