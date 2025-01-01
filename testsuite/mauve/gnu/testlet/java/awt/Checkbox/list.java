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

package gnu.testlet.java.awt.Checkbox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Checkbox;

// test of method list()
public class list
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
    Checkbox checkbox1 = new Checkbox();
    checkbox1.list();
    checkbox1.list(System.out);
    checkbox1.list(System.out, 0);
    checkbox1.list(System.out, 10);

    // test constructor with one parameter
    Checkbox checkbox2 = new Checkbox("xyzzy");
    checkbox2.list();
    checkbox2.list(System.out);
    checkbox2.list(System.out, 0);
    checkbox2.list(System.out, 10);
  }
}

