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

// Checkbox constructors test
public class constructors
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
    harness.check(checkbox1 != null);
    harness.check(!checkbox1.getState());
    doCommonTests(harness, checkbox1);

    // test one parameter constructor
    Checkbox checkbox2 = new Checkbox("xyzzy");
    harness.check(checkbox2 != null);
    harness.check(checkbox2.getLabel(), "xyzzy");
    harness.check(!checkbox2.getState());
    doCommonTests(harness, checkbox2);

    // test two parameters constructor
    Checkbox checkbox3 = new Checkbox("xyzzy", false);
    harness.check(checkbox3 != null);
    harness.check(checkbox3.getLabel(), "xyzzy");
    harness.check(!checkbox3.getState());
    doCommonTests(harness, checkbox3);

    // test two parameters constructor
    Checkbox checkbox4 = new Checkbox("xyzzy", true);
    harness.check(checkbox4 != null);
    harness.check(checkbox4.getLabel(), "xyzzy");
    harness.check(checkbox4.getState());
    doCommonTests(harness, checkbox4);
  }

  public void doCommonTests(TestHarness harness, Checkbox checkbox)
  {
    // label checks
    checkbox.setLabel("42");
    harness.check(checkbox.getLabel(), "42");
    checkbox.setLabel("");
    harness.check(checkbox.getLabel(), "");
    checkbox.setLabel(" ");
    harness.check(checkbox.getLabel(), " ");
    checkbox.setLabel(null);
    harness.check(checkbox.getLabel() == null);

    // state checks
    checkbox.setState(true);
    harness.check(checkbox.getState());
    checkbox.setState(false);
    harness.check(!checkbox.getState());
  }
}

