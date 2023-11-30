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

package gnu.testlet.java.awt.Button;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Button;

// Button constructors test
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
    Button button1 = new Button();
    harness.check(button1 != null);
    doCommonTests(harness, button1);

    // test one parameter constructor
    Button button2 = new Button("xyzzy");
    harness.check(button2 != null);
    harness.check(button2.getLabel(), "xyzzy");
    doCommonTests(harness, button2);
  }

  public void doCommonTests(TestHarness harness, Button button)
  {
    // label checks
    button.setLabel("42");
    harness.check(button.getLabel(), "42");
    button.setLabel("");
    harness.check(button.getLabel(), "");
    button.setLabel(null);
    harness.check(button.getLabel() == null);
  }
}

