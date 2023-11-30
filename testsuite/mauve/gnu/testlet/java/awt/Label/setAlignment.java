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

package gnu.testlet.java.awt.Label;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Label;

// Test of method setAlignment() for the Label class.
public class setAlignment
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
    Label label1 = new Label();
    doTest(harness, label1);

    // test constructor with one parameter
    Label label2 = new Label("xyzzy");
    doTest(harness, label2);

    // test constructor with two parameters
    Label label3 = new Label("xyzzy", Label.CENTER);
    doTest(harness, label3);

    // test constructor with two parameters
    Label label4 = new Label("xyzzy", Label.LEFT);
    doTest(harness, label4);

    // test constructor with two parameters
    Label label5 = new Label("xyzzy", Label.RIGHT);
    doTest(harness, label5);
  }

  /**
    * Check if method Label.setAlignment() is working properly.
    */
  public void doTest(TestHarness harness, Label label)
  {
    label.setAlignment(Label.LEFT);
    harness.check(label.getAlignment(), Label.LEFT);
    label.setAlignment(Label.RIGHT);
    harness.check(label.getAlignment(), Label.RIGHT);
    label.setAlignment(Label.CENTER);
    harness.check(label.getAlignment(), Label.CENTER);
  }
}

