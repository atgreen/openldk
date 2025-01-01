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

/**
 * {@link Label} constructors test
 */
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
    Label label1 = new Label();
    harness.check(label1 != null);
    doCommonTests(harness, label1);

    // test one parameter constructor
    Label label2 = new Label("xyzzy");
    harness.check(label2 != null);
    harness.check(label2.getText(), "xyzzy");
    doCommonTests(harness, label2);

    // test two parameters constructor
    Label label3 = new Label("xyzzy", Label.CENTER);
    harness.check(label3 != null);
    harness.check(label3.getText(), "xyzzy");
    harness.check(label3.getAlignment(), Label.CENTER);
    doCommonTests(harness, label3);

    Label label4 = new Label("xyzzy", Label.LEFT);
    harness.check(label4 != null);
    harness.check(label4.getText(), "xyzzy");
    harness.check(label4.getAlignment(), Label.LEFT);
    doCommonTests(harness, label4);

    Label label5 = new Label("xyzzy", Label.RIGHT);
    harness.check(label5 != null);
    harness.check(label5.getText(), "xyzzy");
    harness.check(label5.getAlignment(), Label.RIGHT);
    doCommonTests(harness, label5);
  }

  public void doCommonTests(TestHarness harness, Label label)
  {
    // label text checks
    label.setText("42");
    harness.check(label.getText(), "42");
    label.setText("");
    harness.check(label.getText(), "");
    label.setText(null);
    harness.check(label.getText() == null);

    // alignment checks
    label.setAlignment(Label.CENTER);
    harness.check(label.getAlignment(), Label.CENTER);
    label.setAlignment(Label.LEFT);
    harness.check(label.getAlignment(), Label.LEFT);
    label.setAlignment(Label.RIGHT);
    harness.check(label.getAlignment(), Label.RIGHT);
  }
}

