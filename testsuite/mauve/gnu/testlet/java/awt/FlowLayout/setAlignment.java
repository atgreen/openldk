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

package gnu.testlet.java.awt.FlowLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.FlowLayout;

/**
 * Basic checks for the setAlignment() method in the {@link FlowLayout} class.  
 */
public class setAlignment implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    FlowLayout layout = new FlowLayout();
    layout.setAlignment(FlowLayout.LEFT);
    harness.check(layout.getAlignment(), FlowLayout.LEFT);
    layout.setAlignment(FlowLayout.RIGHT);
    harness.check(layout.getAlignment(), FlowLayout.RIGHT);
    layout.setAlignment(FlowLayout.CENTER);
    harness.check(layout.getAlignment(), FlowLayout.CENTER);
  }

}
