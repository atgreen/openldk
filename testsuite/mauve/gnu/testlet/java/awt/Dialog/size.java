// Tags: GUI JDK1.1

// Copyright (C) 2005 Red Hat

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.Dialog;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Dimension;
import java.awt.Dialog;
import java.awt.Frame;
import java.awt.Label;

public class size implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    Dialog jd = new Dialog(new Frame());
    jd.show();
    jd.add(new Label("Hello world"));

    // jd insets may be larger than preferred size
    Dimension pref = jd.getPreferredSize();
    Dimension size = jd.getSize();
    System.err.println("size: " + size);
    System.err.println("pref: " + pref);
    harness.check(size.width >= pref.width);
    harness.check(size.height >= pref.height);
  }
}
