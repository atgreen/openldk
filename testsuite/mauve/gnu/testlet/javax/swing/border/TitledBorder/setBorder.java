// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.border.TitledBorder;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;

/**
 * Some checks for the setBorder() method of the {@link TitledBorder} class.
 */
public class setBorder implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)       
  {
    Border baseBorder = new EmptyBorder(1, 1, 1, 1);
    TitledBorder b = new TitledBorder(baseBorder);
    harness.check(b.getBorder(), baseBorder);
    baseBorder = new EmptyBorder(1, 2, 3, 4);
    b.setBorder(baseBorder);
    harness.check(b.getBorder(), baseBorder);
    b.setBorder(null);
    baseBorder = UIManager.getLookAndFeelDefaults().getBorder(
            "TitledBorder.border");
    harness.check(b.getBorder(), baseBorder);
  }

}
