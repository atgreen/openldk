/* getFlowAxis.java -- Tests FlowView.getFlowAxis()
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com
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
// Uses: TestFlowView FlowStrategy/TestView

package gnu.testlet.javax.swing.text.FlowView;

import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.View;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the FlowView.getFlowAxis() method.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getFlowAxis implements Testlet
{

  /**
   * The entry point into the test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    DefaultStyledDocument doc = new DefaultStyledDocument();
    Element el = doc.new BranchElement(null, null);
    TestFlowView v = new TestFlowView(el, View.X_AXIS);
    harness.check(v.getFlowAxis(), View.Y_AXIS);
    v = new TestFlowView(el, View.Y_AXIS);
    harness.check(v.getFlowAxis(), View.X_AXIS);
  }
}
