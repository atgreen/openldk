/* adjustRow.java -- Tests the FlowStrategy.adjustRow() method
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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
// Uses: TestView ../TestFlowView

package gnu.testlet.javax.swing.text.FlowView.FlowStrategy;

import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.AbstractDocument.BranchElement;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.javax.swing.text.FlowView.TestFlowView;

/**
 * Tests the FlowStrategy.adjustRow() method.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class adjustRow implements Testlet
{

  /**
   * The entry point into this test.
   *
   * @param the test harness to use
   */
  public void test(TestHarness harness)
  {
    testUnbreakable(harness);
    testGoodBreakable(harness);
    testExcellentBreakable(harness);
    testForceBreakable(harness);
  }

  /**
   * This places one row into the FlowView and one view into that row that
   * is bigger than the desired span. However, since it is not breakable, this
   * will not be touched.
   *
   * @param h
   */
  private void testUnbreakable(TestHarness h)
  {
    DefaultStyledDocument doc = new DefaultStyledDocument();
    Element el = doc.new BranchElement(null, null);
    TestFlowView fv = new TestFlowView(el, View.Y_AXIS);
    // Create one row and fill it with one oversized testview.
    TestFlowView.TestRow row = (TestFlowView.TestRow) fv.createRow();
    fv.replace(0, 0, new View[]{row});
    row.preferred = 200;

    fv.getFlowStragy().adjustRow(fv, 0, 150, 0);
    h.check(fv.getView(0), row);
  }

  /**
   * This places one row into the FlowView and one view into that row that
   * is bigger than the desired span. However, since it is not breakable, this
   * will not be touched.
   *
   * @param h
   */
  private void testGoodBreakable(TestHarness h)
  {
    DefaultStyledDocument doc = new DefaultStyledDocument();
    Element el = doc.new BranchElement(null, null);
    TestFlowView fv = new TestFlowView(el, View.Y_AXIS);
    // Create one row and fill it with one oversized testview.
    TestFlowView.TestRow row = (TestFlowView.TestRow) fv.createRow();
    fv.replace(0, 0, new View[]{row});
    row.breakWeight = View.GoodBreakWeight;
    row.preferred = 200;
    fv.getFlowStragy().adjustRow(fv, 0, 150, 0);
    h.check(fv.getView(0), row);
  }

  /**
   * This places one row into the FlowView and one view into that row that
   * is bigger than the desired span. However, since it is not breakable, this
   * will not be touched.
   *
   * @param h
   */
  private void testExcellentBreakable(TestHarness h)
  {
    DefaultStyledDocument doc = new DefaultStyledDocument();
    Element el = doc.new BranchElement(null, null);
    TestFlowView fv = new TestFlowView(el, View.Y_AXIS);
    // Create one row and fill it with one oversized testview.
    TestFlowView.TestRow row = (TestFlowView.TestRow) fv.createRow();
    fv.replace(0, 0, new View[]{row});
    row.breakWeight = View.ExcellentBreakWeight;
    row.preferred = 200;
    fv.getFlowStragy().adjustRow(fv, 0, 150, 0);
    h.check(fv.getView(0), row);
  }

  /**
   * This places one row into the FlowView and one view into that row that
   * is bigger than the desired span. However, since it is not breakable, this
   * will not be touched.
   *
   * @param h
   */
  private void testForceBreakable(TestHarness h)
  {
    DefaultStyledDocument doc = new DefaultStyledDocument();
    Element el = doc.new BranchElement(null, null);
    TestFlowView fv = new TestFlowView(el, View.Y_AXIS);
    // Create one row and fill it with one oversized testview.
    TestFlowView.TestRow row = (TestFlowView.TestRow) fv.createRow();
    fv.replace(0, 0, new View[]{row});
    row.breakWeight = View.ForcedBreakWeight;
    row.preferred = 200;
    fv.getFlowStragy().adjustRow(fv, 0, 150, 0);
    h.check(fv.getView(0), row);
  }
}
