/* TestFlowView.java -- A concrete FlowView for testing
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

// Tags: not-a-test

package gnu.testlet.javax.swing.text.FlowView;

import gnu.testlet.javax.swing.text.FlowView.FlowStrategy.TestView;

import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.Element;
import javax.swing.text.FlowView;
import javax.swing.text.View;
import javax.swing.text.Position.Bias;

/**
 * A concrete subclass of FlowView that can be used to test the FlowView.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class TestFlowView extends FlowView
{

  public class TestFlowStrategy extends FlowStrategy
  {
    public void adjustRow(FlowView fv, int rowIndex, int desiredSpan, int x)
    {
      super.adjustRow(fv, rowIndex, desiredSpan, x);
    }
  }

  public class TestRow extends BoxView
  {
    public int preferred = 200;
    public TestRow break1;
    public TestRow break2;
    public int breakWeight = View.BadBreakWeight;

    public TestRow(Element el)
    {
      super(el, View.X_AXIS);
    }

    public float getPreferredSpan(int axis)
    {
      System.err.println("preferredSpan called");
      return preferred;
    }

    public float getMinimumSpan(int axis)
    {
      System.err.println("minimumSpan called");
      return preferred;
    }

    /**
     * This is implemented to return <code>breakWeight</code> when pos + len >
     * <code>preferred / 2</code>, that means it returns the break weight
     * specified by the testing code when the desired break location is beyond
     * the middle of the view.
     */
    public int getBreakWeight(int axis, float pos, float len)
    {System.err.println("getBreakWeight called");
      if (pos + len > preferred)
        return breakWeight;
      else
        return View.BadBreakWeight;
    }

    public View breakView(int axis, int offset, float pos, float len)
    {System.err.println("breakView called");
      break1 = new TestRow(getElement());
      break1.preferred = preferred / 2;
      break2 = new TestRow(getElement());
      break2.preferred = preferred / 2;
      if (offset < preferred / 2)
        return break1;
      else
        return break2;
    }

  }

  /**
   * Constructs a new TestFlowView by calling super.
   *
   * @param element the element
   * @param axis the axis
   */
  public TestFlowView(Element element, int axis)
  {
    super(element, axis);
    strategy = new TestFlowStrategy();
  }

  /**
   * Creates a row.
   *
   * @return a view for one row
   */
  public View createRow()
  {
    return new TestRow(getElement());
  }

  public TestFlowStrategy getFlowStragy()
  {
    return (TestFlowStrategy) strategy;
  }
}
