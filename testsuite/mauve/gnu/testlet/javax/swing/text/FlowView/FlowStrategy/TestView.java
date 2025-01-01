/* TestView.java -- A mock view for testing
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

package gnu.testlet.javax.swing.text.FlowView.FlowStrategy;

import java.awt.Graphics;
import java.awt.Shape;

import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.View;
import javax.swing.text.Position.Bias;


public class TestView extends View
{

  public int preferred = 100;

  public int breakWeight = View.BadBreakWeight;

  public TestView break1;
  public TestView break2;

  public TestView(Element el)
  {
    super(el);
  }

  public void paint(Graphics g, Shape s)
  {
    // Not needed in testing.
  }

  public float getMinimumSpan(int axis)
  {
    return getPreferredSpan(axis);
  }
  public float getPreferredSpan(int axis)
  {
    return preferred;
  }

  public Shape modelToView(int pos, Shape a, Bias b)
      throws BadLocationException
  {
    // Not needed in testing.
    return null;
  }

  public int viewToModel(float x, float y, Shape a, Bias[] b)
  {
    // Not needed in testing.
    return 0;
  }

  /**
   * This is implemented to return <code>breakWeight</code> when pos + len >
   * <code>preferred / 2</code>, that means it returns the break weight
   * specified by the testing code when the desired break location is beyond
   * the middle of the view.
   */
  public int getBreakWeight(int axis, float pos, float len)
  {
    if (pos + len > preferred)
      return breakWeight;
    else
      return View.BadBreakWeight;
  }

  public View breakView(int axis, int offset, float pos, float len)
  {
    break1 = new TestView(getElement());
    break1.preferred = preferred / 2;
    break2 = new TestView(getElement());
    break2.preferred = preferred / 2;
    if (offset < preferred / 2)
      return break1;
    else
      return break2;
  }
}
