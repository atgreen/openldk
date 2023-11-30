/* TestView.java -- A concrete View implementation for testing
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

package gnu.testlet.javax.swing.text.View;

import java.awt.Graphics;
import java.awt.Shape;

import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import javax.swing.text.View;
import javax.swing.text.Position.Bias;

/**
 * A concrete View implementation for testing.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class TestView extends View
{

  /**
   * This will be returned by getPreferredSpan for X_AXIS or Y_AXIS.
   */
  public float preferred[] = new float[]{ 100F, 100F };

  /**
   * Creates a new test view by fetching an element from PlainView.
   */
  public TestView()
  {
    super(createDummyElement());
  }

  /**
   * Creates a dummy element for testing.
   *
   * @return a dummy element for testing
   */
  static Element createDummyElement()
  {
    PlainDocument doc = new PlainDocument();
    return doc.getDefaultRootElement();
  }

  public void paint(Graphics g, Shape s)
  {
    // Not needed in testing.
  }

  /**
   * Implemented to return the values in above array.
   */
  public float getPreferredSpan(int axis)
  {
    return preferred[axis];
  }

  public Shape modelToView(int pos, Shape a, Bias b)
      throws BadLocationException
  {
    // Not used in testing.
    return null;
  }

  public int viewToModel(float x, float y, Shape a, Bias[] b)
  {
    // Not used in testing.
    return 0;
  }

}
