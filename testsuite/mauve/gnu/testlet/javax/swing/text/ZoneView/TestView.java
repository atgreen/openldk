/* TestView.java -- A view for testing zones
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

package gnu.testlet.javax.swing.text.ZoneView;

import javax.swing.text.BoxView;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;

public class TestView extends BoxView
{
  
  private static final Element DEFAULT_ELEMENT;
  static
  {
    PlainDocument doc = new PlainDocument();
    Element el = doc.getDefaultRootElement();
    DEFAULT_ELEMENT = el;
  }

  boolean removeAllCalled;

  TestView()
  {
    this(X_AXIS);
  }

  TestView(int axis)
  {
    this(DEFAULT_ELEMENT, axis);
  }

  TestView(Element el, int axis)
  {
    super(el, axis);
  }

  public void removeAll()
  {
    super.removeAll();
    removeAllCalled = true;
  }
}
