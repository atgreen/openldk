/* TestZoneView.java -- A ZoneView subclass for testing
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

import java.util.ArrayList;

import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.ZoneView;

public class TestZoneView extends ZoneView
{

  private static final Element DEFAULT_ELEMENT;
  static
  {
    PlainDocument doc = new PlainDocument();
    Element el = doc.getDefaultRootElement();
    DEFAULT_ELEMENT = el;
  }

  ArrayList lastUnloadedZones;

  TestZoneView()
  {
    this(X_AXIS);
  }

  TestZoneView(int axis)
  {
    this(DEFAULT_ELEMENT, axis);
  }

  TestZoneView(Element el, int axis)
  {
    super(el, axis);
    lastUnloadedZones = new ArrayList();
  }

  /**
   * Overridden to make method publicly accessible.
   */
  public void zoneWasLoaded(View zone)
  {
    super.zoneWasLoaded(zone);
  }

  public void unloadZone(View zone)
  {
    super.unloadZone(zone);
    lastUnloadedZones.add(zone);
  }

  public boolean isZoneLoaded(View z)
  {
    return super.isZoneLoaded(z);
  }

  public View createZone(int p0, int p1)
  {
    return super.createZone(p0, p1);
  }

  public int getViewIndexAtPosition(int pos)
  {
    return super.getViewIndexAtPosition(pos);
  }

  public void loadChildren(ViewFactory vf)
  {
    super.loadChildren(vf);
  }

  public ViewFactory getViewFactory()
  {
    return new ViewFactory()
    {

      public View create(Element elem)
      {
        return new TestView(elem, View.X_AXIS);
      }
      
    };
  }
}
