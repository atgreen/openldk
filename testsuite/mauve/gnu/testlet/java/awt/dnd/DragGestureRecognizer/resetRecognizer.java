/* resetRecognizer.java -- Tests DragGestureRecognizer.resetRecognizer()
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

package gnu.testlet.java.awt.dnd.DragGestureRecognizer;

import java.awt.Component;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragSource;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import junit.framework.TestCase;

/**
 * Tests DragGestureRecognizer.resetRecognizer().
 */
public class resetRecognizer extends TestCase
{

  static class TestRecognizer
    extends DragGestureRecognizer
  {

    protected TestRecognizer(DragSource s, Component c, int sa,
                             DragGestureListener l)
    {
      super(s, c, sa, l);
    }

    protected void registerListeners()
    {
    }

    protected void unregisterListeners()
    {
    }

    Component getFieldComponent()
    {
      return component;
    }
    DragGestureListener getFieldDragGestureListener()
    {
      return dragGestureListener; 
    }
    DragSource getFieldDragSource()
    {
      return dragSource;
    }
    ArrayList getFieldEvents()
    {
      return events;
    }
    int getFieldSourceActions()
    {
      return sourceActions;
    }
  }

  /**
   * Checks the state of the class before and after a call to
   * resetRecognizer().
   */
  public void testSimple()
  {
    DragSource s = new DragSource();
    Component c = new Component(){};
    DragGestureListener l = new DragGestureListener()
    {

      public void dragGestureRecognized(DragGestureEvent e)
      {
        // TODO Auto-generated method stub
        
      }
    
    };
    TestRecognizer rec = new TestRecognizer(s, c, DnDConstants.ACTION_MOVE, l);
    assertEquals(s, rec.getFieldDragSource());
    assertEquals(c, rec.getFieldComponent());
    assertEquals(l, rec.getFieldDragGestureListener());
    assertEquals(DnDConstants.ACTION_MOVE, rec.getFieldSourceActions());

    ArrayList ev = rec.getFieldEvents();
    ev.add(new MouseEvent(c, MouseEvent.MOUSE_DRAGGED,
                          System.currentTimeMillis(), MouseEvent.ALT_DOWN_MASK,
                          10, 20, 1, false));
    ev.add(new MouseEvent(c, MouseEvent.MOUSE_DRAGGED,
                          System.currentTimeMillis(), MouseEvent.ALT_DOWN_MASK,
                          10, 20, 1, false));
    assertEquals(2, ev.size());
    rec.resetRecognizer();
    assertSame(ev, rec.getFieldEvents());
    assertEquals(0, ev.size());
    assertEquals(s, rec.getFieldDragSource());
    assertEquals(c, rec.getFieldComponent());
    assertEquals(l, rec.getFieldDragGestureListener());
    assertEquals(DnDConstants.ACTION_MOVE, rec.getFieldSourceActions());
  }
}
