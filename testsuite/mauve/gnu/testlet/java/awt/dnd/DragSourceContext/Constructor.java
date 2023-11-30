/* Constructor.java -- 
   Copyright (C) 2006 Red Hat
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

package gnu.testlet.java.awt.dnd.DragSourceContext;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceContext;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.awt.image.BufferedImage;
import java.util.ArrayList;


public class Constructor implements Testlet
{
  TestHarness harness;
  DragGestureEvent trigger = null;
  DragSourceContextPeer peer = null;
  Cursor cur = Cursor.getDefaultCursor();
  StringSelection ss = new StringSelection("data");
  boolean caught = false;
  
  public void test(TestHarness harness)
  {
    this.harness = harness;
    testNPE();
    testIllegalArgumentException();
  }
  
  void testNPE()
  {
    try
      {
        new DragSourceContext(null, null, null, null, null, null, null);
      }
    catch (NullPointerException npe)
      {
        caught = true;
      }
    harness.check(caught);
    caught = false;

    try
      {
        trigger = new DragGestureEvent(new DGR(), DnDConstants.ACTION_COPY,
                                       new Point(), new ArrayList());
        peer = Toolkit.getDefaultToolkit().createDragSourceContextPeer(trigger);
        new DragSourceContext(peer, trigger, cur, null, null, ss, null);
      }
    catch (NullPointerException npe)
      {
        caught = true;
      }
    catch (IllegalArgumentException iae)
      {
      }
    harness.check(!caught);
    caught = false;

    try
      {
        new DragSourceContext(peer, trigger, cur,
                              new BufferedImage(50, 50,
                                                BufferedImage.TYPE_3BYTE_BGR),
                              null, ss, null);
      }
    catch (NullPointerException npe)
      {
        caught = true;
      }
    harness.check(caught);
    caught = false;
  }
  
  void testIllegalArgumentException()
  {
    try
      {
        DGR dgr = new DGR();
        dgr.setComponent(null);
        trigger = new DragGestureEvent(dgr, DnDConstants.ACTION_COPY,
                                       new Point(), new ArrayList());
        peer = Toolkit.getDefaultToolkit().createDragSourceContextPeer(trigger);
        new DragSourceContext(peer, trigger, cur,
                              new BufferedImage(50, 50,
                                                BufferedImage.TYPE_3BYTE_BGR),
                              new Point(), ss, null);
      }
    catch (IllegalArgumentException iae)
      {
        caught = true;
      }
    harness.check(caught);
    caught = false;

    try
      {
        DGR dgr = new DGR(null);
        trigger = new DragGestureEvent(dgr, DnDConstants.ACTION_COPY,
                                       new Point(), new ArrayList());
        peer = Toolkit.getDefaultToolkit().createDragSourceContextPeer(trigger);
        new DragSourceContext(peer, trigger, cur,
                              new BufferedImage(50, 50,
                                                BufferedImage.TYPE_3BYTE_BGR),
                              new Point(), ss, null);
      }
    catch (IllegalArgumentException iae)
      {
        caught = true;
      }
    harness.check(caught);
    caught = false;

    try
      {
        DGR dgr = new DGR();
        trigger = new DragGestureEvent(dgr, DnDConstants.ACTION_NONE,
                                       new Point(), new ArrayList());
        peer = Toolkit.getDefaultToolkit().createDragSourceContextPeer(trigger);
        new DragSourceContext(peer, trigger, cur,
                              new BufferedImage(50, 50,
                                                BufferedImage.TYPE_3BYTE_BGR),
                              new Point(), ss, null);
      }
    catch (IllegalArgumentException iae)
      {
        caught = true;
      }
    harness.check(caught);
    caught = false;
  }
  
  class DGR extends DragGestureRecognizer
  {
    public DGR()
    {
      super(new DragSource());
    }
    
    public DGR(DragSource ds)
    {
      super(ds);
    }
    
    public void setComponent(Component c)
    {
      super.component = c;
    }
    
    public void registerListeners()
    {
    }
    
    public void unregisterListeners()
    {
    }
  }
}
