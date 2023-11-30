/* Constructors.java --
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

// Tags: FIXME

package gnu.testlet.java.awt.dnd.DropTargetDropEvent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Button;
import java.awt.Point;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetContext;
import java.awt.dnd.DropTargetDropEvent;

public class Constructors
    implements Testlet
{
  Button b = new Button("button");
  DropTarget dt = new DropTarget(b, null);
  DropTargetContext dtc = dt.getDropTargetContext();
  Point loc = new Point();
  boolean caught;
  
  public synchronized void test(TestHarness h)
  {
    testNullLoc(h);
    testDropAction(h);
    testSrcAction(h);
    testNormal(h);
  }
  
  void testNullLoc(TestHarness h)
  {
    caught = false;
    try
      {
        new DropTargetDropEvent(dtc, null, DnDConstants.ACTION_COPY,
                                DnDConstants.ACTION_COPY, false);
      }
    catch (NullPointerException npe)
      {
        caught = true;
      }
    h.check(caught);
  }
  
  void testDropAction(TestHarness h)
  {
    caught = false;
    try
      {
        new DropTargetDropEvent(dtc, loc, 4, DnDConstants.ACTION_COPY, false);
      }
    catch (IllegalArgumentException iae)
      {
        caught = true;
      }
    h.check(caught);
  }
  
  void testSrcAction(TestHarness h)
  {
    caught = false;
    try
      {
        new DropTargetDropEvent(dtc, loc, DnDConstants.ACTION_COPY, 4,false);
      }
    catch (IllegalArgumentException iae)
      {
        caught = true;
      }
    h.check(caught);
  }
  
  void testNormal(TestHarness h)
  {
    caught = false;
    DropTargetDropEvent dtde = null;
    try
      {
        dtde = new DropTargetDropEvent(dtc, loc, DnDConstants.ACTION_COPY,
                                DnDConstants.ACTION_COPY, true);
      }
    catch (Exception e)
      {
        caught = true;
      }
    
    h.check(!caught);    
    h.check(dtde != null && dtde.getDropAction() == DnDConstants.ACTION_COPY);
    h.check(dtde != null && dtde.getSourceActions() == DnDConstants.ACTION_COPY);
    h.check(dtde != null && dtde.getLocation().equals(loc));
    h.check(dtde != null && dtde.isLocalTransfer());
  }
}
