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

package gnu.testlet.java.awt.dnd.DropTarget;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Button;
import java.awt.datatransfer.FlavorMap;
import java.awt.datatransfer.SystemFlavorMap;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.util.TooManyListenersException;

public class Constructors implements Testlet
{
  TestHarness h;
  boolean caught = false;
  DropTarget dt;
  DropTarget oldDt;
  FlavorMap fm;
  Button b = new Button();
  
  public void test(TestHarness harness)
  {
    h = harness;
    testNoParam();
    testTwoParams();
    testThreeParams();
    testFourParams();
    testFiveParams();
  }
  
  public void testNoParam()
  {
    dt = new DropTarget();
    oldDt = dt;
    h.check(dt.getComponent() == null);
    h.check(dt.getDefaultActions() == DnDConstants.ACTION_COPY_OR_MOVE);
    
    try
    {
      dt.addDropTargetListener(null);
    }
    catch (TooManyListenersException tmle)
    {
      caught = true;
    }
    
    h.check(!caught);
    h.check(dt.isActive());
    
    fm = dt.getFlavorMap();
    h.check(fm != null);
    h.check((fm instanceof SystemFlavorMap));
    
    caught = false;
  }
  
  public void testTwoParams()
  {    
    dt = new DropTarget(b, oldDt);
    h.check(dt.getComponent().equals(b));
    h.check(dt.getDefaultActions() == DnDConstants.ACTION_COPY_OR_MOVE);
    
    try
    {
      dt.addDropTargetListener(dt);
    }
    catch (IllegalArgumentException iae)
    {
      caught = true;
    }
    catch (TooManyListenersException tmle)
    {
      caught = false;
    }
    h.check(caught);
    caught = false;
    
    try
    {
      dt.addDropTargetListener(oldDt);
    }
    catch (TooManyListenersException tmle)
    {
      caught = true;
    }
    h.check(caught);
    caught = false;
    
    h.check(dt.isActive());
    
    fm = dt.getFlavorMap();
    h.check(fm != null);
    h.check((fm instanceof SystemFlavorMap));
  }
  
  public void testThreeParams()
  {    
    dt = new DropTarget(b, DnDConstants.ACTION_COPY_OR_MOVE, oldDt);
    h.check(dt.getComponent().equals(b));
    h.check(dt.getDefaultActions() == DnDConstants.ACTION_COPY_OR_MOVE);
    
    try
    {
      dt.addDropTargetListener(dt);
    }
    catch (IllegalArgumentException iae)
    {
      caught = true;
    }
    catch (TooManyListenersException tmle)
    {
      caught = false;
    }
    h.check(caught);
    caught = false;
    
    try
    {
      dt.addDropTargetListener(oldDt);
    }
    catch (TooManyListenersException tmle)
    {
      caught = true;
    }
    h.check(caught);
    caught = false;
    
    h.check(dt.isActive());
    
    fm = dt.getFlavorMap();
    h.check(fm != null);
    h.check((fm instanceof SystemFlavorMap));
  }
  
  public void testFourParams()
  {    
    dt = new DropTarget(b, DnDConstants.ACTION_COPY_OR_MOVE, oldDt, false);
    h.check(dt.getComponent().equals(b));
    h.check(dt.getDefaultActions() == DnDConstants.ACTION_COPY_OR_MOVE);
    
    try
    {
      dt.addDropTargetListener(dt);
    }
    catch (IllegalArgumentException iae)
    {
      caught = true;
    }
    catch (TooManyListenersException tmle)
    {
      caught = false;
    }
    h.check(caught);
    caught = false;
    
    try
    {
      dt.addDropTargetListener(oldDt);
    }
    catch (TooManyListenersException tmle)
    {
      caught = true;
    }
    h.check(caught);
    caught = false;
    
    h.check(!dt.isActive());
    
    fm = dt.getFlavorMap();
    h.check(fm != null);
    h.check((fm instanceof SystemFlavorMap));
  }
  
  public void testFiveParams()
  {    
    fm = SystemFlavorMap.getDefaultFlavorMap();
    dt = new DropTarget(b, DnDConstants.ACTION_COPY_OR_MOVE, oldDt, false, fm);
    h.check(dt.getComponent().equals(b));
    h.check(dt.getDefaultActions() == DnDConstants.ACTION_COPY_OR_MOVE);
    
    try
    {
      dt.addDropTargetListener(dt);
    }
    catch (IllegalArgumentException iae)
    {
      caught = true;
    }
    catch (TooManyListenersException tmle)
    {
      caught = false;
    }
    h.check(caught);
    caught = false;
    
    try
    {
      dt.addDropTargetListener(oldDt);
    }
    catch (TooManyListenersException tmle)
    {
      caught = true;
    }
    h.check(caught);
    caught = false;
    
    h.check(!dt.isActive());
    h.check(dt.getFlavorMap().equals(fm));
  }
}
