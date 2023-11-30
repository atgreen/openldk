/* dispose.java -- some checks for the dispose() method in the 
       JInternalFrame class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.javax.swing.JInternalFrame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.List;

import javax.swing.JInternalFrame;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;

public class dispose implements Testlet, InternalFrameListener
{
  List events = new java.util.ArrayList();
  
  public void test(TestHarness harness)
  {
    JInternalFrame f = new JInternalFrame("F1");
    f.addInternalFrameListener(this);
    f.dispose();
    harness.check(f.isClosed());
    harness.check(!f.isSelected());
    harness.check(!f.isVisible());
    harness.check(events.size(), 1);
    InternalFrameEvent event = (InternalFrameEvent) events.get(0);
    harness.check(event.getSource(), f);
    harness.check(event.getID(), InternalFrameEvent.INTERNAL_FRAME_CLOSED);
  }

  public void internalFrameActivated(InternalFrameEvent event) {
    events.add(event);
  }

  public void internalFrameClosed(InternalFrameEvent event) {
    events.add(event);
  }

  public void internalFrameClosing(InternalFrameEvent event) {
    events.add(event);
  }

  public void internalFrameDeactivated(InternalFrameEvent event) {
    events.add(event);
  }

  public void internalFrameDeiconified(InternalFrameEvent event) {
    events.add(event);
  }

  public void internalFrameIconified(InternalFrameEvent event) {
    events.add(event);
  }

  public void internalFrameOpened(InternalFrameEvent event) {
    events.add(event);
  }
}
