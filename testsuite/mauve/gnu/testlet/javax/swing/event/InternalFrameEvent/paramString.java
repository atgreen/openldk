/* paramString.java -- some checks for the paramString() method in the
       InternalFrameEvent class.
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

package gnu.testlet.javax.swing.event.InternalFrameEvent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JInternalFrame;
import javax.swing.event.InternalFrameEvent;

public class paramString implements Testlet
{
  public void test(TestHarness harness)
  {
    JInternalFrame f = new JInternalFrame("Title");
    InternalFrameEvent e = new InternalFrameEvent(f, 
            InternalFrameEvent.INTERNAL_FRAME_ACTIVATED);
    harness.check(e.paramString(), "INTERNAL_FRAME_ACTIVATED");
    e = new InternalFrameEvent(f, InternalFrameEvent.INTERNAL_FRAME_CLOSED);
    harness.check(e.paramString(), "INTERNAL_FRAME_CLOSED"); 
    e = new InternalFrameEvent(f, InternalFrameEvent.INTERNAL_FRAME_CLOSING);
    harness.check(e.paramString(), "INTERNAL_FRAME_CLOSING");  
    e = new InternalFrameEvent(f, InternalFrameEvent.INTERNAL_FRAME_DEACTIVATED);
    harness.check(e.paramString(), "INTERNAL_FRAME_DEACTIVATED");  
    e = new InternalFrameEvent(f, InternalFrameEvent.INTERNAL_FRAME_DEICONIFIED);
    harness.check(e.paramString(), "INTERNAL_FRAME_DEICONIFIED");  
    e = new InternalFrameEvent(f, InternalFrameEvent.INTERNAL_FRAME_ICONIFIED);
    harness.check(e.paramString(), "INTERNAL_FRAME_ICONIFIED");  
    e = new InternalFrameEvent(f, InternalFrameEvent.INTERNAL_FRAME_OPENED);
    harness.check(e.paramString(), "INTERNAL_FRAME_OPENED");  
    e = new InternalFrameEvent(f, InternalFrameEvent.RESERVED_ID_MAX);
    harness.check(e.paramString(), "unknown type");  
  }
}
