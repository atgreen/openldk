// Tags: JDK1.4

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.AWTEvent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.AWTEvent;

/**
 * Verifies constant values for the {@link AWTEvent} class.
 */
public class constants implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.check(AWTEvent.ACTION_EVENT_MASK,	128l);
    harness.check(AWTEvent.ADJUSTMENT_EVENT_MASK,	256l);
    harness.check(AWTEvent.COMPONENT_EVENT_MASK, 1l);
    harness.check(AWTEvent.CONTAINER_EVENT_MASK, 2l);
    harness.check(AWTEvent.FOCUS_EVENT_MASK, 4l);
    harness.check(AWTEvent.HIERARCHY_BOUNDS_EVENT_MASK, 65536l);
    harness.check(AWTEvent.HIERARCHY_EVENT_MASK, 32768l);
    harness.check(AWTEvent.INPUT_METHOD_EVENT_MASK, 2048l);
    harness.check(AWTEvent.INVOCATION_EVENT_MASK, 16384l);
    harness.check(AWTEvent.ITEM_EVENT_MASK, 512l);
    harness.check(AWTEvent.KEY_EVENT_MASK, 8l);
    harness.check(AWTEvent.MOUSE_EVENT_MASK, 16l);
    harness.check(AWTEvent.MOUSE_MOTION_EVENT_MASK, 32l);
    harness.check(AWTEvent.MOUSE_WHEEL_EVENT_MASK, 131072l);
    harness.check(AWTEvent.PAINT_EVENT_MASK, 8192l);
    harness.check(AWTEvent.RESERVED_ID_MAX, 1999);
    harness.check(AWTEvent.TEXT_EVENT_MASK, 1024l);
    harness.check(AWTEvent.WINDOW_EVENT_MASK,	64l);
    harness.check(AWTEvent.WINDOW_FOCUS_EVENT_MASK, 524288l);
    harness.check(AWTEvent.WINDOW_STATE_EVENT_MASK, 262144l);
  }

}
