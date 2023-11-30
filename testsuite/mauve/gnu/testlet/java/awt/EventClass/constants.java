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

package gnu.testlet.java.awt.EventClass;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Event;

/**
 * Verifies constant values for the {@link Event} class.
 */
public class constants implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)     
  {
    harness.check(Event.ACTION_EVENT,	1001);
    harness.check(Event.ALT_MASK,	8);
    harness.check(Event.BACK_SPACE, 8);
    harness.check(Event.CAPS_LOCK, 1022);
    harness.check(Event.CTRL_MASK, 2);
    harness.check(Event.DELETE, 127);
    harness.check(Event.DOWN, 1005);
    harness.check(Event.END, 1001);
    harness.check(Event.ENTER, 10);
    harness.check(Event.ESCAPE, 27);
    harness.check(Event.F1, 1008);
    harness.check(Event.F10, 1017);
    harness.check(Event.F11, 1018);
    harness.check(Event.F12, 1019);
    harness.check(Event.F2, 1009);
    harness.check(Event.F3, 1010);
    harness.check(Event.F4, 1011);
    harness.check(Event.F5, 1012);
    harness.check(Event.F6, 1013);
    harness.check(Event.F7, 1014);
    harness.check(Event.F8, 1015);
    harness.check(Event.F9, 1016);
    harness.check(Event.GOT_FOCUS, 1004);
    harness.check(Event.HOME, 1000);
    harness.check(Event.INSERT, 1025);
    harness.check(Event.KEY_ACTION, 403);
    harness.check(Event.KEY_ACTION_RELEASE, 404);
    harness.check(Event.KEY_PRESS, 401);
    harness.check(Event.KEY_RELEASE, 402);
    harness.check(Event.LEFT,	1006);
    harness.check(Event.LIST_DESELECT, 702);
    harness.check(Event.LIST_SELECT, 701);
    harness.check(Event.LOAD_FILE, 1002);
    harness.check(Event.LOST_FOCUS, 1005);
    harness.check(Event.META_MASK, 4);
    harness.check(Event.MOUSE_DOWN, 501);
    harness.check(Event.MOUSE_DRAG, 506);
    harness.check(Event.MOUSE_ENTER, 504);
    harness.check(Event.MOUSE_EXIT, 505);
    harness.check(Event.MOUSE_MOVE, 503);
    harness.check(Event.MOUSE_UP, 502);
    harness.check(Event.NUM_LOCK, 1023);
    harness.check(Event.PAUSE, 1024);
    harness.check(Event.PGDN, 1003);
    harness.check(Event.PGUP, 1002);
    harness.check(Event.PRINT_SCREEN, 1020);
    harness.check(Event.RIGHT, 1007);
    harness.check(Event.SAVE_FILE, 1003);
    harness.check(Event.SCROLL_ABSOLUTE, 605);
    harness.check(Event.SCROLL_BEGIN,	606);
    harness.check(Event.SCROLL_END, 607);
    harness.check(Event.SCROLL_LINE_DOWN, 602);
    harness.check(Event.SCROLL_LINE_UP, 601);
    harness.check(Event.SCROLL_LOCK, 1021);
    harness.check(Event.SCROLL_PAGE_DOWN,	604);
    harness.check(Event.SCROLL_PAGE_UP, 603);
    harness.check(Event.SHIFT_MASK, 1);
    harness.check(Event.TAB, 9);
    harness.check(Event.UP, 1004);
    harness.check(Event.WINDOW_DEICONIFY, 204);
    harness.check(Event.WINDOW_DESTROY, 201);
    harness.check(Event.WINDOW_EXPOSE, 202);
    harness.check(Event.WINDOW_ICONIFY, 203);
    harness.check(Event.WINDOW_MOVED, 205);
  }
}