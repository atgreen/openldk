// Tags: not-a-test

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.javax.swing.RepaintManager;

import java.awt.AWTEvent;
import java.awt.EventQueue;

/**
 * A special EventQueue used for testing purposes. It completely disables
 * dispatching of events, so the behaviour of the RepaintManager can be
 * examined more closely.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class DisabledEventQueue extends EventQueue
{
  /**
   * Overridden to do nothing.
   */
  protected void dispatchEvent(AWTEvent ev)
  {
    // Do nothing.
  }

  /**
   * Overridden to do nothing.
   */
  public void postEvent(AWTEvent ev)
  {
    // Do nothing.
  }
}
