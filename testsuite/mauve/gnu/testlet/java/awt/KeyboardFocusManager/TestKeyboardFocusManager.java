/* TestKeyboardFocusManager.java -- A helper class for testing
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

package gnu.testlet.java.awt.KeyboardFocusManager;

import java.awt.AWTEvent;
import java.awt.Component;
import java.awt.Container;
import java.awt.KeyboardFocusManager;
import java.awt.event.KeyEvent;

public class TestKeyboardFocusManager extends KeyboardFocusManager
{

  /**
   * Made public for testing.
   */
  public void setGlobalFocusOwner(Component c)
  {
    super.setGlobalFocusOwner(c);
  }

  /**
   * Made public for testing.
   */
  public Component getGlobalFocusOwner()
  {
    return super.getGlobalFocusOwner();
  }

  /**
   * Made public for testing.
   */
  public void setGlobalPermanentFocusOwner(Component c)
  {
    super.setGlobalPermanentFocusOwner(c);
  }

  /**
   * Made public for testing.
   */
  public Component getGlobalPermanentFocusOwner()
  {
    return super.getGlobalPermanentFocusOwner();
  }

  protected void dequeueKeyEvents(long after, Component untilFocused)
  {
    // TODO Auto-generated method stub

  }

  protected void discardKeyEvents(Component comp)
  {
    // TODO Auto-generated method stub

  }

  public boolean dispatchEvent(AWTEvent e)
  {
    // TODO Auto-generated method stub
    return false;
  }

  public boolean dispatchKeyEvent(KeyEvent e)
  {
    // TODO Auto-generated method stub
    return false;
  }

  public void downFocusCycle(Container cont)
  {
    // TODO Auto-generated method stub

  }

  protected void enqueueKeyEvents(long after, Component untilFocused)
  {
    // TODO Auto-generated method stub

  }

  public void focusNextComponent(Component comp)
  {
    // TODO Auto-generated method stub

  }

  public void focusPreviousComponent(Component comp)
  {
    // TODO Auto-generated method stub

  }

  public boolean postProcessKeyEvent(KeyEvent e)
  {
    // TODO Auto-generated method stub
    return false;
  }

  public void processKeyEvent(Component focused, KeyEvent e)
  {
    // TODO Auto-generated method stub

  }

  public void upFocusCycle(Component comp)
  {
    // TODO Auto-generated method stub

  }

}
