// Tags: JDK1.4

// Copyright (C) 2004 David Gilbert (david.gilbert@object-refinery.com)

// This file is part of Mauve.

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

package gnu.testlet.java.awt.AWTKeyStroke;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.AWTKeyStroke;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

/**
 * Some checks for the getAWTKeyStroke() methods.
 */
public class getAWTKeyStroke
  implements Testlet
{
  /**
   * Confirm that two lines with the same end points are NOT considered equal.
   */
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
    testMethod3(harness);
    testMethod4(harness);
    testMethod5(harness);
  }
  
  private void testMethod1(TestHarness harness) 
  {
    harness.checkPoint("(char)");    
    AWTKeyStroke ks = AWTKeyStroke.getAWTKeyStroke('s');
    harness.check(ks.getKeyEventType(), KeyEvent.KEY_TYPED);
    harness.check(ks.getKeyChar(), 's');
    harness.check(ks.getModifiers(), 0);
    harness.check(ks.isOnKeyRelease(), false);
  }

  private void testMethod2(TestHarness harness) 
  {
    harness.checkPoint("(Character, int)");    
    AWTKeyStroke ks = AWTKeyStroke.getAWTKeyStroke(new Character('s'), InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK);
    harness.check(ks.getKeyEventType(), KeyEvent.KEY_TYPED);
    harness.check(ks.getKeyChar(), 's');
    harness.check(ks.getModifiers(), InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK);
    harness.check(ks.isOnKeyRelease(), false);

    // check for IllegalArgumentException for null argument
    try
    {
      ks = AWTKeyStroke.getAWTKeyStroke(null, 0);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
  }

  private void testMethod3(TestHarness harness) 
  {
    harness.checkPoint("(int, int)");    
    AWTKeyStroke ks = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK);
    harness.check(ks.getKeyEventType(), KeyEvent.KEY_PRESSED);
    harness.check(ks.getKeyChar(), KeyEvent.CHAR_UNDEFINED);
    harness.check(ks.getModifiers(), InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK);
    harness.check(ks.isOnKeyRelease(), false);
  }

  private void testMethod4(TestHarness harness) 
  {
    harness.checkPoint("(int, int, boolean)");    
    AWTKeyStroke ks = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK, true);
    harness.check(ks.getKeyEventType(), KeyEvent.KEY_RELEASED);
    harness.check(ks.getKeyChar(), KeyEvent.CHAR_UNDEFINED);
    harness.check(ks.getModifiers(), InputEvent.CTRL_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_MASK | InputEvent.SHIFT_MASK);
    harness.check(ks.isOnKeyRelease(), true);
  }

  private void testMethod5(TestHarness harness) 
  {
    harness.checkPoint("(String)");    
    
    AWTKeyStroke ks = AWTKeyStroke.getAWTKeyStroke("INSERT");
    AWTKeyStroke expected = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_INSERT, 0);
    harness.check(ks, expected);
    
    ks = AWTKeyStroke.getAWTKeyStroke("control DELETE");
    expected = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_DELETE, InputEvent.CTRL_DOWN_MASK);
    harness.check(ks, expected);
    expected = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_DELETE, InputEvent.CTRL_MASK);
    harness.check(ks, expected);

    ks = AWTKeyStroke.getAWTKeyStroke("alt shift X");
    expected = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_X, InputEvent.ALT_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK);
    harness.check(ks, expected);
    expected = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_X, InputEvent.ALT_MASK | InputEvent.SHIFT_MASK);
    harness.check(ks, expected);
  
    ks = AWTKeyStroke.getAWTKeyStroke("alt shift released X");
    expected = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_X, InputEvent.ALT_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK, true);
    harness.check(ks, expected);
    expected = AWTKeyStroke.getAWTKeyStroke(KeyEvent.VK_X, InputEvent.ALT_MASK | InputEvent.SHIFT_MASK, true);
    harness.check(ks, expected);
    
    ks = AWTKeyStroke.getAWTKeyStroke("typed a");
    expected = AWTKeyStroke.getAWTKeyStroke('a');
    harness.check(ks, expected);
    
    // check for IllegalArgumentException for null argument
    harness.checkPoint("null (String) argument");
    try
    {
      ks = AWTKeyStroke.getAWTKeyStroke(null);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    catch (Throwable e)
    {
      harness.check(false);
    }
    
    // check for IllegalArgumentException for bad string
    harness.checkPoint("bad string");
    try
    {
      ks = AWTKeyStroke.getAWTKeyStroke("bad");
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
  }
}
