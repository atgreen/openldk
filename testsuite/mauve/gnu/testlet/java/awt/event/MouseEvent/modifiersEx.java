// Tags: JDK1.4

// Copyright (C) 2005 Roman Kennke <roman@kennke.org>

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.awt.event.MouseEvent;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.Frame;
import java.awt.Point;
import java.awt.Robot;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.KeyEvent;

/**
 * Checks if a click does trigger the correct getModifiers() flags.
 * This doesn't test the ALT, META or ALT_GRAPH modifiers as their
 * behaviour is window-manager specific.  Some window managers
 * intercept some key-mouse combinations, e.g. ALT-BUTTON1 means
 * "Start dragging this window" in Metacity.  Also note that xmodmap
 * settings will affect Java-generated modifier masks.
 *
 * @author Roman Kennke
 */
public class modifiersEx implements Testlet
{
  int mask;
  Robot robot;
  TestHarness h;
  // set this to true to test ALT, META and ALT_GRAPH modifiers
  boolean test_alt;

  public void checkMask (int keycode[], int buttonmask, int keymask)
  {
    int robot_button = 0;

    if (buttonmask == InputEvent.BUTTON1_DOWN_MASK)
      robot_button = InputEvent.BUTTON1_MASK;
    else if (buttonmask == InputEvent.BUTTON2_DOWN_MASK)
      robot_button = InputEvent.BUTTON2_MASK;
    else if (buttonmask == InputEvent.BUTTON3_DOWN_MASK)
      robot_button = InputEvent.BUTTON3_MASK;

    int i;
    for (i = 0; i < keycode.length; i++)
      robot.keyPress (keycode[i]);

    robot.mousePress(robot_button);
    h.check(mask, buttonmask | keymask, "mousePressed: " + mask);
    mask = 0;
    robot.mouseRelease(robot_button);
    // release event extended modifiers don't include button mask
    h.check(mask, keymask, "mouseReleased: " + mask);
    mask = 0;

    for (i = 0; i < keycode.length; i++)
      robot.keyRelease (keycode[i]);
  }

  public void checkAllMaskCombinations (int buttonmask)
  {
    // each of the 5 key masks can be on or off, giving 32 possible
    // combinations.

    // no modifiers
    checkMask (new int[] {},
	       buttonmask,
	       0);

    // one modifier
    // SHIFT_DOWN_MASK
    checkMask (new int[] { KeyEvent.VK_SHIFT },
	       buttonmask,
	       InputEvent.SHIFT_DOWN_MASK);

    // CTRL_DOWN_MASK
    checkMask (new int[] { KeyEvent.VK_CONTROL },
	       buttonmask,
	       InputEvent.CTRL_DOWN_MASK);

    if (test_alt)
      {
	// META_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_META },
		   buttonmask,
		   InputEvent.META_DOWN_MASK);

	// ALT_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_ALT },
		   buttonmask,
		   InputEvent.ALT_DOWN_MASK);

	// ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.ALT_GRAPH_DOWN_MASK);
      }

    // two modifiers

    // SHIFT_DOWN_MASK | CTRL_DOWN_MASK
    checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL },
	       buttonmask,
	       InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK);

    if (test_alt)
      {
	// SHIFT_DOWN_MASK | META_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_META },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.META_DOWN_MASK);

	// SHIFT_DOWN_MASK | ALT_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_ALT },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.ALT_DOWN_MASK);

	// SHIFT_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// CTRL_DOWN_MASK | META_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_CONTROL, KeyEvent.VK_META },
		   buttonmask,
		   InputEvent.CTRL_DOWN_MASK | InputEvent.META_DOWN_MASK);

	// CTRL_DOWN_MASK | ALT_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_CONTROL, KeyEvent.VK_ALT },
		   buttonmask,
		   InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK);

	// CTRL_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_CONTROL, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// META_DOWN_MASK | ALT_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_META, KeyEvent.VK_ALT },
		   buttonmask,
		   InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK);

	// META_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_META, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.META_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// ALT_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_ALT, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// three modifiers

	// SHIFT_DOWN_MASK | CTRL_DOWN_MASK | META_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL, KeyEvent.VK_META },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.META_DOWN_MASK);

	// SHIFT_DOWN_MASK | CTRL_DOWN_MASK | ALT_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL, KeyEvent.VK_ALT },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK);

	// SHIFT_DOWN_MASK | CTRL_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// SHIFT_DOWN_MASK | META_DOWN_MASK | ALT_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_META, KeyEvent.VK_ALT },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK);

	// SHIFT_DOWN_MASK | META_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// SHIFT_DOWN_MASK | ALT_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_ALT, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// CTRL_DOWN_MASK | META_DOWN_MASK | ALT_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_CONTROL, KeyEvent.VK_META, KeyEvent.VK_ALT },
		   buttonmask,
		   InputEvent.CTRL_DOWN_MASK | InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK);

	// CTRL_DOWN_MASK | META_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_CONTROL, KeyEvent.VK_META, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.CTRL_DOWN_MASK | InputEvent.META_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// CTRL_DOWN_MASK | ALT_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_CONTROL, KeyEvent.VK_ALT, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// META_DOWN_MASK | ALT_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_META, KeyEvent.VK_ALT, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// four modifiers

	// SHIFT_DOWN_MASK | CTRL_DOWN_MASK | META_DOWN_MASK | ALT_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL, KeyEvent.VK_META, KeyEvent.VK_ALT },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK);

	// SHIFT_DOWN_MASK | CTRL_DOWN_MASK | META_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL, KeyEvent.VK_META, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.META_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// SHIFT_DOWN_MASK | CTRL_DOWN_MASK | ALT_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL, KeyEvent.VK_ALT, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// SHIFT_DOWN_MASK | META_DOWN_MASK | ALT_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_META, KeyEvent.VK_ALT, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// CTRL_DOWN_MASK | META_DOWN_MASK | ALT_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_CONTROL, KeyEvent.VK_META, KeyEvent.VK_ALT, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.CTRL_DOWN_MASK | InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);

	// five modifiers

	// SHIFT_DOWN_MASK | CTRL_DOWN_MASK | META_DOWN_MASK | ALT_DOWN_MASK | ALT_GRAPH_DOWN_MASK
	checkMask (new int[] { KeyEvent.VK_SHIFT, KeyEvent.VK_CONTROL, KeyEvent.VK_META, KeyEvent.VK_ALT, KeyEvent.VK_ALT_GRAPH },
		   buttonmask,
		   InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK | InputEvent.META_DOWN_MASK | InputEvent.ALT_DOWN_MASK | InputEvent.ALT_GRAPH_DOWN_MASK);
      }
  }

  public void test(TestHarness h)
  {
    this.h = h;
    Frame frame = new Frame();
    MouseAdapter a = new MouseAdapter()
      {
        public void mousePressed(MouseEvent ev) 
        {
          mask = ev.getModifiersEx();
        }
        public void mouseReleased(MouseEvent ev) 
        {
          mask = ev.getModifiersEx();
        }
      };
    frame.addMouseListener(a);
    frame.setSize(100, 100);
    frame.show();
    Point loc = frame.getLocationOnScreen();
    
    robot = h.createRobot();
    robot.setAutoWaitForIdle (true);

    robot.mouseMove(loc.x + 50, loc.y + 50);

    checkAllMaskCombinations (InputEvent.BUTTON1_DOWN_MASK);
    checkAllMaskCombinations (InputEvent.BUTTON2_DOWN_MASK);
    checkAllMaskCombinations (InputEvent.BUTTON3_DOWN_MASK);
  }
}
