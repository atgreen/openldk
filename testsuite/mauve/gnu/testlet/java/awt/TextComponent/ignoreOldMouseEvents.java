/* ignoreOldMouseEvents.java --
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

// Tags: JDK1.4

package gnu.testlet.java.awt.TextComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.BorderLayout;
import java.awt.Event;
import java.awt.Frame;
import java.awt.Point;
import java.awt.Robot;
import java.awt.TextArea;
import java.awt.TextComponent;
import java.awt.TextField;
import java.awt.event.InputEvent;

public class ignoreOldMouseEvents
    extends Frame
    implements Testlet
{

  TextField a;
  TextArea b;
  TestHarness harness;
  boolean mouseUp = false;
  boolean mouseDown = false;
  boolean mouseEnter = false;
  boolean mouseExit = false;
  
  public void test(TestHarness harness)
  {
    this.harness = harness;
    a = new TextField(5);
    b = new TextArea(10, 10);
    add(a, BorderLayout.EAST);
    add(b, BorderLayout.WEST);
    setSize(200,200);
    show();
    
    testOldMouseEvents();
  }
  
  public void testOldMouseEvents()
  {
    Robot r = harness.createRobot();
        
    r.waitForIdle ();
    
    Point aLocScreen = a.getLocationOnScreen();
    Point aMiddle = new Point(aLocScreen.x + a.getWidth()/2, aLocScreen.y + a.getHeight()/2) ;
    
    r.mouseMove(aMiddle.x, aMiddle.y);
    r.delay (1000);
    harness.check(!mouseEnter);
    mouseEnter = false;
    
    r.mousePress(InputEvent.BUTTON1_MASK);
    r.delay (1000);
    harness.check(!mouseDown);
    mouseDown = false;
    
    r.mouseRelease(InputEvent.BUTTON1_MASK);
    r.delay (1000);
    harness.check(!mouseUp);
    mouseUp = false;
    
    Point bLocScreen = b.getLocationOnScreen();
    Point bMiddle = new Point(bLocScreen.x + b.getWidth()/2, bLocScreen.y + b.getHeight()/2) ;
    
    r.mouseMove(bMiddle.x, bMiddle.y);
    r.delay (1000);
    harness.check(!mouseExit);
    harness.check(!mouseEnter);
    mouseEnter = false;
    mouseExit = false;
    
    r.mousePress(InputEvent.BUTTON1_MASK);
    r.delay (1000);
    harness.check(!mouseDown);
    mouseDown = false;
    
    r.mouseRelease(InputEvent.BUTTON1_MASK);
    r.delay (1000);
    harness.check(!mouseUp);
    mouseUp = false;

    r.mouseMove(bMiddle.x*2, bMiddle.y*2);
    r.delay (1000);
    harness.check(!mouseExit);
    mouseExit = false;
  }
  
  public boolean mouseDown(Event evt, int x, int y)
  {
    if (evt.arg instanceof TextComponent)
      mouseDown = true;
    return false;
  }

  public boolean mouseUp(Event evt, int x, int y)
  {
    if (evt.arg instanceof TextComponent)
      mouseUp = true;
    return false;
  }

  public boolean mouseExit(Event evt, int x, int y)
  {
    if (evt.arg instanceof TextComponent)
      mouseExit = true;
    return false;
  }

  public boolean mouseEnter(Event evt, int x, int y)
  {
    if (evt.arg instanceof TextComponent)
      mouseEnter = true;
    return false;
  }
}
