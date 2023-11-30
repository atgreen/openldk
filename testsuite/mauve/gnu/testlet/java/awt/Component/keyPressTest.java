/* keyPressTest.java -- 
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

package gnu.testlet.java.awt.Component;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Event;
import java.awt.Frame;
import java.awt.Robot;
import java.awt.event.KeyEvent;

public class keyPressTest implements Testlet
{
  //arbitrary lock for use in synchronizing test and awt threads
  final Object lock = new Object();

  volatile Integer key = null;

  Robot r;
  myFrame f;
  TestHarness h;

  public void test (TestHarness h)
  {
    f = new myFrame();
    r = h.createRobot ();
    this.h = h;

    f.setSize(200,200); // assumes that the window is positioned at 0,0.
    f.show();

    waitForWindow();

    runTest(KeyEvent.VK_A, 'a');
    runTest(KeyEvent.VK_B, 'b');
    runTest(KeyEvent.VK_C, 'c');
    runTest(KeyEvent.VK_D, 'd');
    runTest(KeyEvent.VK_E, 'e');
    runTest(KeyEvent.VK_F, 'f');
    runTest(KeyEvent.VK_G, 'g');
    runTest(KeyEvent.VK_H, 'h');
    runTest(KeyEvent.VK_I, 'i');
    runTest(KeyEvent.VK_J, 'j');
    runTest(KeyEvent.VK_K, 'k');
    runTest(KeyEvent.VK_L, 'l');
    runTest(KeyEvent.VK_M, 'm');
    runTest(KeyEvent.VK_N, 'n');
    runTest(KeyEvent.VK_O, 'o');
    runTest(KeyEvent.VK_P, 'p');
    runTest(KeyEvent.VK_Q, 'q');
    runTest(KeyEvent.VK_R, 'r');
    runTest(KeyEvent.VK_S, 's');
    runTest(KeyEvent.VK_T, 't');
    runTest(KeyEvent.VK_U, 'u');
    runTest(KeyEvent.VK_V, 'v');
    runTest(KeyEvent.VK_W, 'w');
    runTest(KeyEvent.VK_X, 'x');
    runTest(KeyEvent.VK_Y, 'y');
    runTest(KeyEvent.VK_Z, 'z');

    f.dispose();
  }

  public void runTest(int code, char chr)
  {
    int k; // assigned in the synchronized block
    r.mouseMove(60, 60);

    synchronized(lock) {
      key = null; // reset the key

      // queue the events
      r.keyPress(code);
      r.keyRelease(code); // don't press they key forever
  
      try {

        // release the lock so that the frame can handle the keypress event
        // once it has handled the event, it will notify on the lock.
        // at this point the key should be non-null. We test the result
        // and return
        lock.wait();
      } catch (InterruptedException e) {
        // ignore, we want to get started again
      }

      k = key.intValue();
    }

    h.check(k, chr);
  }

  class myFrame extends Frame {

    public boolean keyDown(Event e, int i) {
      synchronized(lock) {
        key = new Integer(e.key);
        lock.notifyAll();
      }
      return super.keyDown(e, i);
    }
  }

  /**
   * Blocks until the frame has able to respond to keypress events
   */
  private void waitForWindow() {

    // wait until the window starts process events
    synchronized(lock) {
      while (key == null) {
        r.keyPress(KeyEvent.VK_EQUALS); // send a key press event
        r.keyRelease(KeyEvent.VK_EQUALS); // don't press the key forever
        try {

          // wait for a notify from the frame, or timeout in case the
          // window missed the key press event entirely
          lock.wait(100);
        }
        catch (InterruptedException ie) {
          // interrupted, if key is still null, we'll try again
        }
      }

      // send one more "magic" key press as a marker that we've processed
      // all of our probing key strokes
      r.keyPress(KeyEvent.VK_SEMICOLON); // send a key press event
      r.keyRelease(KeyEvent.VK_SEMICOLON); // don't press the key forever
    }

    // Eat up any straggler key strokes, wait for the final magic key press
    while(key != KeyEvent.VK_SEMICOLON) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException e) {
        // shouldn't happen
      }
    }
  }
}
