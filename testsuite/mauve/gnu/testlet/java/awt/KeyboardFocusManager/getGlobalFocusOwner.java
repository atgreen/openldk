/* getFocusOwner.java -- Tests getGlobalFocusOwner in KeyboardFocusManager
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

// Tags: JDK1.4
// Uses: TestKeyboardFocusManager

package gnu.testlet.java.awt.KeyboardFocusManager;

import java.awt.Component;
import java.awt.KeyboardFocusManager;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getGlobalFocusOwner implements Testlet
{

  public void test(TestHarness harness)
  {
    test01(harness);

  }

  /**
   * Tests if and how getFocusOwner() depends on getPermanentFocusOwner().
   *
   * @param h the test harness
   */
  private void test01(TestHarness h)
  {
    Component c1 = new Component(){};
    Component c2 = new Component(){};

    TestKeyboardFocusManager kfm = new TestKeyboardFocusManager();
    // Make this the current KFM to avoid SecurityExceptions.
    KeyboardFocusManager.setCurrentKeyboardFocusManager(kfm);
    
    kfm.setGlobalFocusOwner(null);
    kfm.setGlobalPermanentFocusOwner(null);
    h.check(kfm.getGlobalFocusOwner(), null);

    kfm.setGlobalFocusOwner(c1);
    kfm.setGlobalPermanentFocusOwner(null);
    h.check(kfm.getGlobalFocusOwner(), c1);

    kfm.setGlobalFocusOwner(null);
    kfm.setGlobalPermanentFocusOwner(c2);
    h.check(kfm.getGlobalFocusOwner(), null);

    kfm.setGlobalFocusOwner(c1);
    kfm.setGlobalPermanentFocusOwner(c2);
    h.check(kfm.getGlobalFocusOwner(), c1);
  }
}
