//Tags: JDK1.4

//Copyright (C) 2006 Free Software Foundation, Inc.
//Written by Wolfgang Baer (WBaer@gmx.de)

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 51 Franklin Street, Fifth Floor,
//Boston, MA, 02110-1301 USA.

package gnu.testlet.java.awt.Window;

import java.awt.Frame;
import java.awt.Window;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests that the behaviour of focusCycleRoot.
 */
public class focusCycleRootTest implements Testlet
{

  public void test(TestHarness harness)
  {
    Frame f = new Frame();
    Window w = new Window(f);
    
    harness.check(w.getFocusCycleRootAncestor() == null);
    harness.check(w.isFocusCycleRoot() == true);
    
    // method must not changes focuscycleroot variable
    w.setFocusCycleRoot(false);
    harness.check(w.isFocusCycleRoot() == true);
  }

}
