/* doLayout.java 
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

package gnu.testlet.java.awt.ScrollPane;

import java.awt.Button;
import java.awt.Component;
import java.awt.Point;
import java.awt.ScrollPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class doLayout implements Testlet
{

  public void test(TestHarness harness)
  {
    test1(harness);    
  }
  
  public void test1(TestHarness harness)
  {
    // This test checks that the location of scrollpane's
    // child is "reset" to (0, 0).
    ScrollPane pane = new ScrollPane();
    Component button = new Button();
    button.setLocation(100, 250);
    pane.add(button);
    harness.check(button.getLocation().getX(), 100);
    harness.check(button.getLocation().getY(), 250);
    pane.doLayout();
    harness.check(button.getLocation().getX(), 0);
    harness.check(button.getLocation().getY(), 0);
  }

}
