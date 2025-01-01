/* getElements.java -- some checks for the getElements() method in the 
       ButtonGroup class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.javax.swing.ButtonGroup;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Enumeration;

import javax.swing.ButtonGroup;
import javax.swing.JToggleButton;

public class getElements implements Testlet
{
  public void test(TestHarness harness)
  {
    ButtonGroup g = new ButtonGroup();
    Enumeration e = g.getElements();
    harness.check(e.hasMoreElements(), false);
    
    JToggleButton b1 = new JToggleButton("B1");
    g.add(b1);
    e = g.getElements();
    harness.check(e.nextElement(), b1);
    harness.check(e.hasMoreElements(), false);

    JToggleButton b2 = new JToggleButton("B2");
    g.add(b2);
    e = g.getElements();
    harness.check(e.nextElement(), b1);
    harness.check(e.nextElement(), b2);
    harness.check(e.hasMoreElements(), false);
  }
}
