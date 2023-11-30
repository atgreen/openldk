/* getRegisteredKeyStrokes.java -- some checks for the getRegisteredKeyStrokes()
       method in the JComponent class.
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

// Tags: JDK1.3

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JComponent;
import javax.swing.KeyStroke;

public class getRegisteredKeyStrokes implements Testlet
{
  static class MyJComponent extends JComponent
  {
    public MyJComponent()
    {
      super();
    }
  }
  public void test(TestHarness harness)
  {
    JComponent c = new MyJComponent();
    harness.check(c.getRegisteredKeyStrokes().length, 0);
    KeyStroke ks0 = KeyStroke.getKeyStroke('a');
    KeyStroke ks1 = KeyStroke.getKeyStroke('b');
    KeyStroke ks2 = KeyStroke.getKeyStroke('c');;
    c.getInputMap(JComponent.WHEN_FOCUSED).put(ks0, "A");
    harness.check(c.getRegisteredKeyStrokes()[0], ks0);
    c.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(ks1, "B");
    harness.check(c.getRegisteredKeyStrokes()[0], ks0);
    harness.check(c.getRegisteredKeyStrokes()[1], ks1);
    c.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(ks2, "C");
    harness.check(c.getRegisteredKeyStrokes()[0], ks0);
    harness.check(c.getRegisteredKeyStrokes()[1], ks1);
    harness.check(c.getRegisteredKeyStrokes()[2], ks2);
  }
}
