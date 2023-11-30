/* getInputMap.java -- some checks for the getInputMap() methods inherited by
       the JLabel class.
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

package gnu.testlet.javax.swing.JLabel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.KeyStroke;

/**
 * Checks the input maps defined for a JLabel - this is really testing
 * the setup performed by BasicLabelUI.
 */
public class getInputMap implements Testlet
{
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }
    
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("()");
    JLabel label = new JLabel("Test");
    InputMap m1 = label.getInputMap();
    InputMap m2 = label.getInputMap(JComponent.WHEN_FOCUSED);   
    harness.check(m1 == m2);
  }
    
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(int)");
    JLabel label = new JLabel("Test");
    InputMap m1 = label.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1.keys(), null);
    InputMap m1p = m1.getParent();
    harness.check(m1p, null);
    InputMap m2 = label.getInputMap(
            JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);   
    harness.check(m2.keys(), null);
    harness.check(m2.getParent(), null);
    InputMap m3 = label.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m3.keys(), null);
    harness.check(m3.getParent(), null);

    // now make a label that is the label for a component
    JLabel label2 = new JLabel("Test");
    JButton button = new JButton("Target");
    label2.setLabelFor(button);
    label2.setDisplayedMnemonic('A');

    m1 = label2.getInputMap(JComponent.WHEN_FOCUSED);
    harness.check(m1.keys(), null);
    m1p = m1.getParent();
    harness.check(m1p, null);
    m2 = label2.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);   
    harness.check(m2.keys(), null);
    harness.check(m2.getParent(), null);
    m3 = label2.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m3.keys(), null);
    InputMap m3p = m3.getParent();
    harness.check(m3p.get(KeyStroke.getKeyStroke("alt pressed A")), "press");
    label2.setDisplayedMnemonic('B');
    harness.check(m3p.get(KeyStroke.getKeyStroke("alt pressed B")), "press");
  }
}
